#######################################################################
# Zooplankton CPUE modeling pipeline
#
# Fits hurdle-gamma (or other) brms models of CPUE ~ Type * Region
# across taxonomic groups, compares nested model structures via LOO,
# and exports fitted objects for downstream use.
#
# Structure:
#   1. Packages & global settings
#   2. Helper functions (data prep, formula builder, priors, fit/score)
#   3. Load & prepare data
#   4. Define candidate model structures
#   5. Run pipeline
#   6. Export results
#
# Notes carried over from prior version (13 Aug 2026 synthesis discussion):
#   - Taxonomic resolution: predatory copepods / non-predatory copepods /
#     Limnoithona / cladocerans
#   - Region is modeled as affecting both the mean and the hurdle process
#   - Analysis restricted to oblique/surface tow types (bottom excluded
#     for now -- see `tow_types_include` below to change this)
#######################################################################

library(tidyverse)
library(purrr)
library(brms)
library(tidybayes)
library(bayesplot)
library(bayestestR)
library(ggeffects)
library(httr)
library(glue)
library(loo)
library(statmod)

#######################################################################
## 1. GLOBAL SETTINGS
##    Centralizing these here means you rarely need to touch the
##    functions below -- just change these values.
#######################################################################

CONFIG <- list(
  # data filtering (used inside `preprocess()`)
  season           = "Spring",
  size_class       = "Meso",
  tow_types        = c("Oblique", "Surface"),   # add "Bottom" here if needed
  exclude_regions  = c("Web Tract Berms", "Decker"),
  
  # MCMC settings (passed through to every brm() call)
  mcmc = list(
    warmup = 1000, iter = 3000, chains = 3, cores = 3,
    thin = 10, adapt_delta = 0.95, seed = 123
  ),
  
  # LOO strategy: "moment_match" (fast, default) | "reloo" (slow, precise) | "plain"
  loo_strategy = "moment_match",
  loo_weight_method="stacking",
  
  # where fitted models / summaries get written
  out_dir = "model_output/aug2026"
)

# Families available for swapping in at the pipeline call, e.g.
#   run_pipeline(datasets, model_formulas_hu, family = famhu)
famhu <- hurdle_gamma(link = "log")
famtw <- tweedie(link = "log")     # alternative if hurdle-gamma misbehaves

#######################################################################
## 2. HELPER FUNCTIONS
#######################################################################

# ---- 2a. Data prep -----------------------------------------------
#' Add season/day-of-year/tow-ID columns and drop excluded regions.
#' Applied once per raw taxon dataset right after loading.
prep_taxon_data <- function(df,
                            exclude_regions = CONFIG$exclude_regions,
                            exclude_sources = NULL) {
  out <- df %>%
    mutate(
      Month = month(Date),
      sea = case_when(
        Month %in% c(6, 7, 8, 9)  ~ "Summer",
        Month %in% c(10, 11)      ~ "Fall",
        Month %in% c(12, 1, 2)    ~ "Winter",
        TRUE                      ~ "Spring"
      ),
      yday = yday(Date),
      SourceTow = paste(Source, TowType, sep = "_")
    ) %>%
    filter(!Region %in% exclude_regions)
  
  if (!is.null(exclude_sources)) {
    out <- filter(out, !Source %in% exclude_sources)
  }
  out
}

#' Final filtering step applied just before modeling.
#' Kept separate from `prep_taxon_data()` so datasets can be
#' re-filtered differently (e.g. different season) without reloading.
preprocess <- function(df,
                       season      = CONFIG$season,
                       size_class  = CONFIG$size_class,
                       tow_types   = CONFIG$tow_types,
                       exclude_regions = CONFIG$exclude_regions) {
  df %>%
    filter(
      sea %in% season,
      !Region %in% exclude_regions,
      SizeClass == size_class,
      TowType %in% tow_types
    ) %>%
    droplevels()
}

# ---- 2b. Formula builder ------------------------------------------
#' Build a single brms bf() formula from a fixed-effects RHS string.
#' Random-effects structure and hurdle term are standardized here so
#' the candidate model list below is just a set of short strings,
#' rather than repeated, error-prone bf() blocks.
#'
#' @param fixed_rhs  RHS of the mean-model formula, e.g. "Type * Region"
#'                    or "" for an intercept/random-effects-only null model
#' @param hu_rhs     RHS of the hurdle formula (NULL = no hurdle component,
#'                    i.e. a non-hurdle family like tweedie/negbinomial)
#' @param random_effects  RE structure appended to the fixed effects
make_formula <- function(fixed_rhs,
                         hu_rhs = NULL,
                         random_effects = "(1 | Project_na) + (1 | SourceTow) + (1 | Year)") {
  mean_rhs <- if (nzchar(fixed_rhs)) paste(fixed_rhs, "+", random_effects) else random_effects
  mean_form <- as.formula(paste("CPUE ~", mean_rhs))
  
  if (is.null(hu_rhs)) {
    bf(mean_form)
  } else {
    bf(mean_form, as.formula(paste("hu ~", hu_rhs)))
  }
}

# ---- 2c. Priors -----------------------------------------------------
#' Auto-build a weakly-informative prior set for a given formula/data/family,
#' only including terms that `brms::get_prior()` says are actually valid
#' for that formula (avoids "unused prior" errors when formulas differ
#' in which terms/random effects they contain).
build_priors <- function(form, data, family,
                         mean_b       = "normal(0, 2)",
                         mean_intercept = "student_t(3, 0, 2)",
                         sd           = "student_t(3, 0, 2.5)",
                         shape        = "normal(0, 1)",
                         include_shape = FALSE,
                         hu_intercept = "normal(0, 1.5)",
                         hu_b         = "normal(0, 3)") {
  
  allowed <- brms::get_prior(formula = form, data = data, family = family)
  pri <- list()
  
  add_if <- function(cond, spec) if (cond) pri[[length(pri) + 1]] <<- spec
  
  add_if(any(allowed$class == "b" & is.na(allowed$dpar)),
         set_prior(mean_b, class = "b"))
  add_if(any(allowed$class == "Intercept" & is.na(allowed$dpar)),
         set_prior(mean_intercept, class = "Intercept"))
  add_if(any(allowed$class == "sd" & is.na(allowed$dpar)),
         set_prior(sd, class = "sd"))
  add_if(include_shape && any(allowed$class == "shape"),
         set_prior(shape, class = "shape"))
  add_if(any(allowed$class == "Intercept" & allowed$dpar == "hu"),
         set_prior(hu_intercept, class = "Intercept", dpar = "hu"))
  add_if(any(allowed$class == "b" & allowed$dpar == "hu"),
         set_prior(hu_b, class = "b", dpar = "hu"))
  
  if (length(pri) == 0) brms::empty_prior() else Reduce(`+`, pri, accumulate = FALSE) |> (\(x) do.call(c, pri))()
}

# Build priors for every formula in a named list at once.
build_priors_by_model <- function(formulas, data, family, ...) {
  imap(formulas, ~ build_priors(.x, data = data, family = family, ...))
}

# ---- 2d. Fit a single model ------------------------------------------
#' Thin, validated wrapper around brm(). All tuning knobs default to
#' CONFIG$mcmc so you normally don't need to pass them explicitly.
fit_one <- function(data, form, family, prior = NULL,
                    warmup = CONFIG$mcmc$warmup, iter = CONFIG$mcmc$iter,
                    chains = CONFIG$mcmc$chains, cores = CONFIG$mcmc$cores,
                    thin = CONFIG$mcmc$thin, adapt_delta = CONFIG$mcmc$adapt_delta,
                    seed = CONFIG$mcmc$seed) {
  
  is_valid_family <- function(fam) {
    inherits(fam, c("brmsfamily", "family", "customfamily")) || is.character(fam)
  }
  if (!is_valid_family(family)) {
    stop("`family` must be a brms/base-R family object, custom_family(), or a string.",
         call. = FALSE)
  }
  
  brm(
    formula   = form,
    data      = data,
    family    = family,
    prior     = prior,
    warmup    = as.integer(warmup), iter = as.integer(iter),
    chains    = as.integer(chains), cores = as.integer(cores),
    thin      = as.integer(thin),
    control   = list(adapt_delta = adapt_delta),
    save_pars = save_pars(all = TRUE),
    seed      = seed
  )
}


# ---- 2e. Model weights ------------------------------------------------
#' Attach model weights (stacking or pseudo-BMA) to a `loo_compare()` table.
#' Weights sum to 1 across candidate models and are often a more intuitive
#' summary than raw elpd differences for communicating "how much better"
#' one model is than another.
#' Method options (see `?loo::loo_model_weights`): "stacking" (default,
#' recommended) or "pseudobma" (with/without Bayesian bootstrap).
add_model_weights <- function(loos, comparison, method = CONFIG$loo_weight_method) {
  weights <- tryCatch(
    loo::loo_model_weights(loos, method = method),
    error = function(e) {
      warning("Could not compute model weights: ", e$message)
      NULL
    }
  )
  
  cmp_df <- as.data.frame(comparison)
  cmp_df$model <- rownames(cmp_df)
  
  if (is.null(weights)) {
    cmp_df$weight <- NA_real_
  } else {
    w_df <- data.frame(model = names(weights), weight = as.numeric(weights))
    cmp_df <- merge(cmp_df, w_df, by = "model", sort = FALSE)
  }
  
  # preserve loo_compare()'s elpd-based ranking (best model first)
  cmp_df <- cmp_df[match(rownames(comparison), cmp_df$model), ]
  rownames(cmp_df) <- NULL
  cmp_df
}


# ---- 2fe. Fit + LOO-compare a set of candidate formulas on one dataset ---
#' @param dat        already-preprocessed data (see `preprocess()`)
#' @param formulas   named list of bf() formulas to compare
#' @param priors_by_model  optional named list matching `formulas`, from
#'                          `build_priors_by_model()`; NULL uses brms defaults
fit_and_score <- function(df_name, dat, formulas, family,
                          priors_by_model = NULL,
                          loo_strategy = CONFIG$loo_strategy,
                          ...) {
  
  fits <- imap(formulas, function(form, form_name) {
    message(glue("Fitting {df_name} - {form_name}"))
    fit <- fit_one(
      data = dat, form = form, family = family,
      prior = priors_by_model[[form_name]], ...
    )
    fit <- switch(loo_strategy,
                  reloo         = add_criterion(fit, "loo", reloo = TRUE),
                  moment_match  = add_criterion(fit, "loo", moment_match = TRUE),
                  add_criterion(fit, "loo")
    )
    fit
  })
  
  loos <- map(fits, ~ .x$criteria$loo)
  cmp <- loo_compare(loos)
  list(fits = fits, loos = loos, comparison = add_model_weights(loos, cmp))
  
}

# ---- 2f. Run the full pipeline across datasets ------------------------
#' Iterate `fit_and_score()` over every dataset in `datasets`, auto-building
#' priors per dataset/formula and recording timing.
run_pipeline <- function(datasets, formulas, family,
                         auto_priors = TRUE, prior_args = list(),
                         ...) {
  imap(datasets, function(df, nm) {
    t0 <- Sys.time()
    message(glue("=== Starting {nm} at {t0} ==="))
    
    dat <- preprocess(df)
    priors_this <- if (auto_priors) {
      do.call(build_priors_by_model, c(list(formulas = formulas, data = dat, family = family), prior_args))
    } else NULL
    
    res <- fit_and_score(nm, dat, formulas, family, priors_by_model = priors_this, ...)
    
    t1 <- Sys.time()
    res$timing <- list(start = t0, end = t1,
                       duration_minutes = as.numeric(difftime(t1, t0, units = "mins")))
    message(glue("=== Finished {nm} (Duration: {round(res$timing$duration_minutes, 2)} min) ==="))
    res
  })
}

# ---- 2g. Export ---------------------------------------------------------
#' Save fitted models, LOO comparison tables, and timing info to disk.
save_results <- function(results, out_dir = CONFIG$out_dir, suffix = "") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  iwalk(results, function(res, ds) {
    iwalk(res$fits, function(fit, mname) {
      f <- file.path(out_dir, glue("{ds}_{mname}{suffix}_brmsfit.rds"))
      saveRDS(fit, file = f, compress = "xz")
      message("Saved model: ", f)
    })
    saveRDS(res$comparison, file.path(out_dir, glue("{ds}{suffix}_loo_comparison.rds")))
    if (!is.null(res$timing)) {
      saveRDS(res$timing, file.path(out_dir, glue("{ds}{suffix}_timing.rds")))
    }
  })
  invisible(TRUE)
}

# ---- 2h. Optional: compare fixed-effect coefficients across models -----
#' Pulls all b_ coefficient draws from a fit into long format, tagged
#' with a model name -- handy for combining across a `results` list
#' with `map2_dfr()` / `bind_rows()` when comparing structures.
get_coef_draws <- function(fit, model_name) {
  posterior::as_draws_df(fit) %>%
    dplyr::select(dplyr::starts_with("b_")) %>%
    tidyr::pivot_longer(everything(), names_to = "term", values_to = "draw") %>%
    dplyr::mutate(model = model_name, term = sub("^b_", "", term))
}

#######################################################################
## 3. LOAD & PREPARE DATA
#######################################################################

# --- (optional) re-download raw data from GitHub -----------------------
# Uncomment to refresh local .RData files.
# get_taxon <- function(name) {
#   url <- glue("https://api.github.com/repos/InteragencyEcologicalProgram/",
#               "FoodwebModel/contents/data/{name}.RData")
#   GET(url, add_headers(Accept = "application/vnd.github.v3.raw"),
#       write_disk(glue("{name}.RData"), overwrite = TRUE))
# }
# walk(c("Calanoids", "Cladocera", "Cyclopoids"), get_taxon)

#load("Calanoids.RData")
load("Cladocera.RData")
#load("Cyclopoids.RData")
load("Limnoithona.RData")
load("PredatoryCopepods.RData")
load("NonPredatoryCopepods.RData")

#cal  <- prep_taxon_data(Calanoids)
clad <- prep_taxon_data(Cladocera)
#cyc  <- prep_taxon_data(Cyclopoids)
gcop <- prep_taxon_data(goodcop)
bcop <- prep_taxon_data(badcop)
wcop <- prep_taxon_data(weirdcop, exclude_sources = c("YBFMP", "USGS"))

# Quick sanity checks
# unique(cal$Source); unique(cal$Region); unique(cal$Year)

# --- Which dataset(s) to model in this run ------------------------------
# Comment/uncomment entries to control what `run_pipeline()` fits below.
datasets <- list(
  # cal  = cal,
  #clad = clad,
  # cyc  = cyc,
  #gcop = gcop,
  #bcop = bcop
  wcop = wcop
)

#######################################################################
## 4. CANDIDATE MODEL STRUCTURES
##    Each entry is just a fixed-effects string; make_formula() handles
##    the (identical) random-effects structure and hurdle term for you.
##    Add/remove rows here to change what gets compared via LOO.
#######################################################################

hu_fixed_effects <- list(
  base          = "Type * Region",
  base_nohu     = "Type * Region",   # same mean model, hurdle held constant below
  main_effects  = "Type + Region",
  type_only     = "Type",
  reg_only      = "Region",
  nullmod       = ""
)
hu_hurdle_terms <- list(
  base = "SourceTow", base_nohu = "1", main_effects = "SourceTow",
  type_only = "SourceTow", reg_only = "SourceTow", nullmod = "SourceTow"
)

model_formulas_hu <- imap(hu_fixed_effects, ~ make_formula(.x, hu_hurdle_terms[[.y]]))

#nb_fixed_effects <- list(
#  base         = "Type * Region",
#  main_effects = "Type + Region",
#  no_type      = "Type",
#  nullmod      = ""
#)
#model_formulas_nb <- map(nb_fixed_effects, ~ make_formula(.x, hu_rhs = NULL))

#######################################################################
## 5. RUN PIPELINE
#######################################################################

results_hu <- run_pipeline(
  datasets = datasets,
  formulas = model_formulas_hu,
  family   = famhu
)

# To compare a non-hurdle family instead, e.g.:
# results_nb <- run_pipeline(datasets, model_formulas_nb, family = "negbinomial")

# --- Optional: fit + inspect a single model interactively ---------------
# Useful for quick sanity checks before committing to a full pipeline run.
# dat_wcop <- preprocess(wcop)
# pri_wcop <- build_priors(model_formulas_hu$base, dat_wcop, famhu)
# m_wcop <- fit_one(dat_wcop, model_formulas_hu$base, famhu, prior = pri_wcop)
# summary(m_wcop)
# plot(conditional_effects(m_wcop), theme = theme_bw())

#######################################################################
## 6. EXPORT RESULTS
#######################################################################

save_results(results_hu, suffix = "_hu")

# View LOO comparison tables:
# map(results_hu, "comparison")



#######################################################################
## 7. LOAD RESULTS FROM PREVIOUS RUNS
##    Reconstructs the same list structure produced by `run_pipeline()`
##    (fits / loos / comparison / timing) directly from saved .rds files,
##    so you can re-examine or extend past runs without re-fitting.
#######################################################################

#' @param dataset_names  names to reload, e.g. names(datasets); if NULL,
#'                        tries to infer from a `datasets` list in the
#'                        calling environment, then falls back to treating
#'                        everything before the first "_" as the dataset name
#'                        (only reliable if model names contain no "_")
#' @param suffix          must match the suffix used when saving
#'                        (e.g. "_hu" from save_results(..., suffix = "_hu"))
load_results <- function(dataset_names = NULL, out_dir = CONFIG$out_dir, suffix = "") {
  
  if (!dir.exists(out_dir)) stop("No such directory: ", out_dir)
  
  fit_files <- list.files(out_dir, pattern = glue("{suffix}_brmsfit\\.rds$"), full.names = TRUE)
  if (length(fit_files) == 0) stop("No saved fits found in ", out_dir, " matching suffix '", suffix, "'")
  
  # Filenames are "{dataset}_{model}{suffix}_brmsfit.rds". Since model names
  # can themselves contain underscores (e.g. "main_effects", "type_only"),
  # we can't just strip the last "_token" -- we match against KNOWN dataset
  # names instead, taking the longest matching prefix.
  candidates <- dataset_names
  if (is.null(candidates) && exists("datasets", inherits = TRUE)) {
    candidates <- names(get("datasets", inherits = TRUE))
  }
  
  base <- sub(glue("{suffix}_brmsfit\\.rds$"), "", basename(fit_files))
  
  match_dataset <- function(b) {
    if (!is.null(candidates)) {
      hits <- candidates[startsWith(b, paste0(candidates, "_"))]
      if (length(hits) > 0) return(hits[which.max(nchar(hits))])  # longest prefix wins
    }
    sub("_.*$", "", b)  # fallback: everything before the first "_"
  }
  
  ds_names_all <- vapply(base, match_dataset, character(1), USE.NAMES = FALSE)
  model_names_all <- mapply(function(b, ds) sub(paste0("^", ds, "_"), "", b),
                            base, ds_names_all, USE.NAMES = FALSE)
  
  if (!is.null(dataset_names)) {
    keep <- ds_names_all %in% dataset_names
    fit_files       <- fit_files[keep]
    ds_names_all    <- ds_names_all[keep]
    model_names_all <- model_names_all[keep]
  }
  
  results <- map(unique(ds_names_all), function(ds) {
    idx <- which(ds_names_all == ds)
    fits <- set_names(map(fit_files[idx], readRDS), model_names_all[idx])
    loos <- map(fits, ~ .x$criteria$loo)
    
    cmp_file <- file.path(out_dir, glue("{ds}{suffix}_loo_comparison.rds"))
    timing_file <- file.path(out_dir, glue("{ds}{suffix}_timing.rds"))
    
    list(
      fits       = fits,
      loos       = loos,
      comparison = if (file.exists(cmp_file)) readRDS(cmp_file) else add_model_weights(loos, loo_compare(loos)),
      timing     = if (file.exists(timing_file)) readRDS(timing_file) else NULL
    )
  }) %>% set_names(unique(ds_names_all))
  
  results
}
# --- Example usage --------------------------------------------------
# results_hu_reloaded <- load_results(dataset_names = "wcop", suffix = "_hu")
# summary(results_hu_reloaded$wcop$fits$base)
# results_hu_reloaded$wcop$comparison

results_hu_bcop <- load_results(dataset_names = "bcop", suffix = "_hu")
results_hu_gcop <- load_results(dataset_names = "gcop", suffix = "_hu")
results_hu_clad <- load_results(dataset_names = "clad", suffix = "_hu")
# summary(results_hu_reloaded$wcop$fits$base)
# results_hu_reloaded$wcop$comparison

results_hu_wcop <- load_results(dataset_names = "wcop", suffix = "_hu_limno")
results_hu_wcop$wcop[c("loos","comparison")] <- 
  build_comparison_from_fits(results_hu_wcop$wcop$fits)[c("loos","comparison")]

clad_comp_test <- build_comparison_from_fits(
  results_hu_clad$clad$fits,
  out_dir = "model_output/aug2026",
  label = "clad_hu"
)

#### Summarise results ####
bcop_comp <- data.frame(readRDS("model_output/aug2026/bcop_hu_loo_comparison2.rds"))
gcop_comp <- data.frame(readRDS("model_output/aug2026/gcop_hu_loo_comparison2.rds"))
clad_comp <- data.frame(readRDS("model_output/aug2026/clad_hu_loo_comparison2.rds"))
wcop_comp <- data.frame(readRDS("model_output/aug2026/limno_hu_loo_comparison.rds"))

results_hu_wcop$wcop$comparison

bcop_comp%>%
  arrange(desc(weight))
gcop_comp%>%
  arrange(desc(weight))
clad_comp%>%
  arrange(desc(weight))

bind_rows(bcop_comp%>%
            select(model,weight)%>%
            mutate(group="bcop"),
          gcop_comp%>%
            select(model,weight)%>%
            mutate(group="gcop"),
          clad_comp%>%
            select(model,weight)%>%
            mutate(group="clad"),
          wcop_comp%>%
            select(model,weight)%>%
            mutate(group="wcop"))%>%
  pivot_wider(names_from=group,values_from=weight)


######################## legacy fix for old models that didn't have weights ########

# --- Retrofitting weights onto comparison tables saved by OLD code ------
# Old `save_results()` output saved a plain `loo_compare()` matrix, with
# no `weight` column. You don't need to re-fit anything to add weights --
# `loos` can be recomputed directly from the saved `.rds` model fits
# (each brmsfit already has its LOO criterion attached from when it was
# fit), then re-run through `add_model_weights()`:
#
# retrofit_weights <- function(results) {
#   imap(results, function(res, ds) {
#     res$comparison <- add_model_weights(res$loos, loo_compare(res$loos))
#     res
#   })
# }
#
#results_hu_bcop <- load_results(dataset_names = "bcop", suffix = "_hu")
#results_hu_gcop <- load_results(dataset_names = "gcop", suffix = "_hu")
#results_hu_clad <- load_results(dataset_names = "clad", suffix = "_hu")
#results_hu_bcop$bcop$fits$base


#old_results <- load_results(dataset_names = "clad", suffix = "_hu")   # comparison tables lack weights
#old_results <- retrofit_weights(old_results)  # now include a `weight` column
#old_results$clad$comparison
#
# To persist the fix back to disk (overwriting only the comparison files):
#iwalk(old_results, ~ saveRDS(.x$comparison,
#                               file.path(CONFIG$out_dir, glue("{.y}_hu_loo_comparison2.rds"))))



#######################################################################
## USAGE
#######################################################################

# Run the full diagnostics + export pipeline on results from the
# modeling script (`run_pipeline()` output or `load_results()`):

out_clad_hu <- export_diagnostics(results_hu_clad, out_dir = "model_output/aug2026/diagnostics",
                                  group_vars = c("Region", "Type", "Year"))
out_gcop_hu <- export_diagnostics(results_hu_gcop, out_dir = "model_output/aug2026/diagnostics",
                                  group_vars = c("Region", "Type", "Year"))
out_bcop_hu <- export_diagnostics(results_hu_bcop, out_dir = "model_output/aug2026/diagnostics",
                                  group_vars = c("Region", "Type", "Year"))
out_wcop_hu <- export_diagnostics(results_hu_wcop, out_dir = "model_output/aug2026/diagnostics",
                                  group_vars = c("Region", "Type", "Year"))

out_clad_hu$combined_table %>% arrange(dataset, looic)

# Re-read the combined table later without re-running anything:
#cladcombined <- read_csv(file.path("model_output/aug2026/diagnostics", "model_selection_all_datasets.csv"))
#combined %>% arrange(dataset, looic) %>% group_by(dataset) %>% slice_head(n = 5) %>% ungroup()

# Spot-check the best model for one dataset interactively:
inspect_model(results_hu_clad$clad$fits[[best_model_name(results_hu_clad$clad)]])
inspect_model2(results_hu_clad$clad$fits[[best_model_name(results_hu_clad$clad)]])
inspect_model2(results_hu_gcop$gcop$fits[[best_model_name(results_hu_gcop$gcop)]])
inspect_model2(results_hu_bcop$bcop$fits[[best_model_name(results_hu_bcop$bcop)]])
inspect_model2(results_hu_wcop$wcop$fits[[best_model_name(results_hu_wcop$wcop)]])
