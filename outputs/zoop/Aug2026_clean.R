#### Modeling of zooplankton datasets - August 2026 ####
# all modeling code in ZoopModelCode_Clean.R
# diagnostic code functions in ModelDiag_clean.R

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

#### Recreate coefficient figures so that coefficients are color coded ####

idf_clad <- bayesplot::mcmc_intervals_data(
  as.array(results_hu_clad$clad$fits$base),
  pars = vars(starts_with(c("b","r","sd","Intercept"))),
  #regex_pars = character("beta]"),
  prob = 0.5,
  prob_outer = 0.95,
  point_est = "mean")%>%
  mutate(overlaps_zero_outer = ifelse(l <= 0 & h >= 0,"n","y"))%>%
  mutate(taxon="clad",
         category=case_when(
    str_starts(parameter, "r_")   ~ "Random",
    str_starts(parameter, "sd")   ~ "Sd",
    str_starts(parameter, "b_hu")   ~ "Hurdle",
    str_starts(parameter, "Intercept")   ~ "Intercept",
    TRUE ~ "Beta"
  ))

idf_all <- bind_rows(bayesplot::mcmc_intervals_data(
  as.array(results_hu_clad$clad$fits$base),
  pars = vars(starts_with(c("b","r","sd","Intercept"))),
  #regex_pars = character("beta]"),
  prob = 0.5,
  prob_outer = 0.95,
  point_est = "mean")%>%
    mutate(overlaps_zero_outer = ifelse(l <= 0 & h >= 0,"n","y"))%>%
    mutate(taxon="clad",
           category=case_when(
             str_starts(parameter, "r_")   ~ "Random",
             str_starts(parameter, "sd")   ~ "Sd",
             str_starts(parameter, "b_hu")   ~ "Hurdle",
             str_starts(parameter, "Intercept")   ~ "Intercept",
             TRUE ~ "Beta"
           )),
  bayesplot::mcmc_intervals_data(
    as.array(results_hu_bcop$bcop$fits$reg_only),
    pars = vars(starts_with(c("b","r","sd","Intercept"))),
    #regex_pars = character("beta]"),
    prob = 0.5,
    prob_outer = 0.95,
    point_est = "mean")%>%
    mutate(overlaps_zero_outer = ifelse(l <= 0 & h >= 0,"n","y"))%>%
    mutate(taxon="predcop",
           category=case_when(
             str_starts(parameter, "r_")   ~ "Random",
             str_starts(parameter, "sd")   ~ "Sd",
             str_starts(parameter, "b_hu")   ~ "Hurdle",
             str_starts(parameter, "Intercept")   ~ "Intercept",
             TRUE ~ "Beta"
           )),
  bayesplot::mcmc_intervals_data(
    as.array(results_hu_gcop$gcop$fits$base),
    pars = vars(starts_with(c("b","r","sd","Intercept"))),
    #regex_pars = character("beta]"),
    prob = 0.5,
    prob_outer = 0.95,
    point_est = "mean")%>%
    mutate(overlaps_zero_outer = ifelse(l <= 0 & h >= 0,"n","y"))%>%
    mutate(taxon="gcop",
           category=case_when(
             str_starts(parameter, "r_")   ~ "Random",
             str_starts(parameter, "sd")   ~ "Sd",
             str_starts(parameter, "b_hu")   ~ "Hurdle",
             str_starts(parameter, "Intercept")   ~ "Intercept",
             TRUE ~ "Beta"
           )),
  bayesplot::mcmc_intervals_data(
             as.array(results_hu_wcop$wcop$fits$base),
             pars = vars(starts_with(c("b","r","sd","Intercept"))),
             #regex_pars = character("beta]"),
             prob = 0.5,
             prob_outer = 0.95,
             point_est = "mean")%>%
    mutate(overlaps_zero_outer = ifelse(l <= 0 & h >= 0,"n","y"))%>%
    mutate(taxon="limno",
           category=case_when(
             str_starts(parameter, "r_")   ~ "Random",
             str_starts(parameter, "sd")   ~ "Sd",
             str_starts(parameter, "b_hu")   ~ "Hurdle",
             str_starts(parameter, "Intercept")   ~ "Intercept",
             TRUE ~ "Beta"
           )))

ggplot(idf_all%>%
         #filter(!category%in%c("Intercept","Sd")),
       filter(category%in%c("Beta"),
              parameter!="b_Intercept"),
       aes(y = parameter)) +
  geom_vline(xintercept=0,color="gray50")+
  geom_segment(aes(x = ll, xend = hh, color = overlaps_zero_outer),
               linewidth = 0.6) +
  geom_segment(aes(x = l, xend = h, color = overlaps_zero_outer),
               linewidth = 2.0) +
  geom_point(aes(x = m, color = overlaps_zero_outer), size = 2) +
  scale_color_manual(
    values = c("#D55E00","#0072B2"),
    labels = c("95% CI overlaps 0","95% CI excludes 0")) +
  labs(
    x = "Coefficient",
    y = NULL,
    color=NULL,
    title = "Best fitting model coefficients") +
  facet_grid(category~taxon,scale="free_y")+
  theme_bw(base_size = 12)+
  theme(legend.position="bottom",
        strip.background=element_blank())

#### Generate and explore predictions ####
results_hu_wcop$wcop$fits$base$data$Year

preds_wcop <- predict_best_model(results_hu_wcop$wcop,
                                 out_dir = "model_output/aug2026/predictions",
                                 label = "wcop_hu")
preds_gcop <- predict_best_model(results_hu_gcop$gcop,
                                 out_dir = "model_output/aug2026/predictions",
                                 label = "gcop_hu")
preds_bcop <- predict_best_model(results_hu_bcop$bcop,
                                 out_dir = "model_output/aug2026/predictions",
                                 label = "bcop_hu")
preds_clad <- predict_best_model(results_hu_clad$clad,
                                 out_dir = "model_output/aug2026/predictions",
                                 label = "clad_hu")
# preds_wcop$known_levels   # one row per observed Type x Region x Project x Tow x Year
# preds_wcop$new_level      # one row per Type x Region, for an unseen group
#
allpreds <- bind_rows(preds_wcop$summary%>%
                        mutate(category="limno"),
                      preds_gcop$summary%>%
                        mutate(category="gcop"),
                      preds_clad$summary%>%
                        mutate(category="clad"),
                      preds_bcop$summary%>%
                        mutate(category="bcop"))
allpreddraws <- bind_rows(preds_wcop$draws%>%
                            mutate(category="limno"),
                          preds_gcop$draws%>%
                            mutate(category="gcop"),
                          preds_clad$draws%>%
                            mutate(category="clad"),
                          preds_bcop$draws%>%
                            mutate(category="bcop"))
unique(allpreds$level_type)
ggplot(allpreds%>%
         filter(level_type!="observed"))+
  geom_point(aes(x=Region,y=pred,color=Type),
             position=position_dodge(width=1))+
  geom_segment(aes(x=Region,y=lower,yend=upper,color=Type),
               position=position_dodge(width=1))+
  facet_grid(level_type~category)+
  theme_bw()

library(ggdist)

test <- ggplot(allpreddraws%>%
         filter(level_type!="observed")%>%
           mutate(type2=ifelse(is.na(Type),"NA",Type)))+
  stat_halfeye(aes(x=Region,y=log10(pred),color=type2,fill=type2),
             position=position_dodge(width=1))+
  #geom_segment(aes(x=Region,y=lower,yend=upper,color=Type),
  #             position=position_dodge(width=1))+
  #scale_y_continuous(limits=c(0,1e2))+
  scale_fill_manual(values=c("rosybrown1","gray90","paleturquoise"))+
  scale_color_manual(values=c("salmon","black","mediumaquamarine"))+
  facet_grid(.~category)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))
test
