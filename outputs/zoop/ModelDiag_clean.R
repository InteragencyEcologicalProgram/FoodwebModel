#######################################################################
# Model diagnostics & export for the zooplankton CPUE pipeline
#
# Takes a `results` list (the fits/loos/comparison/timing structure
# produced by `run_pipeline()` or `load_results()` in the modeling
# script) and produces, per dataset:
#   - a model-selection table (LOOIC, elpd, weight, Bayesian p-values)
#   - a combined multi-panel diagnostic figure for the best model
#   - group-level Bayesian p-values (optional; e.g. by Region/Year)
# plus a cross-dataset summary table and model-weight figure.
#
# Structure:
#   1. Packages & config
#   2. Core posterior-predictive helpers
#   3. Bayesian p-value suite (overall + grouped, computed once)
#   4. Model comparison table
#   5. Diagnostic figures (per-model panel + cross-dataset summary)
#   6. Export wrapper
#   7. Interactive inspection helpers
#######################################################################

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(readr)
library(glue)
library(brms)       # pp_check(), posterior_predict(), get_y()
library(loo)         # pareto-k diagnostics
library(ggplot2)
library(patchwork)   # install.packages("patchwork") if needed -- combines ggplots into one figure

#######################################################################
## 1. CONFIG
#######################################################################

DIAG_CONFIG <- list(
  out_dir        = "diagnostics",
  ndraws_bpv     = 400,   # draws used for Bayesian p-value calculations
  ndraws_overlay = 100,   # draws used for pp_check dens_overlay plot
  stats          = c("mean","median", "sd", "zero_prop"),
  group_vars     = NULL   # e.g. c("Region", "Type", "Year") for group-level BPVs
)

#######################################################################
## 2. CORE POSTERIOR-PREDICTIVE HELPERS
#######################################################################

# Discrepancy statistics used throughout. Add new ones here (e.g. "max")
# and they become available everywhere without touching other functions.
STAT_FUNS <- list(
  mean      = function(z) mean(z, na.rm = TRUE),
  median      = function(z) median(z, na.rm = TRUE),
  sd        = function(z) sd(z, na.rm = TRUE),
  zero_prop = function(z) mean(z == 0, na.rm = TRUE)
)

#' Draw posterior predictive replicates once, in a consistent (draws x obs)
#' matrix shape, regardless of whether the model is univariate/multivariate.
posterior_pred_draws <- function(fit, ndraws = 400, re_formula = NULL, resp = NULL) {
  yrep <- brms::posterior_predict(fit, ndraws = ndraws, re_formula = re_formula, resp = resp)
  if (length(dim(yrep)) == 3) yrep <- yrep[, , 1, drop = FALSE]
  if (nrow(yrep) > ndraws) yrep <- yrep[seq_len(ndraws), , drop = FALSE]
  yrep
}

#' Two-sided posterior predictive p-value for one statistic, given
#' already-drawn `yrep` (so this never re-runs posterior_predict()).
bayes_pvalue <- function(y, yrep, stat_fun) {
  if (length(y) == 0 || !is.finite(stat_fun(y))) {
    return(list(p_value = NA_real_, T_obs = NA_real_, p_upper = NA_real_, p_lower = NA_real_))
  }
  T_obs <- stat_fun(y)
  T_rep <- apply(yrep, 1, stat_fun)
  p_upper <- mean(T_rep >= T_obs)
  p_lower <- mean(T_rep <= T_obs)
  list(
    p_value = max(min(2 * min(p_upper, p_lower), 1), 0),
    T_obs = T_obs, p_upper = p_upper, p_lower = p_lower
  )
}

#######################################################################
## 3. BAYESIAN P-VALUE SUITE (overall + grouped, single posterior draw)
##    Replaces the old `compute_bpv()` + `compute_group_bpv_suite()`,
##    which each called posterior_predict() separately per stat/group.
##    Here `yrep` is drawn ONCE and reused for every statistic and
##    every grouping level, since posterior_predict() is the slow step.
#######################################################################

#' @param fit         a brmsfit
#' @param group_vars  character vector of column names in `fit$data` to
#'                     break down BPVs by (e.g. "Region"), or NULL for
#'                     overall-only
#' @param stats       which discrepancy statistics to compute (names in STAT_FUNS)
#' @return long-format tibble: group_var, level, stat, bpv, T_obs, n
compute_bpv_suite <- function(fit, group_vars = NULL, stats = DIAG_CONFIG$stats,
                              ndraws = DIAG_CONFIG$ndraws_bpv,
                              re_formula = NULL, resp = NULL) {
  
  y_raw <- brms::get_y(fit, resp = resp)
  y_obs <- if (is.matrix(y_raw)) as.numeric(y_raw[, 1]) else as.numeric(y_raw)
  yrep  <- posterior_pred_draws(fit, ndraws, re_formula, resp)
  stat_funs <- STAT_FUNS[stats]
  
  summarize_idx <- function(idx, gv_label, lv_label) {
    y_g <- y_obs[idx]
    yrep_g <- yrep[, idx, drop = FALSE]
    purrr::imap_dfr(stat_funs, function(fn, st) {
      # zero_prop is meaningless without any observed zeros to compare against
      if (st == "zero_prop" && !any(y_g == 0, na.rm = TRUE)) {
        return(tibble(stat = st, bpv = NA_real_, T_obs = NA_real_))
      }
      res <- bayes_pvalue(y_g, yrep_g, fn)
      tibble(stat = st, bpv = res$p_value, T_obs = res$T_obs)
    }) %>%
      mutate(group_var = gv_label, level = lv_label, n = length(idx), .before = 1)
  }
  
  overall <- summarize_idx(which(!is.na(y_obs)), "Overall", "Overall")
  
  grouped <- if (!is.null(group_vars)) {
    purrr::map_dfr(group_vars, function(gv) {
      if (!gv %in% names(fit$data)) {
        warning("Grouping variable '", gv, "' not found in model data; skipping.")
        return(tibble())
      }
      grp <- factor(fit$data[[gv]])
      purrr::map_dfr(levels(grp), function(lv) {
        idx <- which(grp == lv & !is.na(y_obs))
        if (length(idx) == 0) return(tibble())
        summarize_idx(idx, gv, lv)
      })
    })
  } else NULL
  
  bind_rows(overall, grouped)
}

#######################################################################
## 4. MODEL COMPARISON TABLE
##    Pulls LOOIC/elpd/weight straight from `res$comparison` (already
##    computed by the modeling pipeline's add_model_weights()) instead
##    of recomputing weights by hand -- one source of truth for weights.
#######################################################################

#' Normalize `res$comparison` into a tibble with a `model` column,
#' regardless of whether it's already a tidy table (from add_model_weights())
#' or a raw `compare.loo` matrix/array (from plain loo::loo_compare(),
#' e.g. an older run or a results object built outside the pipeline).
#' Attaches weights from `res$loos` if they're missing and available.
as_comparison_df <- function(res) {
  cmp <- res$comparison
  if (is.null(cmp)) return(tibble())
  
  if (is.data.frame(cmp) && "model" %in% names(cmp)) {
    return(cmp)  # already tidy (from add_model_weights())
  }
  
  # raw compare.loo matrix/array: rownames hold model names
  cmp_df <- as.data.frame(unclass(cmp), stringsAsFactors = FALSE)
  cmp_df$model <- rownames(cmp)
  
  if (!"weight" %in% names(cmp_df) && !is.null(res$loos) && exists("add_model_weights")) {
    cmp_df <- tryCatch(
      add_model_weights(res$loos, cmp),
      error = function(e) { warning("Could not attach weights: ", e$message); cmp_df }
    )
  }
  cmp_df
}


#' Build a full LOO comparison (elpd, LOOIC, weights) for a set of models
#' that were fit and saved WITHOUT ever going through `fit_and_score()` --
#' e.g. individually-fit models, or fits imported from someone else's
#' `.rds` files -- so `res$comparison` (and possibly `res$loos`) is missing.
#'
#' Mirrors the LOO step inside `fit_and_score()` in the modeling pipeline,
#' but operates on already-fitted models instead of fitting new ones, and
#' reuses any LOO criterion a fit already has attached rather than
#' recomputing it (set `recompute = TRUE` to force a fresh LOO for all
#' models, e.g. if you've changed `loo_strategy`).
#'
#' @param fits         named list of brmsfit objects, OR a `res`-like list
#'                     containing `$fits` (either form is accepted)
#' @param loo_strategy "moment_match" (default) | "reloo" | "plain"
#' @param weight_method passed to `add_model_weights()`: "stacking" (default)
#'                      or "pseudobma"
#' @param recompute    if TRUE, re-run LOO even for fits that already have
#'                      a `$criteria$loo` attached
#' @param out_dir      if supplied, writes the comparison table to
#'                      `{out_dir}/{label}_loo_comparison.csv` (and .rds,
#'                      matching `save_results()`'s naming from the modeling
#'                      pipeline). NULL (default) skips export.
#' @param label        dataset/group label used in the output filename
#'                      when `out_dir` is supplied (default "model")
#' @return list(fits, loos, comparison) -- the same shape produced by
#'         `fit_and_score()`, so it plugs directly into `make_model_table()`,
#'         `best_model_name()`, and `export_diagnostics()`
build_comparison_from_fits <- function(fits,
                                       loo_strategy = "moment_match",
                                       weight_method = "stacking",
                                       recompute = FALSE,
                                       out_dir = NULL,
                                       label = "model") {
  if (!is.null(fits$fits)) fits <- fits$fits  # allow passing a full res-like list
  
  if (!is.list(fits) || is.null(names(fits)) || any(names(fits) == "")) {
    stop("`fits` must be a NAMED list of brmsfit objects (names become model labels).")
  }
  
  loos <- purrr::imap(fits, function(fit, mname) {
    if (!inherits(fit, "brmsfit")) {
      warning("'", mname, "' is not a brmsfit; skipping.")
      return(NULL)
    }
    
    has_loo <- !is.null(fit$criteria$loo)
    if (has_loo && !recompute) {
      message(glue("Using existing LOO for '{mname}'"))
      return(fit$criteria$loo)
    }
    
    message(glue("Computing LOO for '{mname}' (strategy: {loo_strategy})"))
    fit_with_loo <- tryCatch(
      switch(loo_strategy,
             reloo        = add_criterion(fit, "loo", reloo = TRUE),
             moment_match = add_criterion(fit, "loo", moment_match = TRUE),
             add_criterion(fit, "loo")
      ),
      error = function(e) {
        warning("LOO failed for '", mname, "': ", e$message)
        NULL
      }
    )
    if (is.null(fit_with_loo)) NULL else fit_with_loo$criteria$loo
  })
  
  ok <- !purrr::map_lgl(loos, is.null)
  if (!any(ok)) stop("LOO could not be computed for any model; nothing to compare.")
  if (!all(ok)) {
    warning("Excluding from comparison (LOO unavailable): ", paste(names(loos)[!ok], collapse = ", "))
  }
  fits <- fits[ok]
  loos <- loos[ok]
  
  cmp <- loo_compare(loos)
  comparison <- if (exists("add_model_weights")) {
    tryCatch(add_model_weights(loos, cmp, method = weight_method),
             error = function(e) { warning("Could not attach weights: ", e$message); as_comparison_df(list(comparison = cmp)) })
  } else {
    warning("add_model_weights() not found (modeling pipeline not loaded) -- returning comparison without weights.")
    as_comparison_df(list(comparison = cmp))
  }
  
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    csv_path <- file.path(out_dir, glue("{label}_loo_comparison.csv"))
    rds_path <- file.path(out_dir, glue("{label}_loo_comparison.rds"))
    safe_write_csv(comparison, csv_path)
    saveRDS(comparison, rds_path)
    message("Saved comparison table: ", csv_path)
  }
  
  list(fits = fits, loos = loos, comparison = comparison)
}

make_model_table <- function(res, ndraws_bpv = DIAG_CONFIG$ndraws_bpv,
                             re_formula_bpv = NULL, resp = NULL) {
  base_tab <- as_comparison_df(res)
  if (nrow(base_tab) == 0) return(tibble())
  
  base_tab <- base_tab %>%
    select(model, looic, elpd_loo, p_loo, any_of("weight")) %>%
    arrange(looic)
  
  bpv_wide <- purrr::map_dfr(base_tab$model, function(mname) {
    fit <- res$fits[[mname]]
    if (is.null(fit)) {
      warning("Model '", mname, "' not found in res$fits.")
      return(tibble(model = mname, bpv_mean = NA_real_, bpv_sd = NA_real_, bpv_zero_prop = NA_real_))
    }
    compute_bpv_suite(fit, group_vars = NULL, ndraws = ndraws_bpv,
                      re_formula = re_formula_bpv, resp = resp) %>%
      select(stat, bpv) %>%
      pivot_wider(names_from = stat, values_from = bpv, names_prefix = "bpv_") %>%
      mutate(model = mname, .before = 1)
  })
  
  left_join(base_tab, bpv_wide, by = "model")
}

#' Best model by LOOIC. Works whether `res$comparison` is already tidy
#' or a raw `compare.loo` matrix (normalized via `as_comparison_df()`).
best_model_name <- function(res) {
  cmp <- as_comparison_df(res)
  if (nrow(cmp) == 0) stop("No comparison table available.")
  cmp$model[which.min(cmp$looic)]
}

#######################################################################
## 5. DIAGNOSTIC FIGURES
#######################################################################

# ---- 5a. Pareto-k plot (loo's plot method returns a ggplot object) -----
pareto_k_plot <- function(loo_obj) {
  tryCatch(
    plot(loo_obj) + theme_bw() + labs(title = "Pareto k diagnostics"),
    error = function(e) { warning("Pareto-k plot failed: ", e$message); NULL }
  )
}

# ---- 5b. Combined multi-panel diagnostic figure for one model ----------
#' One figure per model instead of 4 separate PNGs: posterior predictive
#' density overlay, Pareto-k diagnostics, and PPC stat checks for mean & sd.
build_diagnostic_panel <- function(fit, ds, model_name,
                                   ndraws_overlay = DIAG_CONFIG$ndraws_overlay,
                                   ndraws_stat = DIAG_CONFIG$ndraws_bpv) {
  p_dens <- pp_check(fit, type = "dens_overlay", ndraws = ndraws_overlay) +
    theme_bw() + labs(title = "Posterior predictive density")
  p_mean <- pp_check(fit, type = "stat", stat = "mean", ndraws = ndraws_stat) +
    theme_bw() + labs(title = "PPC: mean")
  p_median <- pp_check(fit, type = "stat", stat = "median", ndraws = ndraws_stat) +
    theme_bw() + labs(title = "PPC: median")
  p_sd <- pp_check(fit, type = "stat", stat = "sd", ndraws = ndraws_stat) +
    theme_bw() + labs(title = "PPC: sd")
  p_k <- pareto_k_plot(fit$criteria$loo)
  
  panels <- purrr::compact(list(p_dens, p_k, p_mean, p_sd))
  patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_annotation(
      title = glue("{ds} \u2014 {model_name}"),
      subtitle = "Posterior predictive checks & Pareto-k diagnostics"
    )
}

# ---- 5c. Cross-dataset model-weight summary (for presentation) ---------
plot_weight_summary <- function(combined_table) {
  if (!"weight" %in% names(combined_table)) {
    warning("No `weight` column in combined table; skipping summary figure.")
    return(NULL)
  }
  ggplot(combined_table, aes(x = reorder(model, weight), y = weight, fill = model)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ dataset, scales = "free_y") +
    coord_flip() +
    labs(x = NULL, y = "Model weight", title = "Model support by dataset") +
    theme_bw()
}

#######################################################################
## 6. EXPORT WRAPPER
#######################################################################

#' Coerce any table-like object to a data.frame/tibble before writing.
safe_write_csv <- function(x, path) {
  if (is.matrix(x)) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  } else if (!is.data.frame(x) && is.list(x)) {
    x <- tryCatch(as_tibble(x), error = function(e) as.data.frame(x))
  } else if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  readr::write_csv(x, path)
}

#' Main entry point: for every dataset in `results`, write a model
#' selection table + combined diagnostic figure for the best model,
#' plus (optionally) group-level Bayesian p-values. Also writes a
#' combined cross-dataset table and model-weight summary figure.
export_diagnostics <- function(results, out_dir = DIAG_CONFIG$out_dir,
                               ndraws_bpv = DIAG_CONFIG$ndraws_bpv,
                               ndraws_overlay = DIAG_CONFIG$ndraws_overlay,
                               group_vars = DIAG_CONFIG$group_vars) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  per_ds <- purrr::imap(results, function(res, ds) {
    message(glue("Processing dataset: {ds}"))
    
    tab <- make_model_table(res, ndraws_bpv = ndraws_bpv)
    if (nrow(tab) == 0) {
      warning(glue("[{ds}] No models with valid LOOIC; skipping."))
      return(list(table = tibble(), best = NA_character_, panel = NULL))
    }
    tab <- mutate(tab, dataset = ds, .before = 1)
    
    safe_write_csv(tab, file.path(out_dir, glue("{ds}_model_selection.csv")))
    safe_write_csv(as_comparison_df(res), file.path(out_dir, glue("{ds}_loo_compare_raw.csv")))
    
    best <- best_model_name(res)
    fit  <- res$fits[[best]]
    
    # group-level BPVs flag models that fit some subgroups worse than others
    if (!is.null(group_vars)) {
      grp_bpv <- compute_bpv_suite(fit, group_vars = group_vars, ndraws = ndraws_bpv)
      safe_write_csv(grp_bpv, file.path(out_dir, glue("{ds}_{best}_group_bpv.csv")))
    }
    
    panel <- build_diagnostic_panel(fit, ds, best, ndraws_overlay, ndraws_bpv)
    ggsave(file.path(out_dir, glue("{ds}_{best}_diagnostic_panel.png")),
           panel, width = 10, height = 8, dpi = 300)
    
    list(table = tab, best = best, panel = panel)
  })
  
  combined <- bind_rows(purrr::compact(purrr::map(per_ds, "table")))
  
  if (nrow(combined) > 0) {
    safe_write_csv(combined, file.path(out_dir, "model_selection_all_datasets.csv"))
    weight_fig <- plot_weight_summary(combined)
    if (!is.null(weight_fig)) {
      ggsave(file.path(out_dir, "model_weights_summary.png"), weight_fig,
             width = 8, height = 5, dpi = 300)
    }
  } else {
    warning("No combined model selection table to write (no valid LOOIC across datasets).")
  }
  
  list(per_dataset = per_ds, combined_table = combined, out_dir = out_dir)
}

#######################################################################
## 7. INTERACTIVE INSPECTION HELPERS
##    Replaces the repeated summary()/plot()/mcmc_plot()/conditional_effects()
##    blocks (one per dataset) with a single reusable call.
#######################################################################

inspect_model <- function(fit) {
  print(summary(fit))
  print(plot(fit))
  print(mcmc_plot(fit))
  print(plot(conditional_effects(fit), theme = theme_bw()))
  invisible(fit)
}


#######################################################################
## 7b. MODEL SUMMARY PANEL (posterior estimates, not PPC)
##    Combines the pieces you'd otherwise inspect one-by-one --
##    coefficient intervals, trace plots, conditional effects -- into
##    a single patchwork figure per model, matching the style of
##    `build_diagnostic_panel()` in section 5.
#######################################################################

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' @param ce_ncol  number of columns for the conditional-effects sub-panel
#'                  (there can be several effects/predictors, stacked below
#'                  the coefficient-interval and trace plots)
build_summary_panel <- function(fit, ds = NULL, model_name = NULL, ce_ncol = 2) {
  
  p_intervals <- tryCatch(
    mcmc_plot(fit, type = "intervals") + theme_bw() + labs(title = "Coefficient intervals"),
    error = function(e) { warning("Interval plot failed: ", e$message); NULL }
  )
  p_trace <- tryCatch(
    mcmc_plot(fit, type = "trace") + theme_bw() + labs(title = "Trace plots"),
    error = function(e) { warning("Trace plot failed: ", e$message); NULL }
  )
  ce_plots <- tryCatch({
    ce <- conditional_effects(fit)
    plot(ce, plot = FALSE)  # plot = FALSE: build the ggplots without auto-printing each one
  }, error = function(e) { warning("conditional_effects() failed: ", e$message); NULL })
  
  top_row <- patchwork::wrap_plots(purrr::compact(list(p_intervals, p_trace)), ncol = 2)
  
  full <- if (!is.null(ce_plots) && length(ce_plots) > 0) {
    ce_panel <- patchwork::wrap_plots(purrr::map(ce_plots, ~ .x + theme_bw()), ncol = ce_ncol)
    top_row / ce_panel
  } else {
    top_row
  }
  
  full + patchwork::plot_annotation(
    title = glue("{ds %||% ''} {if (!is.null(ds)) '\u2014' else ''} {model_name %||% ''}"),
    subtitle = "Posterior summary: coefficient intervals, trace plots, conditional effects"
  )
}

#' Build (and print, and optionally save) the combined summary panel for
#' one model. Replaces separately printing summary()/plot()/mcmc_plot()/
#' conditional_effects() with a single figure.
inspect_model2 <- function(fit, ds = NULL, model_name = NULL,
                          save = FALSE, out_dir = DIAG_CONFIG$out_dir,
                          width = 10, height = 10) {
  print(summary(fit))  # text summary is still worth printing to console
  
  panel <- build_summary_panel(fit, ds, model_name)
  print(panel)
  
  if (save) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    fname <- glue("{ds %||% 'model'}_{model_name %||% 'fit'}_summary_panel.png")
    ggsave(file.path(out_dir, fname), panel, width = width, height = height, dpi = 300)
    message("Saved: ", file.path(out_dir, fname))
  }
  
  invisible(panel)
}



#######################################################################
## 8. PREDICTIONS: OBSERVED RANDOM-EFFECT LEVELS vs. AN UNKNOWN LEVEL
##    Two related but distinct questions:
##      - "What does the model predict for THIS specific Project/Tow/Year?"
##        -> predict_known_levels(): uses each group's own estimated offset
##      - "What would the model predict for a site/year it's never seen?"
##        -> predict_new_level(): draws a new random effect from the
##           model's estimated between-group variance (NOT the same as
##           re_formula = NA, which would drop RE uncertainty entirely)
##    Both use posterior_epred() (expected CPUE), which for hurdle_gamma
##    already accounts for the hurdle process: E[CPUE] = (1 - hu) * mu.
#######################################################################

#' Distinct combinations of `vars` present in `data`; if none of `vars`
#' exist in `data` (e.g. an intercept-only null model), returns a
#' single-row placeholder grid so downstream code still works.
safe_distinct_grid <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) return(tibble::tibble(.rows = 1))
  dplyr::distinct(data, dplyr::across(dplyr::all_of(vars)))
}

#' Predictions at each combination of fixed effects x random-effect levels
#' actually observed in the model's training data.
#'
#' @param fit         a brmsfit
#' @param fixed_vars  fixed-effect predictors to vary over (default matches
#'                    the modeling pipeline's Type/Region formulas)
#' @param group_vars  random-effect grouping columns to condition on
#'                    (default matches the modeling pipeline's REs)
#' @param type        "epred" (expected value, default) or "predict"
#'                    (full posterior predictive draws, includes hurdle-
#'                    driven zeros)
#' @param prob        width of the summarized credible interval
#' @return list(summary, draws) -- `summary` is one row per combination
#'         (median + interval, as before); `draws` retains every individual
#'         posterior draw (long format, one row per draw x combination) for
#'         downstream custom summaries, plotting, or diagnostics.
predict_known_levels <- function(fit, fixed_vars = c("Type", "Region"),
                                 group_vars = c("Project_na", "SourceTow", "Year"),
                                 type = c("epred", "predict"),
                                 ndraws = DIAG_CONFIG$ndraws_bpv, prob = 0.95) {
  type <- match.arg(type)
  vars_needed <- unique(c(fixed_vars, group_vars))
  newdata <- safe_distinct_grid(fit$data, vars_needed)
  
  draws <- if (type == "epred") {
    tidybayes::add_epred_draws(newdata, fit, re_formula = NULL, ndraws = ndraws)
  } else {
    tidybayes::add_predicted_draws(newdata, fit, re_formula = NULL, ndraws = ndraws)
  }
  val_col <- if (type == "epred") ".epred" else ".prediction"
  
  summary <- tidybayes::median_qi(draws, !!rlang::sym(val_col), .width = prob) %>%
    dplyr::rename(pred = !!val_col, lower = .lower, upper = .upper) %>%
    dplyr::select(-.width, -.point, -.interval) %>%
    dplyr::mutate(level_type = "observed", .before = 1)
  
  draws_out <- draws %>%
    dplyr::rename(pred = !!val_col) %>%
    dplyr::mutate(level_type = "observed", .before = 1)
  
  list(summary = summary, draws = draws_out)
}

#' Names of variables used as FIXED-effect predictors anywhere in the model
#' (any distributional parameter, e.g. mu or hu). Used to detect grouping
#' variables that can't safely get an arbitrary "new" level, because they're
#' not purely `(1 | group)` random effects -- brms still validates fixed-
#' effect factor levels strictly even with allow_new_levels = TRUE, since
#' there's no group-level distribution to sample a new coefficient from.
fixed_effect_vars <- function(fit) {
  tryCatch(unique(unlist(insight::find_predictors(fit, effects = "fixed", flatten = FALSE))),
           error = function(e) character(0))
}

#' Construct a placeholder value for an unseen grouping level that matches
#' the CLASS of the original column, so it can be safely bind_rows()'d with
#' predictions from `predict_known_levels()` later. The actual value just
#' needs to be guaranteed absent from the observed levels/range -- brms
#' only needs it to be recognized as "new" during `validate_newdata()`.
#'   - numeric/integer (e.g. Year): one past the observed max
#'   - Date: one day past the observed max
#'   - factor/character (e.g. Project_na, SourceTow): "new_<varname>" string
#'     (kept as character; brms coerces grouping-factor columns internally)
make_unseen_level <- function(x, varname) {
  if (inherits(x, "Date")) {
    max(x, na.rm = TRUE) + 1
  } else if (is.numeric(x)) {
    val <- max(x, na.rm = TRUE) + 1
    if (is.integer(x)) as.integer(val) else val
  } else {
    paste0("new_", varname)  # character/factor -- brms accepts character here
  }
}

#' Predictions for a hypothetical NEW / unobserved level of each random
#' effect (e.g. a Project, SourceTow, or Year not in the training data),
#' for each fixed-effect combination. Between-group variance the model
#' estimated is preserved in the prediction interval via
#' `sample_new_levels = "gaussian"` (draws a fresh group-level offset from
#' N(0, sd_group) for each posterior draw) rather than discarded.
#'
#' NOTE: some grouping variables (e.g. `SourceTow` when it's also used as
#' a fixed-effect predictor in `hu ~ SourceTow`) can't be assigned an
#' arbitrary unseen level -- brms has no distribution to draw a new
#' fixed-effect coefficient from. These are auto-detected via
#' `fixed_effect_vars()` and instead held at their most common observed
#' level, with a warning, so the "new level" treatment only applies to the
#' variables where it's statistically meaningful (pure `(1 | group)` REs).
#'
#' The unseen-level placeholder is constructed via `make_unseen_level()` to
#' match the original column's class (e.g. numeric Year stays numeric),
#' so this table can be safely `bind_rows()`'d with `predict_known_levels()`.
#'
#' The unseen-level placeholder is constructed via `make_unseen_level()` to
#' match the original column's class (e.g. numeric Year stays numeric),
#' so this table can be safely `bind_rows()`'d with `predict_known_levels()`.
#'
#' @param sample_new_levels "gaussian" (default, recommended) draws new
#'    offsets from the estimated RE distribution; "uncertainty" draws from
#'    existing group posteriors instead. See ?brms::posterior_epred.
#' @return list(summary, draws) -- same shape as `predict_known_levels()`;
#'         `draws` retains every individual posterior draw (long format)
#'         for downstream custom summaries, plotting, or diagnostics.
predict_new_level <- function(fit, fixed_vars = c("Type", "Region"),
                              group_vars = c("Project_na", "SourceTow", "Year"),
                              type = c("epred", "predict"),
                              ndraws = DIAG_CONFIG$ndraws_bpv, prob = 0.95,
                              sample_new_levels = "gaussian") {
  type <- match.arg(type)
  group_vars <- intersect(group_vars, names(fit$data))
  
  fe_vars <- fixed_effect_vars(fit)
  unsafe_groups <- intersect(group_vars, fe_vars)   # also used as a fixed-effect predictor
  safe_groups   <- setdiff(group_vars, unsafe_groups)  # pure (1 | group) REs -- OK to invent a new level
  
  fixed_grid <- safe_distinct_grid(fit$data, fixed_vars)
  for (gv in safe_groups) {
    fixed_grid[[gv]] <- make_unseen_level(fit$data[[gv]], gv)
  }
  
  if (length(unsafe_groups) > 0) {
    for (gv in unsafe_groups) {
      rep_level <- names(sort(table(fit$data[[gv]]), decreasing = TRUE))[1]
      fixed_grid[[gv]] <- rep_level
    }
    warning(
      "Cannot assign an unseen level to ", paste(unsafe_groups, collapse = ", "),
      " -- also used as fixed-effect predictor(s), so brms has no distribution ",
      "to draw a new coefficient from. Held at most common observed level(s): ",
      paste(glue("{unsafe_groups} = '{sapply(unsafe_groups, function(g) names(sort(table(fit$data[[g]]), decreasing = TRUE))[1])}'"), collapse = ", "),
      ". Only ", if (length(safe_groups) > 0) paste(safe_groups, collapse = ", ") else "(none)",
      " reflect a genuinely new/unobserved group.",
      call. = FALSE
    )
  }
  
  draws <- if (type == "epred") {
    tidybayes::add_epred_draws(fixed_grid, fit, re_formula = NULL, ndraws = ndraws,
                               allow_new_levels = TRUE, sample_new_levels = sample_new_levels)
  } else {
    tidybayes::add_predicted_draws(fixed_grid, fit, re_formula = NULL, ndraws = ndraws,
                                   allow_new_levels = TRUE, sample_new_levels = sample_new_levels)
  }
  val_col <- if (type == "epred") ".epred" else ".prediction"
  
  summary <- tidybayes::median_qi(draws, !!rlang::sym(val_col), .width = prob) %>%
    dplyr::rename(pred = !!val_col, lower = .lower, upper = .upper) %>%
    dplyr::select(-.width, -.point, -.interval) %>%
    dplyr::mutate(level_type = "new/unobserved", .before = 1)
  
  draws_out <- draws %>%
    dplyr::rename(pred = !!val_col) %>%
    dplyr::mutate(level_type = "new/unobserved", .before = 1)
  
  list(summary = summary, draws = draws_out)
}

#' Convenience wrapper: runs both prediction modes on the BEST (lowest
#' LOOIC) model from a results list, and optionally exports the combined
#' summary table to CSV. Individual draws are kept in-memory (not written
#' to CSV by default, since they can be large) but returned in the list.
predict_best_model <- function(res, fixed_vars = c("Type", "Region"),
                               group_vars = c("Project_na", "SourceTow", "Year"),
                               type = c("epred", "predict"),
                               ndraws = DIAG_CONFIG$ndraws_bpv, prob = 0.95,
                               sample_new_levels = "gaussian",
                               out_dir = NULL, label = NULL) {
  type <- match.arg(type)
  best <- best_model_name(res)
  fit <- res$fits[[best]]
  
  known <- predict_known_levels(fit, fixed_vars, group_vars, type, ndraws, prob)
  new   <- predict_new_level(fit, fixed_vars, group_vars, type, ndraws, prob, sample_new_levels)
  
  combined_summary <- dplyr::bind_rows(known$summary, new$summary) %>%
    dplyr::mutate(model = best, .before = 1)
  combined_draws <- dplyr::bind_rows(known$draws, new$draws) %>%
    dplyr::mutate(model = best, .before = 1)
  
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    lbl <- label %||% best
    safe_write_csv(combined_summary, file.path(out_dir, glue("{lbl}_predictions.csv")))
    message("Saved predictions: ", file.path(out_dir, glue("{lbl}_predictions.csv")))
    saveRDS(combined_draws, file.path(out_dir, glue("{lbl}_predictions.rds")))
    message("Saved draws: ", file.path(out_dir, glue("{lbl}_predictions.rds")))
  }
  
  list(
    model = best,
    known_levels = known$summary,
    new_level = new$summary,
    summary = combined_summary,
    draws = combined_draws
  )
}
#######################################################################
## USAGE
#######################################################################

# Run the full diagnostics + export pipeline on results from the
# modeling script (`run_pipeline()` output or `load_results()`):
#
# out_hu <- export_diagnostics(results_hu, out_dir = "diagnostics/aug2026",
#                               group_vars = c("Region", "Type", "Year"))
#
# out_hu$combined_table %>% arrange(dataset, looic)
#
# Re-read the combined table later without re-running anything:
# combined <- read_csv(file.path("diagnostics/aug2026", "model_selection_all_datasets.csv"))
# combined %>% arrange(dataset, looic) %>% group_by(dataset) %>% slice_head(n = 5) %>% ungroup()
#
# Spot-check the best model for one dataset interactively:
# inspect_model(results_hu$wcop$fits[[best_model_name(results_hu$wcop)]])


# Generate predictions from the best model, both at observed random-effect
# levels (specific Project/Tow/Year) and for a hypothetical unobserved
# level of each random effect (propagating between-group uncertainty):
#
# preds_wcop <- predict_best_model(results_hu$wcop,
#                                   out_dir = "model_output/aug2026/predictions",
#                                   label = "wcop_hu")
# preds_wcop$known_levels   # one row per observed Type x Region x Project x Tow x Year
# preds_wcop$new_level      # one row per Type x Region, for an unseen group
#
# Works equally well patched into an existing results list missing `$comparison`:
# results_hu_clad$clad[c("loos","comparison")] <- build_comparison_from_fits(results_hu_clad$clad$fits)[c("loos","comparison")]