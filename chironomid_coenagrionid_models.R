# Chironomid & Coenagrionid CPUE models -- latest dataset, with Habitat
# Pete Nelson, DWR (draft 2026-08-27)

# Rebuild the chironomid hurdle-lognormal model from insect_modeling.R (17 May
# 2026) using the broader, more recent Bugs_allfilters (Mar2026) data and
# sampling-method screen established in "insect data explorations.R", fit
# an equivalent model for Coenagrionidae.
#
# Add `Habitat` as a covariate -- it wasn't in the sample_info pull used for
# the original chironomid model, which only carried Microcystis, Chlorophyll,
# Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month,
# BottomDepth, Tide, Datetime, DO, TurbidityFNU, SizeClass, Volume.
#
# Design choice worth checking:
#  - Sampling-method screen: reuses the prop_zero < 0.3 cut ("good_methods_strict")
#    from the focal-family taxonomic work, NOT the ad hoc 20mm/FRP+Surface/
#    USGSbenthic subset the original May chironomid model used, and not the
#    looser prop_zero < 0.75 cut used for the general diversity work. Swap the
#    filter below if you want a different screen?
#  - Project_na and Habitat are both included as FIXED effects, matching the
#    structure of the original bmod_chir2. This may not hold up for
#    Coenagrionidae as it's rarer than Chironomidae (if the disparity is really 
#    large, then less good of an idea) -- see the sanity
#    check below before fitting; move Project_na and/or Habitat to random
#    intercepts (1 | Project_na) if there isn't enough data per level?
#  - Region is deliberately left out since Project_na (site) is nested within
#    Region -- same as the original model.

# libraries ----
library(here)
library(brms)
library(tidyverse)

# data ----
load(here("data/PrioritySites.RData")) # PrioritySites
load(here("data/Bugs_allfilters.RData")) # Bugs_allfilters (Mar2026 vintage)

# I found that Bugs_allfilters sometimes carries a leftover sf geometry column
# (sfc_POINT) from whatever spatial join originally built it. That breaks
# plain dplyr group_by()/summarise() with a vctrs "vec_size() ... sfc_POINT"
# error, since sf's dplyr methods try to carry/union the geometry through.
# Had Claude write this script to filter and yield plain-tibble dplyr.
if (inherits(Bugs_allfilters, "sf")) {
  message("Bugs_allfilters was an sf object -- dropping geometry column.")
  Bugs_allfilters <- sf::st_drop_geometry(Bugs_allfilters)
}

# sampling method screen ----
good_methods_strict <- Bugs_allfilters %>%
  filter(Class == "Insecta") %>%
  group_by(Source, TowType, SampleID) %>%
  summarise(sample_CPUE = sum(CPUE, na.rm = TRUE), .groups = "drop") %>%
  group_by(Source, TowType) %>%
  summarise(prop_zero = mean(sample_CPUE == 0), .groups = "drop") %>%
  filter(prop_zero < 0.3) %>%
  select(Source, TowType)

Bugs_filtered <- Bugs_allfilters %>%
  semi_join(good_methods_strict, by = c("Source", "TowType"))

# sample universe (for zero-filling) ----
# one row per insect-adequate sample, with the covariates we want in the
# models; Habitat is now included.
sample_meta_filtered <- Bugs_filtered %>%
  distinct(SampleID, .keep_all = TRUE) %>%
  select(SampleID, Project_na, Type, Region, Source, TowType, Habitat,
         Date, Year) %>%
  mutate(
    Season = case_when(
      month(Date) %in% c(12, 1, 2) ~ "Winter",
      month(Date) %in% c(3, 4, 5) ~ "Spring",
      month(Date) %in% c(6, 7, 8) ~ "Summer",
      month(Date) %in% c(9, 10, 11) ~ "Fall"
    ),
    Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")),
    Year = factor(Year)
  )

## check Habitat before using it as a covariate -----
sample_meta_filtered %>% count(Habitat, sort = TRUE)
# ^ look for a large NA/blank chunk -- if a big share of samples have no
# Habitat recorded because brms drops incomplete cases by default and shrink
# our effective sample size. 
nrow(sample_meta_filtered) # looks good

# family-level CPUE, zero-filled ----
build_family_cpue <- function(family_name) {
  Bugs_filtered %>%
    filter(Class == "Insecta", Family == family_name) %>%
    group_by(SampleID) %>%
    summarise(CPUE = sum(CPUE, na.rm = TRUE), .groups = "drop") %>%
    right_join(sample_meta_filtered, by = "SampleID") %>%  # brings in zero-catch samples
    mutate(CPUE = replace_na(CPUE, 0))
}

chir <- build_family_cpue("Chironomidae")
nrow(chir) == nrow(sample_meta_filtered) # checks out

coen <- build_family_cpue("Coenagrionidae")
nrow(coen) == nrow(sample_meta_filtered) # also checks out

## sanity check before modeling -----
# n samples, zero rate, and mean CPUE by family -- Coenagrionidae is almost
# certainly much rarer than Chironomidae (the dominant insect family overall).
# If prop_zero is very high (e.g. > 0.9) or n is small, the full fixed-effect
# structure below may not converge or may produce very wide, uninformative
# posteriors for some terms. Simplify (drop Season and/or Habitat, or switch
# Project_na/Habitat to random intercepts) if so, before spending time on
# adapt_delta/iteration tuning.
bind_rows(
  chir %>% summarise(Family = "Chironomidae",  n = n(), prop_zero = mean(CPUE == 0), mean_CPUE = mean(CPUE)),
  coen %>% summarise(Family = "Coenagrionidae", n = n(), prop_zero = mean(CPUE == 0), mean_CPUE = mean(CPUE))
)

# Ooof. It's borderline...critical how those zeros are distributed across levels of 
# Season, Habitat, and Project_na

# also worth a quick look at how many samples exist per Project_na x Habitat
# combo, since a fixed-effect interaction-heavy formula needs enough coverage
# in each cell:
coen %>% count(Project_na, Habitat) %>% filter(n < 3) %>% nrow()  # cells with <3 samples
coen %>%
  group_by(Habitat) %>%
  summarise(n = n(), prop_zero = mean(CPUE == 0), n_nonzero = sum(CPUE > 0))
# not surprising that Open Water has a super high prop of 0s (0.981), but also
# accountw for most of the samples--dropping these probably not an option, plus
# those zeros are ecologically meaningful (even if "obvious"?!)...let's make 
# Habitat a varying (random) intercept for the Coenagrionidae model

# priors ----
# same weakly-informative structure as the m4 / bmod_chir2 priors, just
# combined into one object since both dpars (mu and hu) have fixed effects
# here
priors_hurdle <- c(
  # exp(5)=148.4; SD=3 is very broad but still excludes the ridiculous
  prior(normal(5, 3), class = Intercept),
  prior(normal(0, 1), class = Intercept, dpar = hu),
  prior(normal(0, 1), class = b),
  prior(normal(0, 1), class = b, dpar = hu),
  prior(exponential(1), class = sd),
  prior(exponential(1), class = sigma)
)

# chironomid models -------
## chir_v2 ------
# chironomid model (v2: Mar2026 data, prop_zero<0.3 screen, + Habitat)
bmod_chir_v2 <- brm(
  bf(CPUE ~ Type + Project_na + Season + Habitat + (1 | Source) + (1 | Year),
     hu ~ Type + Project_na + Season + Habitat + (1 | Source) + (1 | Year)),
  data = chir,
  family = hurdle_lognormal(),
  prior = priors_hurdle,
  chains = 4, iter = 3000, warmup = 1000,
  cores = 4,
  seed = 42,
  control = list(adapt_delta = 0.95),
  file = here("outputs/bmod_chir_v2"),
  file_refit = "on_change"
)

pp_check(bmod_chir_v2)
summary(bmod_chir_v2)
plot(bmod_chir_v2)

mcmc_plot(bmod_chir_v2, variable = "^b_", regex = TRUE) +
  labs(title = "Chironomidae CPUE — fixed effects",
  subtitle = "CPUE ~ Type + Project_na + Season + Habitat + (1 | Source) + (1 | Year) 
         hu ~ Type + Project_na + Season + Habitat + (1 | Source) + (1 | Year)")

plot(conditional_effects(bmod_chir_v2),
     theme = theme_bw())

## chir_v3 -----
# chironomid model (v3: Mar2026 data, prop_zero<0.3 screen, + Habitat, 
# w Project_na as random intercept)
bmod_chir_v3 <- brm(
  bf(CPUE ~ Type + Season + Habitat + (1 | Project_na) + (1 | Source) + (1 | Year),
     hu  ~ Type + Season + Habitat + (1 | Project_na) + (1 | Source) + (1 | Year)),
  data = chir,
  family = hurdle_lognormal(),
  prior = priors_hurdle,
  chains = 4, iter = 3000, warmup = 1000,
  cores = 4,
  seed = 42,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  file = here("outputs/bmod_chir_v3"),
  file_refit = "on_change"
)

pp_check(bmod_chir_v3)
summary(bmod_chir_v3)
plot(bmod_chir_v3)

mcmc_plot(bmod_chir_v3, variable = "^b_", regex = TRUE) +
  labs(title = "Chironomidae CPUE — fixed effects",
       subtitle = "CPUE ~ Type + Season + Habitat + (1 | Project_na) + (1 | Source) + (1 | Year) 
         hu ~ Type + Season + Habitat + (1 | Project_na) + (1 | Source) + (1 | Year)")

plot(conditional_effects(bmod_chir_v3),
     theme = theme_bw())

# coenagrionid models (v1) ----
## coen_v1 -----
# same structure as chironomid for direct comparability -- simplify per the
# sanity check above if convergence/divergences are a problem given lower n.
bmod_coen_v1 <- brm(
  bf(CPUE ~ Type + Project_na + Season + Habitat + (1 | Source) + (1 | Year),
     hu  ~ Type + Project_na + Season + Habitat + (1 | Source) + (1 | Year)),
  data = coen,
  family = hurdle_lognormal(),
  prior = priors_hurdle,
  chains = 4, iter = 3000, warmup = 1000,
  cores = 4,
  seed = 42,
  control = list(adapt_delta = 0.95),
  file = here("outputs/bmod_coen_v1"),
  file_refit = "on_change"
)

pp_check(bmod_coen_v1)
summary(bmod_coen_v1)
plot(bmod_coen_v1)

## coen_v2 -----
bmod_coen_v2 <-
  brm(bf(CPUE ~ Type + Project_na + Season + (1 | Habitat) + (1 | Source) + (1 | Year),
         hu  ~ Type + Project_na + Season + Habitat + (1 | Source) + (1 | Year)),
      data = coen,
      family = hurdle_lognormal(),
      prior = priors_hurdle,
      chains = 4, iter = 3000, warmup = 1000,
      cores = 4,
      seed = 42,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      file = here("outputs/bmod_coen_v2"),
      file_refit = "on_change"
      )

pp_check(bmod_coen_v2)
summary(bmod_coen_v2)
plot(bmod_coen_v2)

# Project_naRyer is notably bad for Coenagrionidae on both model components — significantly 
# higher zero-probability (hu = 1.59 [0.73, 2.49]) and significantly lower CPUE 
# when present (mu = -1.18 [-2.19, -0.12]) — while FlywayFarms and Liberty both 
# show significantly lower zero-probability, i.e., more reliable catches. Season 
# shows no credible effect anywhere in either component, and TypeOutside is 
# suggestive but not significant in mu (-0.33 [-0.74, 0.09]).

# compare chironomid v1 (original) vs v2 (this one) ----
# does the apparent inside/outside effect from bmod_chir2 (insect_modeling.R,
# May 2026 data, no Habitat) hold up under the broader dataset with Habitat
# controlled for? Load the original if it's not already in our environment:
# bmod_chir2 <- readRDS(here("outputs/bmod_chir2.rds"))
fixef(bmod_chir_v2)[c("TypeOutside"), ]  # adjust name if the reference level differs
# compare against the original model's TypeOutside estimate/CI directly

# coefficient plot ----
# had Claude code for a forest/dot-and-whisker plot w color
# plot of fixed effects, colored by whether the 95% CI
# excludes zero, with a title. `fixef()` already has the columns needed
# Estimate, Q2.5, Q97.5 (brms's default 95% CI bounds).

plot_coefs <- function(model, title = NULL, drop_intercepts = TRUE) {
  fe <- fixef(model) %>%
    as_tibble(rownames = "term")

  if (drop_intercepts) {
    fe <- fe %>% filter(!term %in% c("Intercept", "hu_Intercept"))
  }

  fe <- fe %>%
    mutate(
      dpar = if_else(str_starts(term, "hu_"),
                      "hu (zero-probability, logit)",
                      "mu (magnitude, log scale)"),
      term_clean = str_remove(term, "^hu_"),
      # CI "doesn't span 0" -- both bounds share the same sign
      credible = sign(Q2.5) == sign(Q97.5)
    )

  ggplot(fe, aes(x = Estimate, y = reorder(term_clean, Estimate), color = credible)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5)) +
    scale_color_manual(
      values = c(`TRUE` = "firebrick", `FALSE` = "grey60"),
      labels = c(`TRUE` = "95% CI excludes 0", `FALSE` = "95% CI includes 0")
    ) +
    facet_wrap(~ dpar, scales = "free_x") +
    labs(title = title, x = "Estimate", y = NULL, color = NULL) +
    theme_bw() +
    theme(legend.position = "bottom")
}

## plot fixed effects ----
plot_coefs(bmod_chir_v2, title = "Chironomidae CPUE (hurdle-lognormal) -- fixed effects")
plot_coefs(bmod_coen_v1, title = "Coenagrionidae CPUE (hurdle-lognormal) -- fixed effects")

## plot random effects ----
plot_coefs(bmod_chir_v3, title = "Chironomidae CPUE (hurdle-lognormal) -- random effects")
plot_coefs(bmod_coen_v2, title = "Coenagrionidae CPUE (hurdle-lognormal) -- random effects")


# cross-taxon comparison plot ----
# also coded by Claude
# same idea, but both models' coefficients side by side (color = taxon this
# time, not credibility) -- useful for the "SAV effect is nearly identical
# across taxa" writeup flagged as a next step.
compare_coefs <- function(model_list, title = NULL, drop_intercepts = TRUE) {
  fe <- imap_dfr(model_list, function(m, name) {
    fixef(m) %>% as_tibble(rownames = "term") %>% mutate(Family = name)
  })

  if (drop_intercepts) {
    fe <- fe %>% filter(!term %in% c("Intercept", "hu_Intercept"))
  }

  fe <- fe %>%
    mutate(
      dpar = if_else(str_starts(term, "hu_"),
                      "hu (zero-probability, logit)",
                      "mu (magnitude, log scale)"),
      term_clean = str_remove(term, "^hu_")
    )

  ggplot(fe, aes(x = Estimate, y = term_clean, color = Family)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5),
                     position = position_dodge(width = 0.5)) +
    facet_wrap(~ dpar, scales = "free_x") +
    labs(title = title, x = "Estimate", y = NULL) +
    theme_bw() +
    theme(legend.position = "bottom")
}

compare_coefs(
  list(Chironomidae = bmod_chir_v2, Coenagrionidae = bmod_coen_v1),
  title = "Chironomidae vs Coenagrionidae -- fixed effect comparison"
)

# Rosie suggested dropping YBFMP/Surface and 20mm/Oblique because they catch so
# few insects, but relying on the existing prop_zero < 0.3 screen, no extra 
# source/technique exclusion is needed

# she also suggested using Spring data only but bmod_chir_v2 and bmod_coen_v1
# show no credible difference btwn Winter (baseline) and Spring (both mu & hu)
# furthermore, Summer cpue was higher but patchier and Winter was lower...in
# other words, there were real seasonal effects yet dropping Winter simply 
# leaves us with less data to work with

# instead, I set up new models pooling projects into regions (per Rosie's rec),
# and consider the efficacy/wisdom of using only Spring data. I understand that
# we may still want to limit our data to Spring when comparing other taxa!

# had Claude build a function to calc family-level cpue
build_family_cpue <- function(family_name, season_filter = NULL) {
  meta <- sample_meta_filtered
  if (!is.null(season_filter)) meta <- meta %>% filter(Season == season_filter)
  
  Bugs_filtered %>%
    filter(Class == "Insecta", Family == family_name) %>%
    group_by(SampleID) %>%
    summarise(CPUE = sum(CPUE, na.rm = TRUE), .groups = "drop") %>%
    right_join(meta, by = "SampleID") %>%
    mutate(CPUE = replace_na(CPUE, 0))
}

chir_full <- build_family_cpue("Chironomidae")
chir_spring <- build_family_cpue("Chironomidae", season_filter = "Spring")
coen_full <- build_family_cpue("Coenagrionidae")
coen_spring <- build_family_cpue("Coenagrionidae", season_filter = "Spring")

# sanity check -- n and prop_zero across all four
bind_rows(
  chir_full   %>% summarise(Family = "Chironomidae",  Data = "All seasons", n = n(), prop_zero = mean(CPUE == 0)),
  chir_spring %>% summarise(Family = "Chironomidae",  Data = "Spring only", n = n(), prop_zero = mean(CPUE == 0)),
  coen_full   %>% summarise(Family = "Coenagrionidae", Data = "All seasons", n = n(), prop_zero = mean(CPUE == 0)),
  coen_spring %>% summarise(Family = "Coenagrionidae", Data = "Spring only", n = n(), prop_zero = mean(CPUE == 0))
)

# models -- Season kept for the all-seasons models, dropped for Spring-only
# (constant, can't be estimated) ----

bmod_chir_full_region <- brm(
  bf(CPUE ~ Type + Region + Season + Habitat + (1 | Source) + (1 | Year),
     hu  ~ Type + Region + Season + Habitat + (1 | Source) + (1 | Year)),
  data = chir_full, family = hurdle_lognormal(), prior = priors_hurdle,
  chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 42,
  control = list(adapt_delta = 0.95),
  file = here("outputs/bmod_chir_full_region"), file_refit = "on_change"
)

bmod_chir_spring_region <- brm(
  bf(CPUE ~ Type + Region + Habitat + (1 | Source) + (1 | Year),
     hu  ~ Type + Region + Habitat + (1 | Source) + (1 | Year)),
  data = chir_spring, family = hurdle_lognormal(), prior = priors_hurdle,
  chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 42,
  # 1 diverg trans w adapt_delta=0.95, so
  control = list(adapt_delta = 0.99),
  file = here("outputs/bmod_chir_spring_region"), file_refit = "on_change"
)

bmod_coen_full_region <- brm(
  bf(CPUE ~ Type + Region + Season + Habitat + (1 | Source) + (1 | Year),
     hu  ~ Type + Region + Season + Habitat + (1 | Source) + (1 | Year)),
  data = coen_full, family = hurdle_lognormal(), prior = priors_hurdle,
  chains = 4, iter = 4000, warmup = 1500, cores = 4, seed = 42,
  control = list(adapt_delta = 0.99),
  file = here("outputs/bmod_coen_full_region"), file_refit = "on_change"
)

bmod_coen_spring_region <- brm(
  bf(CPUE ~ Type + Region + Habitat + (1 | Source) + (1 | Year),
     hu  ~ Type + Region + Habitat + (1 | Source) + (1 | Year)),
  data = coen_spring, family = hurdle_lognormal(), prior = priors_hurdle,
  chains = 4, iter = 4000, warmup = 1500, cores = 4, seed = 42,
  control = list(adapt_delta = 0.99),
  file = here("outputs/bmod_coen_spring_region"), file_refit = "on_change"
)

# side-by-side comparison of shared coefficients (Season excluded --
# Spring-only model doesn't have it) ----
compare_full_vs_spring <- function(model_full, model_spring, title = NULL) {
  fe <- bind_rows(
    fixef(model_full)   %>% as_tibble(rownames = "term") %>% mutate(Data = "All seasons"),
    fixef(model_spring) %>% as_tibble(rownames = "term") %>% mutate(Data = "Spring only")
  ) %>%
    filter(!term %in% c("Intercept", "hu_Intercept"),
           !str_starts(term, "Season"), !str_starts(term, "hu_Season")) %>%
    mutate(
      dpar = if_else(str_starts(term, "hu_"), "hu (zero-probability)", "mu (magnitude)"),
      term_clean = str_remove(term, "^hu_")
    )
  
  ggplot(fe, aes(x = Estimate, y = term_clean, color = Data)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5), position = position_dodge(width = 0.5)) +
    facet_wrap(~ dpar, scales = "free_x") +
    labs(title = title, x = "Estimate", y = NULL) +
    theme_bw() + theme(legend.position = "bottom")
}

compare_full_vs_spring(bmod_chir_full_region, bmod_chir_spring_region,
                       title = "Chironomidae: all seasons vs Spring only (Region)")
compare_full_vs_spring(bmod_coen_full_region, bmod_coen_spring_region,
                       title = "Coenagrionidae: all seasons vs Spring only (Region)")

# model comparisons ------

bmod_chir_full_region <- add_criterion(bmod_chir_full_region, "loo")
bmod_chir_v2 <- add_criterion(bmod_chir_v2, "loo")
bmod_chir_v3 <- add_criterion(bmod_chir_v3, "loo")
loo_compare(bmod_chir_full_region, bmod_chir_v2, bmod_chir_v3)

# going w bmod_chir_v3, which regularizes those thin sites via partial pooling 
# instead of giving them a free, unconstrained estimate, predicts just as well 
# as v2 — meaning the site-level signal that beat Region is real, not an 
# artifact of overfitting a couple of sparse sites. Given that, let's adopt 
# v3 as the model going forward rather than sticking with v2, even though 
# they're statistically tied on LOO: it's the more defensible choice precisely 
# because it doesn't let Tule Red's 5 data points drive an unconstrained 
# estimate, it matches the same partial-pooling treatment we already gave 
# Habitat in the Coenagrionidae model (so our methodology is consistent across 
# both families if this ends up in a write-up)
# 

bmod_coen_full_region <- add_criterion(bmod_coen_full_region, "loo")
bmod_coen_v1 <- add_criterion(bmod_coen_v1, "loo")
bmod_coen_v2 <- add_criterion(bmod_coen_v2, "loo")
# pareto_k warnings to deal with...

bmod_coen_v2 <- add_criterion(bmod_coen_v2, "loo", reloo = TRUE, overwrite = TRUE)
bmod_coen_v1 <- add_criterion(bmod_coen_v1, "loo", reloo = TRUE, overwrite = TRUE)
bmod_coen_full_region <- add_criterion(bmod_coen_full_region, "loo", reloo = TRUE, overwrite = TRUE)

loo_compare(bmod_coen_full_region, bmod_coen_v1, bmod_coen_v2)

save(bmod_chir_v3, bmod_coen_v2, file = "outputs/insectmodels.RData")
