# Top performing Corbicula and Potmocorbula models: spring CPUE, all regions
# Food web synthesis
# Model of invert density, edited for applying to bivalve data
## from: c_ZoopModels_rosie.R created by Rosie Hartman, DWR March 2026
## modified by: Kristi Arend, USFWS

# Last modified: 09/10/2026

#### Load packages -----
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(bayestestR)
library(ggeffects)
library(here) 


#### Load and prep data -----

##load bivalve dataset that Rosie organized.
###this is all the data from these sites:
###Flyway Farms, Winter Island, LICB, Webb Tract, Tule Red, Ryer Island, LHT,
###Liberty, Decker, Chipps, Browns
###along with data witihin 2km in channels

load("Data/d_Bivalves.RData")

#glimpse(Bivalves)


### format Date
Bivalves$Date_d <- as.Date(Bivalves$Date, tz = "America/Los_Angeles")

### Correct data in year and month; create season
biv_data <- Bivalves%>%
  mutate(
    Year = year(Date_d),
    Month = month(Date_d),
    Season = case_when(Month %in% c(3,4,5)~ "Spring",
                       Month %in% c(6,7,8) ~ "Summer",
                       Month %in% c(9,10,11) ~ "Fall",
                       Month %in% c(12,1,2) ~ "Winter"),
    Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
    logCPUE = log(CPUE+1))

# summ across all taxonomic groups, ignoring the NAs
biv_data_wide <- biv_data %>%
  dplyr::select(!logCPUE) %>% # remove logCPUE column
  pivot_wider(names_from = ClamGroup, values_from = CPUE, names_glue = "{ClamGroup}_CPUE") %>%
  rowwise() %>% # treat each row as a group
  mutate(All_CPUE = sum(c_across(ends_with("_CPUE")), na.rm = TRUE)) %>% 
  ungroup() %>% # resolve rowwise grouping
  mutate(All_logCPUE = log(All_CPUE+1))

## load summer-fall mean X2 data (from Dayflow)
mnX2_sf <- readRDS("Data/d_X2_SmFlMn.RDS")

### add a year column to each indicating the subsequent year the X2 is to be associated with
mnX2_sf2 <- mnX2_sf %>%
  mutate(NextYr = Year + 1) %>%
  rename(mnX2sf = mn_X2) %>%
  select(-Year)

## load annual summer-fall specific conductivity mean, median, upper Q3
spc_reg_summ <- readRDS("Data/d_SpCond_reg_summ.rds")

### remove 2015 data and WinterSpring season; remove mean, and median
### add a year column to each indicating the subsequent year the SpC is to be associated with
spc_reg_Q3 <- spc_reg_summ %>%
  filter(year > 2015) %>%
  filter(season == "SummerFall") %>%
  select(year, season, SalRegion, Q3SpC) %>%
  mutate(NextYr = year + 1) %>%
  mutate(Q3SpC_n = unname(Q3SpC)) %>%
  select(-c(year, season, Q3SpC))

## merge prev year X2 and Q3 SpC to bivalve data by year and year and region, respectively
biv_data_X2 <- left_join(biv_data, mnX2_sf2, join_by(Year == NextYr))

biv_data_X2_spc <- left_join(biv_data_X2, spc_reg_Q3, 
                             join_by(Year == NextYr, Region == SalRegion))

biv_data_wide_X2 <- left_join(biv_data_wide, mnX2_sf2, join_by(Year == NextYr))

biv_data_wide_X2_spc <- left_join(biv_data_wide_X2, spc_reg_Q3, 
                                  join_by(Year == NextYr, Region == SalRegion))

# subset spring data
## grand-mean center the mnX2sf variable and calculate the a-score for Q3SpC_n variable
## to remove correlation (for the interaction term)
biv_data_sp <- biv_data_wide_X2_spc %>%
  filter(Season %in% "Spring") %>%
  mutate(mnX2sf_c = mnX2sf - mean(mnX2sf, na.rm = TRUE)) %>%
  mutate(Q3SpC_n_z = (Q3SpC_n - mean(Q3SpC_n, na.rm = TRUE))/sd(Q3SpC_n, na.rm = TRUE))

##### Save data -----
saveRDS(biv_data_sp, file = "Data/d_Bivalves_wide_X2_spc_sp.rds")

#### To rerun models: load saved data -----
biv_data_sp <- readRDS(here("Data/d_Bivalves_wide_X2_spc_sp.rds"))

# Corbicula, spring, lognormal -----
# Region, Region and year in main and Region*mnX2 and year in hurdle
mcor58 <- brm(formula =  bf(Corbicula_CPUE ~ Region + (1|Year),
                            hu ~ Region*mnX2sf_c + (1|Year)), 
              data=biv_data_sp,
              family=hurdle_lognormal(),
              warmup=1000,iter=3000,chains=3,cores=3,thin=10,
              control=list(adapt_delta=0.99))

#look at model summary
summary(mcor58)
plot(mcor58)
mcmc_plot(mcor58)
plot(conditional_effects(mcor58),theme=theme_bw())
plot(conditional_effects(mcor58, dpar = "mu"),theme=theme_bw())
plot(conditional_effects(mcor58, dpar = "hu"),theme=theme_bw())


pp_check(mcor58)
pp_check(mcor58, type = "stat", stat = "mean")
pp_check(mcor58, type = "loo_pit_overlay")

loo_mcor58 <- loo(mcor58)
print(loo_mcor58)
plot(loo_mcor58)

#bayesian p - target is ~ 0.5
T_obs <- mean(biv_data_sp$Corbicula_CPUE)
T_rep <- apply(posterior_predict(mcor58, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs
# 0.56

#region of practical equivalence (ROPE)
rope_result <- rope(mcor58, range = c(-0.1, 0.1))
rope_result

#probability of direction (max probability of effect)
ppd <- p_direction(mcor58)
ppd

# save output
saveRDS(mcor58, file = "Output/Corb_lognorm_h_58.rds")


# Potamocorbula, spring, lognormal -----
## upper 3rd quantile of spec cond and year in main; Q3 SpC, Type, and Year in hurdle
mpot61b <- brm(formula = bf(Potamocorbula_CPUE ~ Q3SpC_n_z + (1|Year),
                            hu ~ Q3SpC_n_z + Type + (1|Year)),
               data = biv_data_sp,
               family = hurdle_lognormal(),
               warmup=1000, iter=3000, chains=3, cores=3, thin=10,
               control=list(adapt_delta=0.99))

#look at model summary
summary(mpot61b)
plot(mpot61b)
mcmc_plot(mpot61b)
plot(conditional_effects(mpot61b),theme=theme_bw())
plot(conditional_effects(mpot61b, dpar = "mu"),theme=theme_bw())
plot(conditional_effects(mpot61b, dpar = "hu"),theme=theme_bw())

pp_check(mpot61b)
pp_check(mpot61b, type = "stat", stat = "mean")
pp_check(mpot61b, type = "loo_pit_overlay")

loo_mpot61b <- loo(mpot61b)
print(loo_mpot61b)
plot(loo_mpot61b)

#bayesian p - target is ~ 0.5
T_obs <- mean(biv_data_sp$Potamocorbula_CPUE)
T_rep <- apply(posterior_predict(mpot61b, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs
# 0.505

#region of practical equivalence (ROPE)
rope_result <- rope(mpot61b, range = c(-0.1, 0.1))
rope_result

#probability of direction (max probability of effect)
ppd <- p_direction(mpot61b)
ppd

# save output
saveRDS(mpot61b, file = "Output/Potamo_lognorm_h_61b.rds")

