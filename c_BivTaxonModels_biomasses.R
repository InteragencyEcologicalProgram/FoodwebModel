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

#load("Data/Bivalves.RData")
load("data/clams_withbiomass.RData")
#glimpse(Bivalves)
Bivalves = clams_allfilters

### format Date
Bivalves$Date_d <- as.Date(Bivalves$Date, tz = "America/Los_Angeles") 

### Correct data in year and month; create season
biv_data <- Bivalves%>%
  mutate(Date_d = case_when(is.na(Date_d)~ymd(paste(Year, Month, "1", sep = "-")),
                            TRUE ~ Date_d),
    Year = year(Date_d),
    Month = month(Date_d),
    Season = case_when(Month %in% c(3,4,5)~ "Spring",
                       Month %in% c(6,7,8) ~ "Summer",
                       Month %in% c(9,10,11) ~ "Fall",
                       Month %in% c(12,1,2) ~ "Winter"),
    Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
    logCPUE = log(CPUE+1),
    Species = case_when(is.na(Species) ~ "Other",
                        TRUE ~ Species))


# summ across all taxonomic groups, ignoring the NAs
biv_data_wide <- biv_data %>%
  dplyr::select(!logCPUE) %>% # remove logCPUE column
  pivot_wider(names_from = Species, values_from = Biomass, names_glue = "{Species}_Biomass", values_fn = mean,
              values_fill = 0) %>%
  rowwise() %>% # treat each row as a group
  mutate(All_Biomass = sum(c_across(ends_with("_Biomass")), na.rm = TRUE)) %>% 
  ungroup() %>% # resolve rowwise grouping
  mutate(All_logBiomass = log(All_Biomass+1))

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
saveRDS(biv_data_sp, file = "Data/d_Bivalves_wide_X2_spc_sp2.rds")

#### To rerun models: load saved data -----
biv_data_sp <- readRDS(here("Data/d_Bivalves_wide_X2_spc_sp2.rds"))

# Corbicula, spring, lognormal -----
# Region, Region and year in main and Region*mnX2 and year in hurdle
mcor58 <- brm(formula =  bf(Corbicula_Biomass ~ Region + (1|Year),
                            hu ~ Region*mnX2sf_c + (1|Year)), 
              data=biv_data_sp,
              family=hurdle_lognormal(),
              warmup=1000,iter=3000,chains=3,cores=3,thin=10,
              control=list(adapt_delta=0.99),  backend = "cmdstanr")

#look at model summary
summary(mcor58)
plot(mcor58)
mcmc_plot(mcor58)
plot(conditional_effects(mcor58),theme=theme_bw())
plot(conditional_effects(mcor58, dpar = "mu"),theme=theme_bw())
plot(conditional_effects(mcor58, dpar = "hu"),theme=theme_bw())

#I need a better interaction plot
X2mean  = filter(biv_data_wide_X2_spc, Season == "Spring")$mnX2sf %>%
  mean( na.rm = TRUE)
SPCmean = filter(biv_data_wide_X2_spc, Season == "Spring")$Q3SpC_n %>%
  mean( na.rm = TRUE)
SPCsd = filter(biv_data_wide_X2_spc, Season == "Spring")$Q3SpC_n %>%
 sd( na.rm = TRUE)

coneff_corbic = conditional_effects(mcor58)
coneff_corbic_plot = coneff_corbic$`mnX2sf_c:Region` %>%
  mutate(X2 = mnX2sf_c + X2mean)

ggplot(coneff_corbic_plot, aes(x = X2, y = estimate__, fill = Region)) + geom_line()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3)+
  facet_wrap(~Region)+xlab("Previous Year's Summer Fall X2")+ 
  ylab("Corbicula biomass (g)\nper meter squared")+
  theme_bw()+
  theme(legend.position = "inside", legend.position.inside = c(.8,.2,.2,.1))


pp_check(mcor58)
pp_check(mcor58, type = "stat", stat = "mean")
pp_check(mcor58, type = "loo_pit_overlay")

loo_mcor58 <- loo(mcor58)
print(loo_mcor58)
plot(loo_mcor58)

#bayesian p - target is ~ 0.5
T_obs <- mean(biv_data_sp$Corbicula_Biomass)
T_rep <- apply(posterior_predict(mcor58, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs
# 0.97
#ew, mayb not so great.

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
mpot61b <- brm(formula = bf(Potamocorbula_Biomass ~ Q3SpC_n_z + (1|Year),
                            hu ~ Q3SpC_n_z + Type + (1|Year)),
               data = biv_data_sp,
               family = hurdle_lognormal(),
               warmup=1000, iter=4000, chains=3, cores=3, thin=10,
               control=list(adapt_delta=0.99),  backend = "cmdstanr", save_pars = save_pars(all = TRUE))

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
T_obs <- mean(biv_data_sp$Potamocorbula_Biomass)

T_obs1= mean(biv_data_sp$Potamocorbula_Biomass[which(biv_data_sp$Potamocorbula_Biomass != 0)])
T_obs0 = length(biv_data_sp$Potamocorbula_Biomass[which(biv_data_sp$Potamocorbula_Biomass == 0)])


fest = posterior_predict(mpot61b, draws = 1000) %>%
  as.data.frame()
foo = rowMeans(fest)
T_rep2 <- apply(posterior_predict(mpot61b, draws = 1000), 1, function(x){
  df = data.frame(Zeros = length(x[which(x==0)]), 
                  Mean= mean(x[which(x!=0)]))
}) %>%
  bind_rows()

T_rep <- apply(posterior_predict(mpot61b, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs# 0.91 not great
bayes_p2 = mean(T_rep2$Mean >= T_obs1) #also not great
bayes_p3 = mean(T_rep2$Zeros >= T_obs0) #oh, this is very good!


#region of practical equivalence (ROPE)
hist(log(biv_data_sp$Potamocorbula_Biomass+1))
rope_result <- rope(mpot61b, range = c(-0.05, 0.05))
rope_result

#probability of direction (max probability of effect)
ppd <- p_direction(mpot61b)
ppd

# save output
saveRDS(mpot61b, file = "Outputs/Potamo_lognorm_h_61b.rds")

mpot1 <- brm(formula = bf(Potamocorbula_Biomass ~ Region+Type + (1|Year),
                            hu ~ Region + Type + (1|Year)),
               data = biv_data_sp,
               family = hurdle_lognormal(),
               warmup=1000, iter=4000, chains=3, cores=3, thin=10,
               control=list(adapt_delta=0.99),  backend = "cmdstanr", save_pars = save_pars(all = TRUE))

summary(mpot1)
plot(mpot1)
mcmc_plot(mpot1)
plot(conditional_effects(mpot1),theme=theme_bw())
plot(conditional_effects(mpot1, dpar = "mu"),theme=theme_bw())
plot(conditional_effects(mpot1, dpar = "hu"),theme=theme_bw())

pp_check(mpot1)
pp_check(mpot1, type = "stat", stat = "mean")
pp_check(mpot1, type = "loo_pit_overlay")



#bayesian p - target is ~ 0.5
T_obs <- mean(biv_data_sp$Potamocorbula_Biomass)
T_rep <- apply(posterior_predict(mpot61b, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs


#bayesian p - target is ~ 0.5

T_rep2a <- apply(posterior_predict(mpot1, draws = 1000), 1, function(x){
  df = data.frame(Zeros = length(x[which(x==0)]), 
                  Mean= mean(x[which(x!=0)]))
}) %>%
  bind_rows()

T_rep <- apply(posterior_predict(mpot1, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs# 0.91 not great
bayes_p2 = mean(T_rep2$Mean >= T_obs1) #also not great
bayes_p3 = mean(T_rep2a$Zeros >= T_obs0) #oh, this is very good!


# try something else
 mpot2 <- brm(formula = bf(Potamocorbula_Biomass ~ Q3SpC_n_z+Region + Type + (1|Year),
                            hu ~ Q3SpC_n_z + Type + (1|Year)),
               data = biv_data_sp,
               family = hurdle_lognormal(),
               warmup=1000, iter=4000, chains=3, cores=3, thin=10,
               control=list(adapt_delta=0.99),  backend = "cmdstanr", save_pars = save_pars(all = TRUE))

 
 summary(mpot2)
 plot(mpot2)
 mcmc_plot(mpot2)
 plot(conditional_effects(mpot2),theme=theme_bw())
 plot(conditional_effects(mpot2, dpar = "mu"),theme=theme_bw())
 plot(conditional_effects(mpot2, dpar = "hu"),theme=theme_bw())

 
 loo(mpot61b, mpot1, mpot2, moment_match = TRUE)

 
#just use Kristi's best model for now
 
 
 Newdat_biv = data.frame(Year = unique(biv_data_sp$Year)) %>%
   mutate(mnX2sf_c =0, Q3SpC_n_z =0) %>%
   merge(data.frame(Region = unique(biv_data_sp$Region)))
 
 Newdat_biv2 = data.frame(Year = unique(biv_data_sp$Year)) %>%
   mutate(mnX2sf_c =0, Q3SpC_n_z =0) %>%
   merge(data.frame(Region = unique(biv_data_sp$Region))) %>%
   merge(data.frame(Type = unique(biv_data_sp$Type)))
 
 
corbic_predictions = predict(mcor58, newdata = Newdat_biv)  %>%
   bind_cols(Newdat_biv)
 

potam_predictions = predict(mpot61b, newdata = Newdat_biv2)  %>%
  bind_cols(Newdat_biv2)

save(corbic_predictions, potam_predictions, file = "outputs/bivalve_predictions.RData")

#eh, should I use "predict" or "posterior_predict"? Need to ask Matt.
 