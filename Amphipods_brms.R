#amphipods with BRMS


####packages####
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(bayestestR)
library(ggeffects)

####functions####
write_brmsfit <- function(fit, file, compress = "xz") {
  # Validate inputs
  if (!inherits(fit, "brmsfit")) {
    stop("`fit` must be a brmsfit object.")
  }
  if (!is.character(file) || length(file) != 1) {
    stop("`file` must be a single string path.")
  }
  
  # Try saving
  tryCatch({
    saveRDS(fit, file = file, compress = compress)
    message("Model saved to: ", normalizePath(file))
    TRUE
  }, error = function(e) {
    message("Error saving model: ", e$message)
    FALSE
  })
}

get_coef_draws <- function(fit, model_name) {
  posterior::as_draws_df(fit) %>%
    dplyr::select(dplyr::starts_with("b_")) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "term",
      values_to = "draw"
    ) %>%
    dplyr::mutate(
      model = model_name,
      term = sub("^b_", "", term)  # remove b_ prefix for cleaner labels
    )
}


####prep data####

#load calanoid copepod dataset that Rosie organized.
#this is all the data from these sites:
#Flyway Farms, Winter Island, LICB, Webb Tract, Tule Red, Ryer Island, LHT,
#Liberty, Decker, Chipps, Browns
#along with data witihin 2km in channels

load("data/Amphipods.RData")
glimpse(Amphipoda)

gamarid_data <- Amphipoda%>%
  filter(AmphGroup == "Gammaridae and friends", SizeClass == "Macro") %>%
  mutate(Month = month(Date),
    Season = case_when(Month %in% c(3,4,5)~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall",
                            Month %in% c(12,1,2) ~ "Winter"),
         Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
         yr_mo=paste(Year,Month,sep="_"),
         wetland_na_yr_mo=paste(Project_na,Year,Month,sep="_"),
         wetland_yr_sea=paste(Project_na,Year,Season,sep="_"),
         
         Reagion_yr_sea=paste(Region,Year,Season,sep="_"),
         wetland_yr=paste(Project_na,Year, sep="_"),
         logCPUE = log(CPUE+1)) %>%
  ungroup()

unique(Amphipoda$Project_na)


unique(gamarid_data%>%
         select(Type,Project_na, Region)) 

#exploritory plots

ggplot(gamarid_data, aes(x = Year, y = logCPUE, color = Project_na))+
  facet_grid(Season ~ Type)+ geom_point()

#very little winter data

ggplot(gamarid_data, aes(x = Year, y = logCPUE, color = Project_na))+
  facet_grid(Region ~ Type)+ geom_point()


ggplot(gamarid_data, aes(x = SalSurf, y = logCPUE, color = Project_na))+
  facet_wrap(~Region, scales = "free_x")+ geom_point()+ geom_smooth()
#that looks ugly

####run example model####

#We can do this with log-transformed CPUE or a hurdle lognormal model
#I haven't quite figured out the best way to deal with "project" and "region"
cachedata = filter(gamarid_data, Region ==  "Cache Slough", !is.na(Season), !is.na(Source),
                   !is.na(Year), !is.na(logCPUE), !is.na(Type))

m_gamarid2 <- brm(formula = logCPUE ~ Type + 
                #Region +
                  Project_na+
                Season + 
                (1|Source)+
                (1|Year),
              data=filter(gamarid_data, Region ==  "Cache Slough"),
              family=gaussian(),
              warmup=1000,iter=3000,chains=3,cores=3,thin=10,
              control=list(adapt_delta=0.99))


#I got all these warnings with region in the model and all teh data. no warnings when just Cahce. 
# 1: Rows containing NAs were excluded from the model. 
# 2: There were 600 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 3: Examine the pairs() plot to diagnose sampling problems
# 
# 4: The largest R-hat is 1.97, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 


m_gamarid3 <- brm(formula =  bf(CPUE ~ Type*Project_na +
                             # Region + 
                              (1|Source)+
                              (1|Year),
                            hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
              data=filter(gamarid_data, Region ==  "Cache Slough"),
              family=hurdle_lognormal(),
              warmup=1000,iter=3000,chains=3,cores=3,thin=10,
              control=list(adapt_delta=0.99))



write_brmsfit(m_gamarid2,"fit_m_gamarid2.rds")
write_brmsfit(m_gamarid3,"fit_m_gamarid3.rds")


#### example exploration of model output ####
#look at model summary
summary(m_gamarid3)
plot(m_gamarid3)
mcmc_plot(m_gamarid3)
plot(conditional_effects(m_gamarid3),theme=theme_bw())
pp_check(m_gamarid3, type = "loo_pit_overlay")


#compare to the gaussian version
summary(m_gamarid2)
plot(m_gamarid2)
mcmc_plot(m_gamarid2)
plot(conditional_effects(m_gamarid2),theme=theme_bw())
pp_check(m_gamarid2, type = "loo_pit_overlay")


#posterior prediction check
pp_check(m_gamarid3)
pp_check(m_gamarid3, type = "stat", stat = "mean")

#bayesian p - target is ~ 0.5
T_obs <- mean(gamarid_data$logCPUE)
T_rep <- apply(posterior_predict(m_gamarid3, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs

#region of practical equivalence (ROPE)
# - user defined ROPE - how much of prob. distribution overlaps
# with a 'negligible effect size', "not zero, but basically meaningless"
rope_result <- rope(m_gamarid3, range = c(-0.1, 0.1))
rope_result
# 

#probability of direction (max probability of effect)
# - probability that parameter is strictly positive/negative, 
# analog to p-value for individual estimates
ppd <- p_direction(m_gamarid3)
ppd



#model comparison
waic(m_gamarid3,m_gamarid2)
loo(m_gamarid3,m_gamarid2)

#how much variability is there in your random effect?
#if random effect
m_gamarid3 %>%
  spread_draws(r_Year[Year], sd_Year__Intercept) %>%
  head(15)

r_draws <- m_gamarid3 %>%
  spread_draws(r_Year[Year], sd_Year__Intercept) 
r_draws2 <- m_gamarid3 %>%
  spread_draws(r_Source[Source], sd_Source__Intercept) 

ggplot(data=r_draws2,aes(x = r_Source, y = Source)) +
  stat_halfeye(aes(group=Source))+
  theme_bw()
ggplot(data=r_draws,aes(x = r_Year, y = Year)) +
  stat_halfeye(aes(group=Year))+
  theme_bw()


m_gamarid4 <- brm(formula =  bf(CPUE ~ Type*Project_na +
                                   Region + 
                                  (1|Source)+
                                  (1|Year),
                                hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
                  data=filter(gamarid_data, !Region %in% c( "Decker","Web Tract Berms")),
                  family=hurdle_lognormal(),
                  warmup=1000,iter=5000,chains=3,cores=3,thin=10,
                  control=list(adapt_delta=0.99))
warnings(m_gamarid4)
#that's really bad. Why  so bad?

m_gamarid4.1 <- brm(formula =  bf(CPUE ~ Type*Project_na +
                                  #Region + 
                                  (1|Source)+
                                  (1|Year),
                                hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
                  data=filter(gamarid_data, !Region %in% c( "Decker","Web Tract Berms"), Season != "Winter"),
                  family=hurdle_lognormal(),
                  warmup=1000,iter=5000,chains=3,cores=3,thin=10,
                  control=list(adapt_delta=0.99))
#so having region and project gives the problem. Which makes sense becasue each project is only in one region

m_gamarid5 <- brm(formula =  bf(CPUE ~ Type*Region + 
                                  (1|Source)+
                                  (1|Year),
                                hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
                  data=gamarid_data,
                  family=hurdle_lognormal(),
                  warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                  control=list(adapt_delta=0.99))
#oh, way better 

summary(m_gamarid5)
plot(m_gamarid5)
mcmc_plot(m_gamarid5)
plot(conditional_effects(m_gamarid5),theme=theme_bw())
pp_check(m_gamarid5, type = "loo_pit_overlay")
# really need to figure out what this means. 

m_gamarid5.1 <- brm(formula =  bf(CPUE ~ Type*Region + Season +
                                  (1|Source)+
                                  (1|Year),
                                hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
                  data=gamarid_data,
                  family=hurdle_lognormal(),
                  warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                  control=list(adapt_delta=0.99))
#oh, way better 

summary(m_gamarid5.1)
plot(m_gamarid5.1)
mcmc_plot(m_gamarid5.1)
plot(conditional_effects(m_gamarid5.1),theme=theme_bw())
pp_check(m_gamarid5.1, type = "loo_pit_overlay")
# really need to figure out what this means. 






m_gamarid6 <- brm(formula =  bf(CPUE ~ Type+Region + Season +
                                  # Region + 
                                  (1|Source)+
                                  (1|Year),
                                hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
                  data=gamarid_data,
                  family=hurdle_lognormal(),
                  warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                  control=list(adapt_delta=0.99))

loo(m_gamarid5,m_gamarid5.1,  m_gamarid6)

#the one with season and the interaction term is best, but the interaction didn't add a lot. 


#how much variability is there in your random effect?
#if random effect

r_draws <- m_gamarid5.1 %>%
  spread_draws(r_Year[Year], sd_Year__Intercept) 
r_draws2 <- m_gamarid5.1 %>%
  spread_draws(r_Source[Source], sd_Source__Intercept) 

ggplot(data=r_draws2,aes(x = r_Source, y = Source)) +
  stat_halfeye(aes(group=Source))+
  theme_bw()
ggplot(data=r_draws,aes(x = r_Year, y = Year)) +
  stat_halfeye(aes(group=Year))+
  theme_bw()

save(m_gamarid5,m_gamarid5.1,  m_gamarid6, file = "outputs/gamarid_brms.RData")

#why is 2023 so different?

######### corophiids ######################################


corph_data <- Amphipoda%>%
  filter(AmphGroup == "Corophiidae", SizeClass == "Macro") %>%
  mutate(Month = month(Date),
         Season = case_when(Month %in% c(3,4,5)~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall",
                            Month %in% c(12,1,2) ~ "Winter"),
         Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
         yr_mo=paste(Year,Month,sep="_"),
         wetland_na_yr_mo=paste(Project_na,Year,Month,sep="_"),
         wetland_yr_sea=paste(Project_na,Year,Season,sep="_"),
         
         Reagion_yr_sea=paste(Region,Year,Season,sep="_"),
         wetland_yr=paste(Project_na,Year, sep="_"),
         logCPUE = log(CPUE+1)) %>%
  ungroup()



m_corph5 <- brm(formula =  bf(CPUE ~ Type*Region + 
                                  (1|Source)+
                                  (1|Year),
                                hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
                  data=corph_data,
                  family=hurdle_lognormal(),
                  warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                  control=list(adapt_delta=0.99))
#oh, way better 

summary(m_corph5)
plot(m_corph5)
mcmc_plot(m_corph5)
plot(conditional_effects(m_corph5),theme=theme_bw())
pp_check(m_corph5, type = "loo_pit_overlay")
# really need to figure out what this means. 

m_corph5.1 <- brm(formula =  bf(CPUE ~ Type*Region + Season +
                                    (1|Source)+
                                    (1|Year),
                                  hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
                    data=corph_data,
                    family=hurdle_lognormal(),
                    warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                    control=list(adapt_delta=0.99))
#oh, way better 

summary(m_corph5.1)
plot(m_corph5.1)
mcmc_plot(m_corph5.1)
plot(conditional_effects(m_corph5.1),theme=theme_bw())
pp_check(m_corph5.1, type = "loo_pit_overlay")
# really need to figure out what this means. 






m_corph6 <- brm(formula =  bf(CPUE ~ Type+Region + Season +
                                  # Region + 
                                  (1|Source)+
                                  (1|Year),
                                hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
                  data=corph_data,
                  family=hurdle_lognormal(),
                  warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                  control=list(adapt_delta=0.99))

loo(m_corph5,m_corph5.1,  m_corph6)
