#model of invert density

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

load("data/Calanoids.RData")
glimpse(Calanoids)

cal_data <- Calanoids%>%
  mutate(Season = case_when(Month %in% c(3,4,5)~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall",
                            Month %in% c(12,1,2) ~ "Winter"),
         Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")),
    yr_mo=paste(Year,Month,sep="_"),
         wetland_na_yr_mo=paste(Project_na,Year,Month,sep="_"),
         wetland_yr_sea=paste(Project_na,Year,Season,sep="_"),
    
    Reagion_yr_sea=paste(Region,Year,Season,sep="_"),
         wetland_yr=paste(Project_na,Year, sep="_"),
    logCPUE = log(CPUE+1))



unique(cal_data%>%
         select(Type,Project_na, Region))

calexplore <- cal_data%>%
  group_by(Project_na,Type,Year)%>%
  summarise(n=n())%>%
  spread(Year,n)


####run example model####

#We can do this with log-transformed CPUE or a hurdle lognormal model
#I haven't quite figured out the best way to deal with "project" and "region"

m_cal2 <- brm(formula = logCPUE ~ Type + 
               Region +Project_na+
               Season + 
               (1|Source)+
               (1|Year),
             data=cal_data,
             family=gaussian(),
             warmup=1000,iter=3000,chains=3,cores=3,thin=10,
             control=list(adapt_delta=0.99))

m_cal3 <- brm(formula =  bf(CPUE ~ Type*Project_na +
                              Region + 
                              (1|Source)+
                              (1|Year),
                            hu ~ 1), #this is the hurdle or zero-inflation component. It's currently just the intercept, could add other predictors
              data=cal_data,
              family=hurdle_lognormal(),
              warmup=1000,iter=3000,chains=3,cores=3,thin=10,
              control=list(adapt_delta=0.99))




write_brmsfit(m_cal3,"fit_cal3.rds")


#### example exploration of model output ####
#look at model summary
summary(m_cal3)
plot(m_cal3)
mcmc_plot(m_cal3)
plot(conditional_effects(m_cal3),theme=theme_bw())

#compare to the gaussian version
summary(m_cal2)
plot(m_cal2)
mcmc_plot(m_cal2)
plot(conditional_effects(m_cal2),theme=theme_bw())



#posterior prediction check
pp_check(m_cal3)
pp_check(m_cal3, type = "stat", stat = "mean")

#bayesian p - target is ~ 0.5
T_obs <- mean(cal_data$logCPUE)
T_rep <- apply(posterior_predict(m_cal3.1, draws = 1000), 1, mean)
bayes_p <- mean(T_rep >= T_obs)     # Proportion of times T_rep >= T_obs

#region of practical equivalence (ROPE)
# - user defined ROPE - how much of prob. distribution overlaps
# with a 'negligible effect size', "not zero, but basically meaningless"
rope_result <- rope(m_cal, range = c(-0.1, 0.1))
rope_result

#probability of direction (max probability of effect)
# - probability that parameter is strictly positive/negative, 
# analog to p-value for individual estimates
ppd <- p_direction(brmtest)
ppd


#model comparison
waic(brmtest,brmtest2)
loo(brmtest,brmtest2)

#how much variability is there in your random effect?
#if random effect
brmtest %>%
  spread_draws(r_WY[WY], sd_WY__Intercept) %>%
  head(15)

r_draws <- m_cal3 %>%
  spread_draws(r_year[year], sd_year__Intercept) 
r_draws2 <- m_cal3 %>%
  spread_draws(r_Source[Source], sd_Source__Intercept) 

ggplot(data=r_draws2,aes(x = r_Source, y = Source)) +
  stat_halfeye(aes(group=Source))+
  theme_bw()
ggplot(data=r_draws,aes(x = r_year, y = year)) +
  stat_halfeye(aes(group=year))+
  theme_bw()

#### mor calanoid models ####

#compare gamma to lognormal
m_cal_v1g <- brm(formula = CPUE ~ Type*Project_na +
                  Season + 
                  (1|Source)+
                  (1|Year),
                data=cal_data,
                family=hurdle_gamma(link = "log"),
                warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                control=list(adapt_delta=0.99))
#all sorts of nasty warning messages. running the chains with more itterations might help

m_cal_v1l <- brm(formula = CPUE ~ Type*Project_na +
                   Season + 
                   (1|Source)+
                   (1|Year),
                 data=cal_data,
                 family=hurdle_lognormal(),
                 warmup=1000,iter=6000,chains=3,cores=3,thin=10,
                 control=list(adapt_delta=0.99))
#more nasty warning messages. running the chains with more itterations might help
m_cal_v2 <- brm(formula = CPUE~ Type*Region +
                  Season + 
                  (1|Project_na)+
                  (1|Source)+
                  (1|Year),
                data=cal_data,
                family=hurdle_gamma(link = "log"),
                warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                control=list(adapt_delta=0.99))

m_cal_v3 <- brm(formula = CPUE ~ Region +
                  Season+ 
                  #(1|Project_na)+
                  (1|Source)+
                  (1|Year),
                data=cal_data,
                family=hurdle_gamma(link = "log"),
                warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                control=list(adapt_delta=0.99))

m_cal_v4 <- brm(formula = CPUE~ Season+
                  #sea + 
                  #(1|Project_na)+
                  (1|Source)+
                  (1|Year),
                data=cal_data,
                family=hurdle_gamma(link = "log"),
                warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                control=list(adapt_delta=0.99))

m_cal_v5 <- brm(formula = CPUE~ Project_na +
                  Season + 
                  #(1|Region)+
                  (1|Source)+
                  (1|Year),
                data=cal_data,
                family=hurdle_gamma(link = "log"),
                warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                control=list(adapt_delta=0.99))

m_cal_v6 <- brm(formula = Calanoida ~ Type + Project_na +
                   Season + 
                   (1|Source)+
                   (1|Year),
                 data=cal_data,
                 family=hurdle_gamma(link = "log"),
                 warmup=1000,iter=3000,chains=3,cores=3,thin=10,
                 control=list(adapt_delta=0.99))

#model test
summary(m_cal_v1g)
summary(m_cal_v2)
summary(m_cal_v3)
summary(m_cal_v4)

#Note from Rosie: This is as far as I got. I haven't done anything else. 

bp_calv1 <- mean(apply(posterior_predict(m_cal_v1g, draws = 1000), 1, mean) >= mean(cal_data$CPUE)) #T_rep >= T_obs
#bp_calv1b <- mean(apply(posterior_predict(m_cal_v1l, draws = 1000), 1, mean) >= mean(cal_data$CPUE)) #T_rep >= T_obs
#bp_calv1c <- mean(apply(posterior_predict(m_cal_v1g1, draws = 1000), 1, mean) >= mean(cal_data$CPUE+0.01)) #T_rep >= T_obs
bp_calv2 <- mean(apply(posterior_predict(m_cal_v2, draws = 1000), 1, mean) >= mean(cal_data$CPUE)) #T_rep >= T_obs
bp_calv3 <- mean(apply(posterior_predict(m_cal_v3, draws = 1000), 1, mean) >= mean(cal_data$CPUE)) #T_rep >= T_obs
bp_calv4 <- mean(apply(posterior_predict(m_cal_v4, draws = 1000), 1, mean) >= mean(cal_data$CPUE)) #T_rep >= T_obs
bp_calv5 <- mean(apply(posterior_predict(m_cal_v4, draws = 1000), 1, mean) >= mean(cal_data$CPUE)) #T_rep >= T_obs

#model comparison
loo(m_cal_v1l)
loo(m_cal_v2)
loo(m_cal_v3)
loo(m_cal_v4)
m_cal_v1g <- add_criterion(m_cal_v1g, "loo")
m_cal_v2 <- add_criterion(m_cal_v2, "loo")
m_cal_v3 <- add_criterion(m_cal_v3, "loo")
m_cal_v4 <- add_criterion(m_cal_v4, "loo")
m_cal_v5 <- add_criterion(m_cal_v5, "loo")

loo_compare(m_cal_v1g,m_cal_v2,m_cal_v3,m_cal_v4,m_cal_v5)
waic(m_cal_v1g,m_cal_v2,m_cal_v3)
model_weights(m_cal_v1g,m_cal_v2,m_cal_v3,m_cal_v4,m_cal_v5,weights="loo")


#how much variability is there in your random effect?
cal_r_draws <- m_cal_v1g %>%
  spread_draws(r_year[Year], sd_year__Intercept) 
cal_r_draws2 <- m_cal_v1g %>%
  spread_draws(r_Source[Source], sd_Source__Intercept) 

ggplot(data=cal_r_draws2,aes(x = r_Source, y = Source)) +
  stat_halfeye(aes(group=Source))+
  theme_bw()
ggplot(data=cal_r_draws,aes(x = r_year, y = Year)) +
  stat_halfeye(aes(group=Year))+
  theme_bw()

#coefficient draws

#### combine predictions and r_draws ####
predgrid <- expand.grid(
  Project_na = unique(bug_data$Project_na),
  Type = unique(bug_data$Type)
)%>%
  mutate(Season="Spring")


preds_combined <- bind_rows(
  as.data.frame(predict_response(m_cal_v1g, terms = predgrid))%>%
    mutate(sp="Calanoida"),
  as.data.frame(predict_response(m_clad_v1, terms = predgrid))%>%
    mutate(sp="Cladocera"),
  as.data.frame(predict_response(m_cyc_v1, terms = predgrid))%>%
    mutate(sp="Cyclopoida")
)

r_combined <- bind_rows(
  cal_r_draws%>%
    mutate(sp="Calanoida",r_type="Year")%>%
    rename(r=r_year,
           sd=sd_year__Intercept),
  cal_r_draws2%>%
    mutate(sp="Calanoida",r_type="Source")%>%
    rename(r=r_Source,
           sd=sd_Source__Intercept),
  clad_r_draws%>%
    mutate(sp="Cladocera",r_type="Year")%>%
    rename(r=r_year,
           sd=sd_year__Intercept),
  clad_r_draws2%>%
    mutate(sp="Cladocera",r_type="Source")%>%
    rename(r=r_Source,
           sd=sd_Source__Intercept),
  cyc_r_draws%>%
    mutate(sp="Cyclopoida",r_type="Year")%>%
    rename(r=r_year,
           sd=sd_year__Intercept),
  cyc_r_draws2%>%
    mutate(sp="Cyclopoida",r_type="Source")%>%
    rename(r=r_Source,
           sd=sd_Source__Intercept)
)%>%
  gather("var","category",c(1,9))

b_combined <- dplyr::bind_rows(
    get_coef_draws(m_cal_v1g, "Calanoida"),
    get_coef_draws(m_clad_v1, "Cladocera"),
    get_coef_draws(m_cyc_v1, "Cyclopoida"))

b_summ <- b_combined %>%
  dplyr::group_by(model, term) %>%
  tidybayes::median_qi(draw, .width = c(0.5, 0.8, 0.95)) %>%
  dplyr::ungroup()


#Compute the overlap flag using the 80% interval
overlap_80 <- b_summ %>%
  filter(.width == 0.80) %>%
  transmute(
    model, term,
    overlaps0_80 = (.lower <= 0 & .upper >= 0)
  )

# Join the flag back to all widths
b_summ <- b_summ %>%
  left_join(overlap_80, by = c("model", "term")) %>%
  mutate(
    # Color group: gray if overlap, else use the model name
    color_grp = ifelse(overlaps0_80, "Overlaps 0", model)
  )



#### combined plots ####
ggplot(data=r_combined%>%
         filter(!is.na(category)),
       aes(x = r, y = category)) +
  stat_halfeye(aes(group=sp,color=sp),alpha=0.7,position=position_dodge())+
  #stat_halfeye(aes(group=category,color=category),alpha=0.5,position=position_dodge())+
  facet_wrap(r_type~sp,scales="free")+
  theme_bw()

ggplot(preds_combined,aes(x=x,y=predicted,color=group),group=group)+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),position=position_dodge(),width=0.5)+
  geom_point(position=position_dodge(width=0.5))+
  labs(x="Region")+
  facet_grid(sp~.,scales="free_y")+
  theme_bw()

ggplot(preds_combined%>%
         filter(x!="Blacklock",
                x!="Bradmoor"),aes(x=x,y=predicted,color=group),group=group)+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),position=position_dodge(),width=0.5)+
  geom_point(position=position_dodge(width=0.5))+
  labs(x="Region")+
  facet_grid(sp~.,scales="free_y")+
  theme_bw()




#Plot: point + multi-width intervals, dodged by model
ggplot(b_summ%>%
         filter(term%in%c("TypeOutside_1k","seaWinter",
                          "seaSpring","seaSummer")), aes(x = draw, y = term, color = color_grp)) +
  # 95% interval (light)
  geom_errorbarh(
    aes(xmin = .lower, xmax = .upper),
    height = 0, alpha = 0.4,
    data = dplyr::filter(b_summ%>%
                           filter(term%in%c("TypeOutside_1k","seaWinter",
                                            "seaSpring","seaSummer")), .width == 0.95)) +
  # 80% interval (medium)
  geom_errorbarh(
    aes(xmin = .lower, xmax = .upper),
    height = 0, alpha = 0.7,linewidth=1,
    data = dplyr::filter(b_summ%>%
                           filter(term%in%c("TypeOutside_1k","seaWinter",
                                            "seaSpring","seaSummer")), .width == 0.80)) +
  # Median point (dark)
  geom_point(
    data = dplyr::filter(b_summ%>%
                           filter(term%in%c("TypeOutside_1k","seaWinter",
                                            "seaSpring","seaSummer")), .width == 0.50)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  facet_grid(model~.)+
  scale_color_manual(values=c("red","blue","gold","gray50"))+
  labs(x = "Coefficient estimate with 80%/95% cred int",
    y = "Term",
    color = "Model") +
  theme_bw(base_size = 12)


ggplot(b_summ%>%
         filter(model%in%c("Calanoida")), aes(x = draw, y = term, color = color_grp)) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),height = 0, alpha = 0.4,
    data = dplyr::filter(b_summ%>%
                           filter(model%in%c("Calanoida")), .width == 0.95)) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),height = 0, alpha = 0.7,linewidth=1,
    data = dplyr::filter(b_summ%>%
                           filter(model%in%c("Calanoida")), .width == 0.80)) +
  geom_point(data = dplyr::filter(b_summ%>%
                           filter(model%in%c("Calanoida")), .width == 0.50)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  facet_grid(model~.)+
  scale_color_manual(values=c("red","gray50","gold","gray50"))+
  labs(x = "Coefficient estimate with 80%/95% cred int",
       y = "Term",color = "Model") +
  theme_bw(base_size = 12)

#### predictions and plot####
#order of parameters matters. 

#generate higher resolution of predictions as desired
predgrid <- expand.grid(
  Project_na = unique(cal_data$Project_na),
  Type = unique(cal_data$Type)
  )%>%
  mutate(sea="Spring")

pred3 <- predict_response(m_cal3, terms = predgrid)   # preferred for grid control
plot(pred3)  # quick plot

#generate higher resolution of predictions as desired
predgrid_clad <- expand.grid(
  Project_na = unique(clad_data$Project_na),
  Type = unique(clad_data$Type)
)%>%
  mutate(sea="Spring")

pred_clad <- predict_response(m_clad, terms = predgrid_clad)   # preferred for grid control
plot(pred_clad)  # quick plot

preds_combined <- bind_rows(
  as.data.frame(pred3)%>%
    mutate(sp="Calanoida"),
  as.data.frame(pred_clad)%>%
    mutate(sp="Cladocera")
)


ggplot(pred_clad,aes(x=x,y=predicted,color=group),group=group)+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),position=position_dodge(),width=0.5)+
  geom_point(position=position_dodge(width=0.5))+
  labs(x="Region")+
  facet_grid(.~facet)+
  theme_bw()

ggplot(pred3,aes(x=x,y=10^predicted,color=group),group=group)+
  geom_errorbar(aes(ymin=10^conf.low,ymax=10^conf.high),position=position_dodge(),width=0.5)+
  geom_point(position=position_dodge(width=0.5))+
  labs(x="Region",y="Predicted per l^3")+
  facet_grid(.~facet)+
  theme_bw()


ggplot(preds_combined,aes(x=x,y=predicted,color=group),group=group)+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high),position=position_dodge(),width=0.5)+
  geom_point(position=position_dodge(width=0.5))+
  labs(x="Region")+
  facet_grid(sp~.)+
  theme_bw()
