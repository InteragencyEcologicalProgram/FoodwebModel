#wetland bug power analysis
#2026-2-13

#load packages
#data manipulation
library(tidyverse)
#linear models
library(lme4)
library(lmerTest)
library(DHARMa)
library(effects)
#Power
library(simr)
#mapping
library(sf)
library(deltamapr)

#grab wetland bug data

load("data/AllWetlandBugs_2010onwards.RData")

#pick one reference, one restoration site, one open water area control
#Or maybe do this for all the wetlands?

#bring in wetland data and subset to sites we decided to prioritize
load("data/wetlandsites.RData")
prioritysites = filter(allsites, Project_na %in% c("LICB", "Rush Ranch",  "Blacklock",
                                                   "Tule Red", "Liberty", "Wings Landing", "Dutch Sl.",
                                                   "Flyway Farms","Browns",
                                                   "Lower Yolo Ranch", "Winter")) %>%
  arrange(Project_na) %>%
  mutate(Region = c("Suisun","Confluence","Confluence","Confluence","Confluence","Confluence",
                    "Cache Slough","Cache Slough","Cache Slough",
                    "Cache Slough","Suisun",   "Grizzly", "Suisun", "Confluence"),
         site_type = case_when(site_type == "Planned" ~ "Restored",
                               TRUE ~ site_type)) %>%
  filter(Project_na != "Blacklock") %>%
  st_transform(crs = st_crs(WW_Delta))
#eh, maybe drop blacklock?

#now restoration, reference, external site groups
External = filter(WW_Delta, HNAME %in% c("MONTEZUMA SLOUGH", "SUISUN SLOUGH", "GRIZZLY BAY", 
                                         "CACHE SLOUGH", "BROAD SLOUGH", "PROSPECT SLOUGH",
                                         "SACTO. R DEEP WATER SH CHAN", "NEW YORK SLOUGH", "SAN JOAQUIN RIVER")) %>%
  select("Project_na" = HNAME) %>%
  mutate(site_type = "channel", Region = c("Cache Slough", "Cache Slough", "Cache Slough", "Suisun", "Suisun", "Grizzly", "Confluence",
                                           "Confluence","Confluence")) %>%
  bind_rows(prioritysites)
  


#do I have enough samples in those areas?
bugsexternal = Allbugs_Oct2025 %>%
  filter(!is.na(Longitude)) %>%
mutate(Longitude = case_when(Longitude >0 ~ Longitude*-1,
                             TRUE ~ Longitude)) %>%
   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_join(External) %>%
  filter(!is.na(Project_na)) %>%
  st_drop_geometry()

test = filter(Allbugs_Oct2025, Source == "FRP")%>%
   select(Source, SampleID, Latitude, Longitude) %>%
  distinct() %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_join(prioritysites) 

ggplot(External) +
  geom_sf()+
  geom_sf(data = test)

##########################################################################################
#OK, let's do three different analyses 
#1. density of Pseudodiaptomus
#2. density of amphipods
#3. denstiy of anything that might be "fish food" - insects, amphipods, copepods, cladocera, mysids, shrimp


#first up, density of amphipods ###########################################

#add in zeros

amphipods = bugsexternal %>%
  pivot_wider(id_cols = c(Source, Date, Latitude, Longitude, Station, Chlorophyll, Secchi, Temperature, 
                          Conductivity, SalSurf, TowType, SampleID, Year, Month, Chl, BottomDepth, Project_na,
                          site_type, Region, SizeClass),
              names_from = Order, values_from = CPUE, values_fn = sum, values_fill = 0) %>%
  select(Source, Date, Latitude, Longitude, Station, Chlorophyll, Secchi, Temperature, 
         Conductivity, SalSurf, TowType, SampleID, Year, Month, Chl, BottomDepth, Project_na,
         site_type, Region, Amphipoda, SizeClass) %>%
#get rid of mesozooplankton samples that don't include amphipods
filter(!(SizeClass %in% c("Meso", "Micro") & Source %in% c("FMWT", "EMP", "STN", "20mm")))


ggplot(amphipods, aes(x = Region, y = Amphipoda, fill = site_type)) +
  facet_wrap(~Year)+ geom_boxplot()+
  scale_y_log10()
#better!

ggplot(amphipods, aes(x = Source, y = Amphipoda, fill = site_type)) +
  geom_boxplot()+
  scale_y_log10()




ggplot(amphipods, aes(x = Project_na, y = Amphipoda, fill = site_type)) +
  geom_boxplot()+
  scale_y_log10()


#let's just use the Cache SLough and Confluence regions, 2017-2022

amphipods_sub = filter(amphipods, Year %in% c(2017:2022), Region %in% c("Cache Slough", "Confluence")) %>%
  mutate(Month = month(Date),logCPUE = log(Amphipoda+1),
         Season  = case_when(Month %in% c(3,4,5) ~ "Spring",
                             Month %in% c(6,7,8) ~ "Summer",
                             Month %in% c(9,10,11) ~ "Fall",
                             Month %in% c(12,1,2) ~ "Winter"))

#in order to make the power analysis function run correctly, filter out all the cases with missing values.
amphipods_subb = filter(amphipods_sub, !is.na(site_type), !is.na(Region), !is.na(Year), !is.na(Season))

#exploritory plots
ggplot(amphipods_sub, aes(x = Project_na, y = logCPUE))+
  facet_wrap(~Year)+ geom_boxplot()

#we have a lot of zeros, and a lot of variation by sampling group. Might make it hard to model
ggplot(amphipods_sub, aes(x = Source, y = logCPUE, fill = site_type)) +
  geom_boxplot()

#######################################################################################
#now start modeling
#Our basic linear model will be testing the difference in CPUE between site types 
#while controlling for region, season, and year.
#For example:



myrNB = lmer(logCPUE ~ site_type + Region  + Season+ (1|Year) + (1|Project_na),  
                 data = amphipods_subb, control = lmerControl(check.conv.singular = "ignore"))
summary(myrNB)

plot(simulateResiduals(myrNB))
testZeroInflation(myrNB)
#bleh, kinda nasty. Some outliers, not normally distributed, and zero inflated.

#I'm not sure if I should block by tow type, but I'll see if it helps.
myrNB2 = lmer(logCPUE ~ site_type + Region  +TowType+ (1|Season)+ (1|Year) + (1|Project_na),  
             data = amphipods_subb, control = lmerControl(check.conv.singular = "ignore"))
summary(myrNB2)
plot(simulateResiduals(myrNB2))
#that's maybe a little better? Still pretty gross
#there is obviously some more work to be done to figure out the right model framework that allows 
#you to look at amphipods, maybe an addative model or bayesian approach.
#but the basic theory remains the same. We need to change the effect size to one
#that we want to be able to pick up, and then resample the data a bunch of times
#and see how many of the models are within p<0.05 (or where credible intervals don't
#overlap, or wahtever your metric of success is.)

#I'll just run with the first model, even though it isn't a great fit.
#First change the effect size of the relevant factor to whatever you want to use for the power analysis
fixef(myrNB)["site_typeRestored"] =  .1
fixef(myrNB)["site_typeReference"] =  .1

#this is a basic power analysis. You probably want more like 1000 simulations, but it takes
#a long time to run, so I'll start with 100
#and we specifically want to look at the restoration effect, not the other model terms
x = powerSim(myrNB,  nsim=100, test = fixed("site_typeRestored", "t"))
x

#Now we extend the dataset artificially to have at least 500 samples in each season+project+year combination
modexa = extend(myrNB, within = "Project_na + Year", n = 500)

#Now we run the same power analysis from above, but with different levels of replication. 
pAa = powerCurve(modexa, 
                 within = "Project_na+ Year",  
                 breaks = c(10, 20,50,100,200, 500), nsim =100,
                 test = fixed("site_typeRestored", "t"))

#small effect size, low power
pAa
plot(pAa)
#huh, why is this not working!?
#I dthink it's because if Project is a random effect I need more projects
#in order to actually increase my sample size. 

#let's just look for differences bewteen sites, within one region for simiplicities sake?

amphipods_subb_cache = filter(amphipods_subb, Region == "Cache Slough")

myrNB_s = lmer(logCPUE ~ Project_na +  Season+ (1|Year)+ (1|SampleID),  
             data = amphipods_subb_cache, control = lmerControl(check.conv.singular = "ignore"))
summary(myrNB_s)

plot(simulateResiduals(myrNB_s))
testZeroInflation(myrNB_s)
#ugh, yeah, gross. 
#We'll probably have to design our own power simulation analysis 
#in order to do this for real, and use a zero-inflated model or hurdle model


#increase effect size
fixef(myrNB_s)["Project_naFlyway Farms"] =  .1
fixef(myrNB_s)["Project_naLiberty"] =  .1
fixef(myrNB_s)["Project_naLower Yolo Ranch"] =  .1
fixef(myrNB_s)["Project_naPROSPECT SLOUGH"] =  0
fixef(myrNB_s)["Project_naSACTO. R DEEP WATER SH CHAN"] =  0
#
modexa = extend(myrNB_s, along = "SampleID", n = 10000)


#power curve
pAa_.1 = powerCurve(modexa,  along = "SampleID",  
                    breaks = c(10, 50,200, 500, 1000, 5000), progress = T, nsim =100)

pAa_.1
#plot(pAa_.1)


#increase effect size
fixef(myrNB_s)["Project_naFlyway Farms"] =  .2
fixef(myrNB_s)["Project_naLiberty"] =  .2
fixef(myrNB_s)["Project_naLower Yolo Ranch"] =  .2
fixef(myrNB_s)["Project_naPROSPECT SLOUGH"] =  0
fixef(myrNB_s)["Project_naSACTO. R DEEP WATER SH CHAN"] =  0
#
modexa = extend(myrNB_s, along = "SampleID", n = 10000)


#power curve
pAa_.2 = powerCurve(modexa,  along = "SampleID",  
                    breaks = c(10, 50,200, 500, 1000, 5000), progress = T, nsim =100)

pAa_.2
#plot(pAa_.2)

#increase effect size more
fixef(myrNB_s)["Project_naFlyway Farms"] =  .5
fixef(myrNB_s)["Project_naLiberty"] =  .5
fixef(myrNB_s)["Project_naLower Yolo Ranch"] =  .5
fixef(myrNB_s)["Project_naPROSPECT SLOUGH"] =  0
fixef(myrNB_s)["Project_naSACTO. R DEEP WATER SH CHAN"] =  0

#modexa is the one with more factors
modexa = extend(myrNB_s, along = "SampleID", n = 10000)

#power curve
pAa_.5 = powerCurve(modexa,  along = "SampleID",  
                    breaks = c(10, 50,200, 500, 1000, 5000), progress = F, nsim =100)


#increase effect size more
#increase effect size more
fixef(myrNB_s)["Project_naFlyway Farms"] =  1
fixef(myrNB_s)["Project_naLiberty"] =  1
fixef(myrNB_s)["Project_naLower Yolo Ranch"] =  1
fixef(myrNB_s)["Project_naPROSPECT SLOUGH"] =  0
fixef(myrNB_s)["Project_naSACTO. R DEEP WATER SH CHAN"] =  0


#modexa is the one with more factors
modexa = extend(myrNB_s,  along = "SampleID", n = 10000)  
                

#power curve
pAa_1 = powerCurve(modexa,  within = "Year + Project_na",  breaks = c(10, 50,200, 500, 1000, 5000), progress = F, nsim =100)
                  



#put all the power curves together and graph them
powers = bind_rows(mutate(summary(pAa_.1), Effect = 0.1),
mutate(summary(pAa_.2), Effect = 0.2),
mutate(summary(pAa_.5), Effect = 0.5),
mutate(summary(pAa_1), Effect = 1))

ggplot(powers, aes(x = nlevels, y = mean, color = as.factor(Effect))) + geom_point() + geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+ylab("Power")+xlab("Total number of samples")+
  theme_bw()+scale_color_manual(values = c("grey30", "blue", "green4", "orange"), name = "effect size")
  
