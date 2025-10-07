#Let's correlate macroinvertebreates with vegetation!!!
library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest)
library(mgcv)
library(emmeans)
library(effects)
library(sf)
library(deltamapr)
library(ggbeeswarm)
library(DHARMa)
library(glmmTMB)

#bring in wetland data and subset to sites we decided to prioritize
load("GIS dta/wetlandsites.RData")
prioritysites = filter(allsites, Project_na %in% c("LICB", "Rush Ranch", "Decker", "Blacklock",
                                                   "Tule Red", "Liberty", "Wings Landing", "Dutch Sl.",
                                                   "Flyway Farms","Browns",
                                                   "Lower Yolo Ranch"))

#buffer by 500 m so we capture data collected outside the site
prioritysitesbuffer = st_buffer(prioritysites, 500)

Allsitesbuffer = st_buffer(allsites, 1000)

#st_write(Allsitesbuffer, "GIS dta/WetlandsBuffered.shp")

#load bug data
load("data/AllWetlandBugs.RData")
names(AllBugs)

#calculate CPUE for sweep nets
AllBugs = AllBugs %>% mutate(Volume = case_when(TowType %in% c("SN", "EAV", "SAV", "FAV") ~ 0.3,
                                     TRUE ~ Volume),
                  CPUE = case_when(TowType %in% c("SN", "EAV", "SAV", "FAV") ~ CPUE/0.3,
                                   TRUE ~ CPUE))

#some of the logitude values were off, fix those and join in project name
AllBugsx = mutate(AllBugs, Longitude = case_when(Longitude >1 ~ Longitude *-1,
                                                TRUE ~ Longitude))%>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)  %>%
  st_join(prioritysitesbuffer) 


#got to add in zeros and add vegetation type
Taxonomy = zooper::crosswalk %>%
  select(Taxname, Phylum, Class, Order, Family, Genus, Species) %>%
  distinct()

AllbugsWetlands = AllBugsx %>%
  st_drop_geometry() %>%
  filter(!is.na(Project_na), SizeClass != "Micro",
         !(Source == "YBFMP" & Project_na == "Lower Yolo Ranch")) %>% #keep YBFMP dta with Flyway, not lower yolo
  
  mutate( VegType = case_when(str_detect(SampleID, "FAV") ~ "Floating",
                              str_detect(SampleID, "EAV") ~ "Emergent",
                              str_detect(SampleID, "SAV") ~ "Submersed",
                              TowType %in% c("PPG", "PVC", "Ponar") ~ "Benthic",
                              TowType %in% c("NT", "Neuston") ~ "Neuston/drift",
                              TRUE ~ "Plankton")) %>%
  pivot_wider(id_cols = c(Source, Date, Latitude, Longitude, Station, SalSurf, TowType, SampleID, Year, Month, 
                          Project_na, site_type, VegType), names_from = Taxname, values_from = CPUE, values_fn = sum,
              values_fill = 0) %>%
  pivot_longer(cols = c("Copepoda":last_col()), names_to = "Taxname", values_to = "CPUE") %>%
  left_join(Taxonomy) %>%
  filter(!(Source == "Dutch Slough" & CPUE ==0)) #dutch slough didn't look at most stuff, so don't include all the zeros


#let's start by just looking at amphipods
Amphs_wetlands = filter(AllbugsWetlands, Order == "Amphipoda")
AmphFRP = filter(Amphs_wetlands, Source == "FRP")

#one point for each samples so i can map it
AllBugsSamples = mutate(AllBugs, Longitude = case_when(Longitude >1 ~ Longitude *-1,
                                                       TRUE ~ Longitude))%>%
  group_by(Latitude, Longitude, TowType) %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)  %>%
  st_join(prioritysitesbuffer)%>%
  filter(!is.na(Project_na), TowType != "Vertical Pump", !(Source == "YBFMP" & Project_na == "Lower Yolo Ranch"))

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = prioritysitesbuffer)+
  geom_sf(data = AllBugsSamples, aes(color = TowType))+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38, 38.4))

#collapse the taxa into smaller categories
Amphs_wetlands = mutate(Amphs_wetlands,
                        AmpType = case_match(Taxname, c("Americorophium", "Americorophium spinicorne", "Americorophium stimpsoni", "Corophiidae", "Sinocorophium alienense", "Monocorophium acherusicum") ~"Corophiid",
                                             "Crangonyx" ~ "Crangonyx",
                                             c("Gammarus", "Eogammarus","Gammaridae", "Gammarus daiberi") ~ "Gammarid",
                                             "Hyalella" ~ "Hyalella",
                                             c("Grandidierella","Grandidierella japonica") ~ "Grandidierella",
                                               .default = "Other Amphipod"),
                        AmpType2 = case_match(Taxname, c("Americorophium", "Americorophium spinicorne", "Americorophium stimpsoni", "Corophiidae", "Sinocorophium alienense", "Monocorophium acherusicum") ~"Corophiid",
                                             c("Gammarus", "Eogammarus","Gammaridae", "Gammarus daiberi","Crangonyx") ~ "Gammarid",
                                             "Hyalella" ~ "Hyalella",
                                             .default = "Other Amphipod")) %>%
  filter(VegType != "Benthic", !is.na(Date)) %>% #remove benthic data because it was just clams
  mutate(DOY = yday(Date), Month = month(Date), Season = case_when(Month %in% c(3,4,5) ~ "Spring",
                                                                     Month %in% c(6,7,8) ~ "Summer",
                                                                     Month %in% c(9,10,11) ~ "Fall",
                                                                     Month %in% c(12,1,2) ~ "Winter"))

ggplot(Amphs_wetlands, aes(x = VegType, y = CPUE)) +
  facet_grid(AmpType~Project_na)+ geom_boxplot()+
  scale_y_log10()

#summarize by amphipod category
Ampsum = group_by(Amphs_wetlands, SampleID, Source, Date, VegType, AmpType, Project_na, SalSurf, Year, Season) %>%
  summarise(CPUE = sum(CPUE, na.tm =T))

ggplot(Ampsum, aes(x = VegType, y = CPUE)) +
  facet_wrap(~AmpType)+ geom_boxplot()+
  scale_y_log10()

ggplot(Ampsum, aes(x = VegType, y = CPUE)) +
  facet_grid(Project_na~AmpType)+ geom_boxplot()+
  scale_y_log10()

#what are the main drieers of abundance?
#first guess is salinity, vegetation type, and season
#I don't think year is as important, but do I need it as a random effect?
ampmod1 = glmmTMB(CPUE ~ VegType*AmpType + SalSurf*AmpType +Season+ (1|Year), 
                  family = "nbinom2", ziformula = ~SalSurf, 
                  data = Ampsum)
summary(ampmod1)
plot(allEffects(ampmod1))
plot(simulateResiduals(ampmod1))
#hmmmm.... one critter at a time?
#or just corophiids and gammarids?

#summarize by larger categoreies
Ampsum2 = group_by(Amphs_wetlands, SampleID, Source, Date, VegType, AmpType2, Project_na, SalSurf, Year, Season) %>%
  summarise(CPUE = sum(CPUE, na.tm =T)) 

ampmod2 = glmmTMB(CPUE~ VegType*AmpType2+ SalSurf*AmpType2 +Season+ (1|Year), 
               ziformula = ~ SalSurf + VegType, family = "nbinom2", data = Ampsum2)
summary(ampmod2)
plot(allEffects(ampmod2))

#diagnostics. It's gross looking
plot(simulateResiduals(ampmod2))
testOutliers(ampmod2, type = "bootstrap")

#what happens without the effect of year?

ampmod2.1 = glmmTMB(CPUE~ VegType*AmpType2+ SalSurf*AmpType2 +Season, 
                  ziformula = ~ SalSurf*AmpType2, family = "nbinom2", data = Ampsum2)

summary(ampmod2.1)
plot(allEffects(ampmod2.1))

#diagnostics. That's even more gross
plot(simulateResiduals(ampmod2.1))


#let's just look at total amphipods now. 
Ampsum3 = group_by(Amphs_wetlands, SampleID, Source, Date, VegType,  Project_na, SalSurf, Year, Season) %>%
  summarise(CPUE = sum(CPUE, na.tm =T))

ggplot(filter(Ampsum3, VegType != "Plankton"), aes(x = yday(Date), fill = VegType))+ geom_histogram() 

ggplot(filter(Ampsum3, VegType != "Plankton", yday(Date) %in% c(0:150)),
       aes(x = yday(Date), y = SalSurf, color = Project_na))+ geom_point() 

ggplot(filter(Ampsum3, VegType != "Plankton", yday(Date) %in% c(200:550)),
       aes(x = yday(Date), y = SalSurf, color = Project_na))+ geom_point() 

#model 
ampmod3 = glmmTMB(CPUE ~ VegType+SalSurf+Season,
                  ziformula = ~VegType + SalSurf, data = Ampsum3, family = "nbinom2")
summary(ampmod3)
plot(allEffects(ampmod3))
plot(simulateResiduals(ampmod3))
#gross, gross, gross


#Let's try just looking at samples in vegetation
ampmod3veg = glmmTMB(CPUE ~ VegType+SalSurf+Season ,
                  ziformula = ~VegType +SalSurf, 
                  data = filter(Ampsum3, VegType %in% c("Emergent", "Floating", "Submersed")), 
                  family = "nbinom2")
summary(ampmod3veg)
plot(allEffects(ampmod3veg, partial.residuals =T))
plot(simulateResiduals(ampmod3veg))
#much, much better. 

#but i'm really surprised winter is higher. that's odd.
ggplot(Ampsum3, aes(x = Season, y = CPUE)) +
  facet_wrap(~VegType)+ geom_boxplot()+
  scale_y_log10()

seasons = Ampsum3 %>%
  group_by(VegType, Season) %>%
  summarize(Mean = mean(CPUE), SD = sd(CPUE), n = n())
View(seasons)

#so we only have two samples in winter for submersed veg, only 6 for floating veg, 16 for emergent. 
#Pretty unbalanced. Maybe I lump fall and summer with spring and winter?
Ampsum3 = mutate(Ampsum3, Semester = case_when(Season %in% c("Winter", "Spring") ~ "Wet",
                                               Season %in% c("Summer", "Fall") ~ "Dry"))


#the zero inflation term never comes out as significant, so might not need it for all amphipods
ampmod3vegb = glmmTMB(CPUE ~ VegType+SalSurf+Semester + (1|Year),
                     data = filter(Ampsum3, VegType %in% c("Emergent", "Floating", "Submersed")), 
                     family = "nbinom2")
summary(ampmod3vegb)
plot(allEffects(ampmod3vegb, partial.residuals =T))
plot(simulateResiduals(ampmod3vegb))

#remove random effect of year and see what happens
ampmod3vegc = glmmTMB(CPUE ~ VegType+SalSurf+Semester,
                      data = filter(Ampsum3, VegType %in% c("Emergent", "Floating", "Submersed")), 
                      family = "nbinom2")
summary(ampmod3vegc)
plot(allEffects(ampmod3vegc, partial.residuals =T))
plot(simulateResiduals(ampmod3vegc))
#not great

#use this model to predict density in each veg type in each wetland (spring only)



ggplot(Ampsum3, aes(x = log(SalSurf), y = log(CPUE+1))) +
  facet_wrap(~VegType)+ geom_point()+ geom_smooth(method = "lm")#+
 # scale_y_log10()

ggplot(filter(Ampsum3, Year >2015), aes(x = as.factor(Year), y = log(CPUE+1))) +
  facet_wrap(~VegType)+ geom_boxplot()
# scale_y_log10()


ggplot(Ampsum3, aes(x = VegType, y = CPUE)) +
  facet_wrap(~Project_na)+ geom_boxplot()+
  scale_y_log10()

ggplot(Ampsum3, aes(x = Date, y = CPUE, color = VegType)) +
  facet_wrap(~Project_na, scales = "free")+ geom_point()+ geom_smooth(se = F)+
  scale_y_log10()

#Try going back to where we have different categorie sof ampihpods
Ampsum2 = mutate(Ampsum2, DOY = yday(Date), Month = month(Date), Season = case_when(Month %in% c(3,4,5) ~ "Spring",
                                                                                    Month %in% c(6,7,8) ~ "Summer",
                                                                                    Month %in% c(9,10,11) ~ "Fall",
                                                                                    Month %in% c(12,1,2) ~ "Winter"))


ampmod2b = lmer(log(CPUE+1) ~ VegType*AmpType2+ SalSurf*AmpType2 + Month +(1|Year), data = Ampsum2)
summary(ampmod2b)
plot(allEffects(ampmod2b))

#just look at hyalella
hyalella = filter(Ampsum2, AmpType2 == "Hyalella")

hyalellamod = lmer(log(CPUE+1) ~ VegType+ SalSurf + I(Month^2) +(1|Year), data = hyalella)
summary(hyalellamod)
plot(simulateResiduals(hyalellamod))
plot(allEffects(hyalellamod))
testZeroInflation(hyalellamod)

ggplot( filter(Ampsum2, AmpType2 == "Hyalella"), aes(x = DOY, y = CPUE))+geom_point()+
  facet_wrap(~VegType)+ scale_y_log10()+ geom_smooth()
#I think we need a zero inflation model for Hyalella

hymod2 = glmmTMB(CPUE ~  VegType+ SalSurf + Season +(1|Year), family = "nbinom2",
                 ziformula = ~SalSurf+ VegType,
                 data = filter(Ampsum2, AmpType2 == "Hyalella"))

summary(hymod2)
plot(simulateResiduals(hymod2))
plot(allEffects(hymod2))
#stilllooks gross


#I think we'll have to do vegetation seperate from open water
hyalellaX = filter(Ampsum2, AmpType2 == "Hyalella", VegType %in% c("Emergent", "Floating", "Submersed"))

hymod3 = glmmTMB(CPUE ~  VegType+ SalSurf + Season +(1|Year), family = "nbinom2",
                 ziformula = ~SalSurf+ VegType,
                 data = hyalellaX)

summary(hymod3)
plot(simulateResiduals(hymod3))
plot(allEffects(hymod3))

#do I need a sperate model for open water? Maybe
#and that one outlier is really throwing stuff off

hyalellaY = filter(Ampsum2, AmpType2 == "Hyalella", VegType == "Plankton") %>%
  filter(CPUE <200)

hymod4 = glmmTMB(CPUE ~  SalSurf + Season +(1|Year), family = "nbinom2",
                 ziformula = ~SalSurf,
                 data = hyalellaY)

summary(hymod4)
plot(simulateResiduals(hymod4))
plot(allEffects(hymod4))

ggplot(hyalellaY, aes(x = DOY, y = CPUE)) + geom_point()

#uh, this makes no sense. 

#########################################################################################
#larger categories of bugs ############################
bugcats = read.csv("bugcats.csv")


BugsClass = AllbugsWetlands %>%
  left_join(bugcats) %>%
  filter(Category != "") %>%
  group_by(Category, SampleID, Project_na, VegType, Date, Latitude, Longitude, SalSurf) %>%
  summarize(CPUE = sum(CPUE)) %>%
  mutate(Month = month(Date),
         Season = case_when(Month %in% c(12,1,2) ~ "Winter",
                            Month %in% c(3,4,5)~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall"))

ggplot(BugsClass, aes(x = VegType, y = CPUE, fill = VegType))+ geom_boxplot() + facet_wrap(~Category)+ scale_y_log10()+
  xlab(NULL)+ theme_bw()+
  theme(axis.text.x = element_blank())

ggplot(BugsClass, aes(x = Season, y = log(CPUE+1)))+ 
  geom_quasirandom(aes(color = Category))+
  facet_grid(VegType~Project_na)


######################################
#just look at SAV###########################


SAVbugs = filter(BugsClass, VegType == "Submersed") 

ggplot(SAVbugs, aes(x = Date, y = CPUE, color = Category))+ geom_point()+
  facet_wrap(~Project_na)

ggplot(SAVbugs, aes(x = SalSurf, y = CPUE, color = Category))+ geom_point()+
  facet_wrap(~Project_na)

ggplot(SAVbugs, aes(x = Season, y = log(CPUE+1)))+ 
  geom_boxplot()+geom_point(aes(color = Category))+
  facet_wrap(~Project_na)

ggplot(SAVbugs, aes(x = Season, y = log(CPUE+1)))+ 
  geom_quasirandom(aes(color = Category))+
  facet_wrap(~Project_na)

#########################################
#productivity of SAV ###################

savproduction = read_excel("data/Boyer2023/Data/AV_productivity_rates_lit_values_tidy_20220216.xlsx")
egeria = filter(savproduction, species == "Egeria densa")
names(egeria)

ggplot(egeria, aes(x = 1, y = productivity_365_days_gCm2)) + geom_boxplot()
#averages 500 g/m2/year

###############################################
#bring in coverage of vegetation types and scale up ################

AllvegForReal = read_csv("data/AllvegAreas.csv")

unique(AllvegForReal$Project_na)

unique(BugsClass$Project_na)
unique(BugsClass$VegType)

#let's look at sprign of 2018
Allveg2018 = filter(AllvegForReal, Year ==2018) %>%
  mutate(VegType2 = case_when(VegType %in% c("Primrose", "Hyacinth", "Alligatorweed", "Spongeplant") ~ "Floating",
                              VegType %in% c("Marsh Invaded by Primrose") ~ "Emergent",
                              VegType %in% c("SAV") ~ "Submersed",
                              VegType %in% c("water") ~ "Plankton",
                              TRUE ~ VegType), Year = as.numeric(Year)) %>%
  select(-VegType) %>%
  rename(VegType = VegType2) %>%
  group_by(VegType, Project_na) %>%
  summarise(Area = sum(Area))

Spring2018Bugs = filter(BugsClass, year(Date)==2018) %>%
  mutate(Year = year(Date)) %>%
  group_by(Project_na, VegType, Category) %>%
  summarise(CPUEm = mean(CPUE), sdCPUE = sd(CPUE))

VegAndBugs = Allveg2018 %>%
  left_join(Spring2018Bugs)

#I needt to turn volume into area
# 5, 1-m sweeps for EAV and SAV, net is 30cm x 20 cm, so 0.3 m2
# Assume water column is 1 m deep
# 0.3*5*1/0.2 = 1.5 m2
# CPV*5/1.5 = CPM2

#but maybe we just assume all the water is 1 m deep?
BugsperVeg = VegAndBugs %>%
  mutate(Bugs = Area*CPUEm, sdBugs = Area*sdCPUE)

ggplot(filter(BugsperVeg,Category == "Amphipoda"), aes(x = Project_na, y = Bugs, fill = VegType, group = VegType))+
  geom_col(position = "dodge")+ geom_errorbar(aes(ymin = Bugs-sdBugs, ymax = Bugs+sdBugs), position = "dodge")+
  ylab("Total Number of amphipods per site")

ggplot(filter(BugsperVeg,Category == "Amphipoda"), aes(x = Project_na, y = CPUEm, fill = VegType, group = VegType))+
  geom_col(position = "dodge")+ geom_errorbar(aes(ymin = CPUEm-sdCPUE, ymax = CPUEm+sdCPUE), position = "dodge")+
  ylab("Mean density of amphipods per site")

##################################################################
#sources of variation ##############################

#Do we create oen value for all times/spaces in terms of amphipod density? Or make it reagional and/or seasonal?

AllvegZ = AllvegForReal %>%
  mutate(VegType2 = case_when(VegType %in% c("Primrose", "Hyacinth", "Alligatorweed", "Spongeplant") ~ "Floating",
                              VegType %in% c("Marsh Invaded by Primrose") ~ "Emergent",
                              VegType %in% c("SAV") ~ "Submersed",
                              VegType %in% c("water") ~ "Plankton",
                              TRUE ~ VegType), Year = as.numeric(Year)) %>%
  select(-VegType) %>%
  rename(VegType = VegType2) %>%
  group_by(VegType, Project_na, Year) %>%
  summarise(Area = sum(Area))

BugsZ = BugsClass %>%
  mutate(Year = year(Date)) %>%
  group_by(Project_na, VegType, Category, Year) %>%
  summarise(CPUEm = mean(CPUE), sdCPUE = sd(CPUE), seCPUE= sdCPUE/sqrt(n()))

VegAndBugsZ = AllvegZ %>%
  left_join(BugsZ)

#I needt to turn volume into area
# 5, 1-m sweeps for EAV and SAV, net is 30cm x 20 cm, so 0.3 m2
# Assume water column is 1 m deep
# 0.3*5*1/0.2 = 1.5 m2
# CPV*5/1.5 = CPM2

#but maybe we just assume all the water is 1 m deep?
BugsperVegZ = VegAndBugsZ %>%
  mutate(Bugs = Area*CPUEm, seBugs = Area*seCPUE) %>%
  filter(!VegType %in% c("NPV", "Riparian", "Shadow", "soil", "unclassified"), Category == "Amphipoda")

ggplot(BugsperVegZ, aes(x = Year, y = CPUEm, fill =Project_na))+ geom_col(position = "dodge")+
  facet_wrap(~VegType, scales = "free_y")+ 
  geom_errorbar(aes(ymin = CPUEm-seCPUE, ymax = CPUEm+seCPUE), position = "dodge")+
  ylab("Mean density of amphipods per site")#+scale_y_log10()

ggplot(BugsperVegZ, aes(x = Year, y = Bugs, fill =Project_na))+ geom_col(position = "dodge")+
  facet_wrap(~VegType, scales = "free_y")+ 
  geom_errorbar(aes(ymin = Bugs-seBugs, ymax = Bugs+seBugs), position = "dodge")+
  ylab("total abundance of amphipods per site")+scale_y_log10()

#now do it by site, all years together


BugsZ2 = BugsClass %>%
  mutate(Year = year(Date)) %>%
  group_by(Project_na, VegType, Category, Year) %>%
  summarise(CPUEm = mean(CPUE), sdCPUE = sd(CPUE))%>%
  group_by(Project_na, VegType, Category) %>%
  summarise(CPUEm2 = mean(CPUEm), sdCPUE = sd(CPUEm), seCPUE = sdCPUE/sqrt(n()))

VegAndBugsZ2 = AllvegZ %>%
  left_join(BugsZ2)

BugsperVegZ2 = VegAndBugsZ2 %>%
  mutate(Bugs = Area*CPUEm2, seBugs = Area*seCPUE) %>%
  filter(!VegType %in% c("NPV", "Riparian", "Shadow", "soil", "unclassified"), Category == "Amphipoda")

ggplot(filter(BugsperVegZ2, Year ==2019), aes(x = Project_na, y = CPUEm2, fill =Project_na))+ geom_col(position = "dodge")+
  facet_wrap(~VegType, scales = "free_y")+ 
  geom_errorbar(aes(ymin = CPUEm2-seCPUE, ymax = CPUEm2+seCPUE), position = "dodge")+
  ylab("Mean density of amphipods per site")#+scale_y_log10()

#scale up per area of vegetation
ggplot(BugsperVegZ2, aes(x = Year, y = Bugs, fill =Project_na))+ geom_col(position = "dodge")+
  facet_wrap(~VegType, scales = "free_y")+ 
  geom_errorbar(aes(ymin = Bugs-seBugs, ymax = Bugs+seBugs), position = "dodge")+
  ylab("Total number of amphipods per site")+scale_y_log10()

#now use my favorite model - ampmod3vegb

#what's the average salinity in the spring by site and year?

Salmean = AllbugsWetlands %>%
  st_drop_geometry() %>%
  filter(month(Date) %in% c(3:5)) %>%
  group_by(Project_na) %>%
  group_by(Year, Project_na) %>%
    summarize(SalSurf = mean(SalSurf, na.rm =T))

Newdat = Salmean %>%
  merge(data.frame(VegType = c("Floating", "Submersed", "Emergent"))) %>%
  merge(data.frame(Semester = c("Wet", "Dry"))) %>%
  filter(!is.nan(SalSurf))

#predictions based on th model
predictions = predict(ampmod3vegc, newdata = Newdat, se.fit = T)
Newdat = mutate(Newdat, PredictCPUE = predictions$fit,  PredictSD = predictions$se.fit) %>%
  left_join(AllvegZ) %>%
  mutate(Bugs = Area*PredictCPUE, sdBugs = Area*PredictSD) %>%
  filter(!VegType %in% c("NPV", "Riparian", "Shadow", "soil", "unclassified"))

ggplot(filter(Newdat, Year > 2015, Semester == "Wet"), aes(x = Year, y = PredictCPUE, fill =Project_na))+ geom_col(position = "dodge")+
  facet_wrap(~VegType, scales = "free_y")+ 
  geom_errorbar(aes(ymin = PredictCPUE-PredictSD, ymax = PredictCPUE+PredictSD), position = "dodge")+
  ylab("Mean density of amphipods per site")#+scale_y_log10()

ggplot(filter(Newdat, Year > 2015, Semester == "Wet"), aes(x = Year, y = Bugs, fill =Project_na))+ 
  geom_col(position = "dodge")+
  facet_wrap(~VegType, scales = "free_y")+ 
  geom_errorbar(aes(ymin = Bugs-sdBugs, ymax = Bugs+sdBugs), position = "dodge")+
  ylab("Total number of amphipods per site")+scale_y_log10()

