
library(tidyverse)
library(sf)
library(glmmTMB)
library(lme4)
library(DHARMa)

#####take another tact ######################################
#annual vegetation turnorver gets converted into detritus, scale up.

#look at one particular site?

AllvegForReal = read_csv("data/AllvegAreas.csv")

unique(AllvegForReal$Project_na)
#let's look at sprign of 2018
Libveg2018 = filter(AllvegForReal, Year ==2018, Project_na == "Liberty") %>%
  mutate(VegType2 = case_when(VegType %in% c("Primrose", "Hyacinth", "Alligatorweed", "Spongeplant") ~ "Floating",
                              VegType %in% c("Marsh Invaded by Primrose") ~ "Emergent",
                              VegType %in% c("SAV") ~ "Submersed",
                              VegType %in% c("water") ~ "Plankton",
                              TRUE ~ VegType), Year = as.numeric(Year)) %>%
  select(-VegType) %>%
  rename(VegType = VegType2) %>%
  group_by(VegType, Project_na) %>%
  summarise(Area = sum(Area))

#Rates for benthic microalgae on shaded sediments (beneath the plant canopy) were 
#estimated based on a light attenuation by emergent vascular plants using the factor f, 
#the fraction of light above plant canopies that reaches the sediment surface. The light 
#factor f has not been systematically measured in Delta tidal marshes, but it has in 
#reshwater marshes of the central US with similar plant communities (Williams et al., 2017). 
#We used the mean value of = 0.68 from light profiles measured in 25 tule (Schoenoplectus acutus) 
#stands of those freshwater marshes (Table 4, Williams et al., 2017). Thus, the literature NPP rates 
#for benthic microalgae on non-shaded sediments was multiplied by 0.68 to estimate NPP of benthic, 
#shaded sediments. Similarly, NPP estimates for sediments submerged by 0 – 1 m of water were obtained 
#using f = 0.05. For sediments submerged by 1 – 2 m of water, we used f = 0.01. For epiphytic algae we assumed no 
#light attenuation (f = 1).#


#The peak aboveground dry biomass measurements were converted to marsh carbon production rates by multiplying by 0.441, 
#the mean plant organic carbon content for marsh vegetation across six regions of the United States 
#(n = 1384, 95% C.I. = 43.99%–44.37%; Byrd et al., 2018). 

#emergent vegetation - 1633 g/m2 - miller and fugi
#byrd et al 2017 - mean 410 g/m2, min 78, max 1559

#is peak biomass appropriate?

byrdplots = read_csv("data/byrd et al 2020 biomass data.csv") %>%
  filter(sentinelsite == "SFBay",  sp1 %in% c("Typha domingensis", "Schoenoplectus acutus", "Schoenoplectus californicus",
                                              "Typha angustifolia", "Typha spp.", "Phragmites australis"))

byrdremote = read_csv("data/byrd et al 2020 biomass remote.csv") %>%
  filter(sentinelsite == "SFBay", sp1 %in% c("Typha domingensis", "Schoenoplectus acutus", "Schoenoplectus californicus",
                                             "Typha angustifolia", "Typha spp.", "Phragmites australis")) %>%
  mutate(Date = mdy(sampledate)) 

byrdsum = group_by(byrdplots, sp1) %>%
  summarize(biomass = mean(biomass_gm))
#phragmites has a lot more biomass than other things. 

byrdsum2 = group_by(byrdremote, sp1) %>%
  summarize(biomass = mean(biomassavg))


#hmmm, our veg data is just "emergent" right now, need to see if we can do better than that. 

#OK, for your average amphipod, we've got about 15% assymilation effeciency on benthic algae (hargrave).
#consumption rates Vary. SAV about 50% of algae, bigger diffdrence when given a choice (Scriber 2013)


#So, for Liberty, we've got 3157259 m2 emergemnt vegetation, mostly tules and cattails
#
Libveg2018

mean(filter(byrdremote, sp1 != "Phragmites australis")$biomassavg)
#1240

#So we've got an emergent vegetation biomass of about 
3157259*1240
#3915001 kg

#how many amphipods is that?



#just look at hyalella
load("data/amphipods.RData")
hyalella = filter(Ampsum2, AmpType2 == "Hyalella")

#I think we'll have to do vegetation seperate from open water
hyalellaX = filter(Ampsum2, AmpType2 == "Hyalella", VegType %in% c("Emergent", "Floating", "Submersed"))

hymod3 = glmmTMB(CPUE ~  VegType+ SalSurf + Season +(1|Year), family = "nbinom2",
                 ziformula = ~SalSurf+ VegType,
                 data = hyalellaX)

summary(hymod3)
plot(simulateResiduals(hymod3))
plot(allEffects(hymod3))

#using the model we get
newdat = data.frame(VegType = "Emergent", SalSurf = 0.1, Season = "Summer", Year = 2018)
predict(hymod3, newdata = newdat)
#6.6 hyalella per cubic meter. 
#assuming emergetn marsh is about 0.5m deep
3157259/2*6.6
10418955 #hyalella individuals

#what's the average biomass of a hyalella?

#just take the summary of the actual data

HyLIB18 =  filter(Ampsum2, AmpType2 == "Hyalella", VegType =="Emergent", Project_na == "Liberty")

mean(HyLIB18$CPUE)
#hm. mean CPUE is 42.6 hyalellas
#oh, but that's because one sample  has over 1000

exp(mean(log(HyLIB18$CPUE+1)))
#geometric mean is only 5.17, much closer to the model. 

#how do I figure out the average weight of a hyalella? ##################
#ugh, emp doesn't publish amphipod lengths any more 

#old version of macro length data from EMP
maclength  <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/522/11/43455fa3be9f34fa745280a292801b7a")
unique(maclength$SpeciesName)

hylength = filter(maclength, SpeciesName == "Hyalella sp")
conversions = zooper::biomass_macro
#convert to biomass
hylength = mutate(hylength, Biomass = 0.000033400*Size^2.593599,
                  Lengthexpanded = Size*AdjustedFreq, Biomassexpanded = Biomass*AdjustedFreq)

meanLength = sum(hylength$Lengthexpanded)/sum(hylength$AdjustedFreq)
meanBiomass = sum(hylength$Biomassexpanded)/sum(hylength$AdjustedFreq)
0.000033400*meanLength^2.593599
ggplot(hylength, aes(x = round(Biomass, 4), y = AdjustedFreq)) + geom_col()
ggplot(hylength, aes(x = Size, y = AdjustedFreq)) + geom_col()
#most individuals are 2-4 mm.
#mean biomass is about 0.000677g (wet weight)
10418955*0.000677
7.053 #kg hyalella (wet weight)
#probably about 0.7kg dry weight
#literater values for production to biomass ratio were 3.9 to 11. So total produciton could be 
#anywhere from 2.7 kg to 7.75 kg carbon (dry wight)
#with an assymelation effeiciency of 15%, that means they 
#could have used
2.7/0.15
7.75/0.15
#between 18 and 52 kg of vegtation biomas. So, not very much in the scheme of the weltand

#what other amphipods are in Liberty? ###########################
amplib = filter(Ampsum2, AmpType2 == "Corophiid", VegType %in% c("Emergent", "Floating", "Submersed"))

cormod3 = glmmTMB(CPUE ~  VegType+ SalSurf + Season +(1|Year), family = "nbinom2",
                 ziformula = ~SalSurf+ VegType,
                 data = amplib)
predict(cormod3, newdata = newdat)
#2.46 corophiids per cubic meter. 
#assuming emergetn marsh is about 0.5m deep
3157259/2*2.46
3883429 #corophids individuals


CorLIB18 =  filter(Ampsum2, AmpType2 == "Corophiid", VegType =="Emergent", Project_na == "Liberty")

mean(CorLIB18$CPUE)
#no corophiids. that's a bit odd. 


gamLIB18 =  filter(Ampsum2, AmpType2 == "Gammarid", VegType =="Emergent", Project_na == "Liberty")

mean(gamLIB18$CPUE)
#31
exp(mean(log(gamLIB18$CPUE+1)))
#2.25

gamarids =  filter(Ampsum2, AmpType2 == "Gammarid", VegType %in% c("Emergent", "Floating", "Submersed"))

gammod3 = glmmTMB(CPUE ~  VegType+ SalSurf + Season +(1|Year), family = "nbinom2",
                  ziformula = ~SalSurf+ VegType,
                  data = gamarids)
predict(gammod3 , newdata = newdat)
#six gamarids per cubic meter