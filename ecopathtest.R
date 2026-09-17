#ok, now let's try ecopath for real

library(Rpath) # we should use this for this effort
library(tidyverse)
library(data.table)
library(readxl)
#library(ecostate) #this might work better for time series dat ain the future. 

#what we need

#questions for Matt
#which package did you use?
#parameters - start with what you developd for wildlands with various updates?
#hydrodynamics....
#biomass in

#regional versus site-specific models. 

#biomass of critters in the wetlands that are important to fish. Maybe we don't need a food web model?

#we're stuck on rates. HOw do we contextualize a field measurement of bug density?

#a mass-balance model could help on rates. 
#system through-put - tons of carbon per area. Ecosystem-wide metric of production - where is that coming from?
#Go back to Cloern et al? Percentage from different areas? Phytoplankon, detritus, macrophytes. Ephyton stuff too. 
#percent of each group of producer to total system throughput.
#what is the controbution of each of those three things to the bugs we care about. Inferred diet study or stable isotopes. But scaled up. 

#Diets from stable isotopes?
#Harfman et al - detritis. 
#PB ratios - literature values - but which ones?
#diet from stable isotopes. 
#matt's ecopath model says total system system throughput  is on the right order of magnitude
#linear inverse models - simplified food chain models with marcov chains. '
#generalized wetland to generalized channel. 

#make sure we have the critters we care about and taxonomic resolution 
#maybe explore densities at a finer taxonomic resolution, system throughouput at a courser level. 

#GOal: Total system throughput and proportion from each producer

#1. Define funcitonal groups. Discritize our taxa however.
#2. Do we need fish data? what resolution do we need? Could just assume we have consumers that eat everything. Don't need to define it a priori
#2. Work to get justifiable biomass. Maybe biggest hangup. Or maybe not. We've got some idea.
#3. Need to translate data to kg/Hectar. Scale survey data to area of habitat data in each wetland. 
#use remote sensing to get at area and volume of each habitat type. Then generalize it. Make up some wetlands with different precentages of each habitat type.
# Biomass table, diet controbution table. Just sit down and talk through it. PB and EE will come from lit.
# It all gets advected or consumped. Two loss terms one for consumption and one for advection. 
#dam it, omnivory. 
#dont forget about microbes/mocrozoops. Maybe there are some rules of thumb about that? Are there normal ratios of macro to microzoops? Size structure of zoop populations 


#matt to lay out objective and path to get there. 
#ask for each people is going to be getting biomass first. 1. functional groups within our taxa. 

#2. biomass per volume/area or data needed to get there. - Rosie to describe. 

#first work on biomass predicitons
#predatory copepods
bcop = readRDS("outputs/zoop/bcop_hu_predictions.rds")
#non-predatory copepods
gcop = readRDS("outputs/gcop_hu_predictions.rds")
#limnoithona
wcop = readRDS("outputs/wcop_hu_predictions.rds")
#cladocera
clad = readRDS("outputs/clad_hu_predictions.rds")

#zooplankton model plots
gcopmod = read_rds("outputs/gcop_base_hu_brmsfit.rds")
wcopmod = read_rds("outputs/wcop_base_hu_limno_brmsfit.rds")
cladmod = read_rds("outputs/zoop/clad_base_hu_brmsfit.rds")
bcopmod = read_rds("outputs/zoop/bcop_reg_only_hu_brmsfit.rds")

plot(conditional_effects(gcopmod), theme = theme_bw())
bayes_R2(gcopmod)
plot(conditional_effects(wcopmod), theme = theme_bw())
bayes_R2(wcopmod)
plot(conditional_effects(bcopmod), theme = theme_bw())
plot(conditional_effects(cladmod), theme = theme_bw())

#average by region. Turn into biomass
#biomass is anywhere from 3 for Acanthocyclops to 15 for an andult tortaus. I'll start with 7 ug carbon weight.
#This is biomass per cubic meter. Need to multiply by depth, deeper outside wetlands than inside.
#also convert to wet weight - 7/.4 * 10
bcopmean = group_by(bcop, Region) %>%
  summarise(Mean = mean(pred), sd = sd(pred), 
            meanbiomass = Mean*7/1000000/.4*10) %>% #wet weight in g. per cubic meter
mutate(Group = "PredatoryCopepods", Habitat = "Open Water") %>%
  merge(data.frame(Type = c("Inside", "Outside")))

#non predatory copepods are 2-3 ugC for adults.
gcopmean = group_by(gcop, Region, Type) %>%
  summarise(Mean = mean(pred), sd = sd(pred), 
            meanbiomass = Mean*2/1000000/.4*10) %>%
  mutate(Group = "OtherCopepods", Habitat = "Open Water")

#Limniothona are 0.13
wcopmean = group_by(wcop, Region,Type) %>%
  summarise(Mean = mean(pred), sd = sd(pred), 
            meanbiomass = Mean*.13/1000000/.4*10) %>%
  mutate(Group = "Limnoithona", Habitat = "Open Water")

#cladocera are pretty variable, Daphnia are big (4ug), bosmina are small (0.6 ug)
#I'll split the difference for now
cladmean = group_by(clad, Region, Type) %>%
  summarise(Mean = mean(pred), sd = sd(pred), 
            meanbiomass = Mean*2/1000000/.4*10) %>%
  mutate(Group = "Cladocera", Habitat = "Open Water")


#amphipod data
load("outputs/amph_predictions.RData")

#gamarids
#from FRP data, geometric mean biomass per gammarid in March was 9.642778e-04
#and i think that's wet weight
#I need to doublecheck wehhter that i grams or mg
gammean = group_by(gamarid_predictions, Region, Habitat, Type) %>%
  summarise(Mean = mean(Estimate), 
            meanbiomass = Mean*0.000964) %>%
  mutate(Group = "Gammarids")

#1.020357e-03
cormean = group_by(corph_predictions, Region, Habitat, Type) %>%
  summarise(Mean = mean(Estimate), 
            meanbiomass = Mean*0.0010203) %>%
  mutate(Group = "Corophiids")

#clams - Kristi gave me the models I need to generate predictions
corbmodel = readRDS("Corb_lognorm_h_58.rds")
potmodel = readRDS("Potamo_lognorm_h_61.rds")

summary(corbmodel)
summary(potmodel)
biv_data_sp <- readRDS(here("Data/d_Bivalves_wide_X2_spc_sp.rds"))

#oh, conductivity and X2 have been centered so I can just use 0 for the mean!
load("outputs/bivalve_predictions.RData")

corpred = group_by(corbic_predictions, Region) %>%
  summarise(meanbiomass = mean(Estimate)) %>%
  mutate(Group = "Corbicula", Habitat = "Open Water") %>%
  merge(data.frame(Type = c("Inside", "Outside")))

potpred = group_by(potam_predictions, Region, Type) %>%
  summarise(meanbiomass = mean(Estimate)) %>%
  mutate(Group = "Potamocorbula", Habitat = "Open Water")

#don't have insects yet, placeholder
insects = read_excel("MattsRpath/RPath_parameterdraftlist.xlsx", sheet = "InsectPlaceholder", na = "NA")


#all biomasses
biomasses = bind_rows(cormean, gammean, cladmean, gcopmean, wcopmean, bcopmean, corpred, potpred)

write.csv(biomasses, file = "outputs/biomass_predictions.csv")

biomassesA = biomasses %>%
  mutate(Region = case_when(Region %in% c("Decker", "Web Tract Berms") ~ "Decker/Webb",
                            TRUE ~ Region)) %>%
  group_by(Region, Habitat, Type, Group) %>%
  summarize(meanbiomass = mean(meanbiomass, na.rm = T))

#now biomass per hectare

load("outputs/VegAreaByRegion.RData")
#what percentage of each region is in each habitat?

vegarea_mean2 = vegarea_mean2 %>%
  mutate(Habitat = recode_values(VegType2,
                                 "emergent" ~ "EAV",
                                 "FAV" ~ "FAV",
                                 "water" ~ "Open Water",
                                 "SAV" ~ "SAV",
                                 default = "Other"  ))


ggplot(vegarea_mean2, aes(x = Region, y = PercentVeg, fill = Habitat)) +
  geom_col() + facet_wrap(~Type)

ggplot(vegarea_mean2, aes(x = Region, y = PercentVeg, fill = VegType2)) +
  geom_col() + facet_wrap(~Type)

#convert to biomass per hectare
biomasses_habitat = left_join(biomassesA, vegarea_mean2) %>%
  mutate(kg_per_ha = case_when(Habitat %in% c("EAV", "SAV", "FAV") ~ meanbiomass*10*PercentVeg, #assume vegetated habitats area all about 1mdeep, multiply by 10 to convert to wet weith
                               Habitat == c("Open Water") & Type == "Outside" & !Group %in% c("Potamocorbula", "Corbicula") ~ meanbiomass*10*PercentVeg*5, #still need depth values. Sigh. Assume it's 5 m deep outside, 1 m deep insiode
                               Habitat == c("Open Water") & Type == "Inside" & !Group %in% c("Potamocorbula", "Corbicula") ~ meanbiomass*10*PercentVeg*1,
                               Habitat == c("Open Water") & Type == "Outside" & !Group %in% c("Potamocorbula", "Corbicula") ~ meanbiomass*10*PercentVeg*5,
                               Habitat == c("Open Water") & Group %in% c("Potamocorbula", "Corbicula") ~ meanbiomass*PercentVeg*10,
                               TRUE ~ 0))

#biomass per hectare for the vegtation and phytoplankton.
#ugh, phyplankton

PPs = read_excel("MattsRpath/RPath_parameterdraftlist.xlsx", sheet = "primary producers", na = "NA") %>%
 right_join(vegarea_mean2)%>%
  mutate(kg_per_ha = Biomass*PercentVeg)

#detritus and fishing

detfish = data.frame(Group = c("Detritus", "Fishing")) %>%
  merge(data.frame(InsideOutside = c("Inside", "Outside"))) %>%
  merge(data.frame(Region = unique(biomasses_habitat$Region)))

Allbiomass = bind_rows(biomasses_habitat, PPs) %>%
  select(Region, Habitat, Type, Group, kg_per_ha) %>%
  rename(Biomass = kg_per_ha, InsideOutside = Type) %>%
  group_by(Region, InsideOutside, Group) %>%
  summarise(Biomass = sum(Biomass, na.rm =T)) %>%
  filter(!is.na(Group)) %>%
  bind_rows(insects) %>%
  bind_rows(detfish) %>%
  mutate(Scenario = paste(Region, InsideOutside))



#load other parameters besides biomass
pathModel = read_excel("MattsRpath/RPath_parameterdraftlist.xlsx", sheet = "testparams", na = "NA") %>%
  filter(!is.na(Group)) %>%
  select(-Biomass) %>%
  left_join(Allbiomass)



diets = read_excel("MattsRpath/RPath_parameterdraftlist.xlsx", sheet = "dietmatrix", na = "NA")

#set up Rpath for each scenario
source("checkRpath.R")

rpathcreation = function(ScenarioX, Data){
  pathmodel = filter(Data, Scenario == ScenarioX)
  Rosiepath <- create.rpath.params(group = pathmodel$`Group`,
                                        type = pathmodel$Type, stgroup = NA)

Rosiepath$model[, Biomass := pathmodel$Biomass]
Rosiepath$model[,Detritus := pathmodel$Detritus]
Rosiepath$model[, Fishing := 0]
Rosiepath$model[, Fishing.disc := 0]
Rosiepath$model[, PB := pathmodel$PB]
Rosiepath$model[, QB := pathmodel$QB_calc]
Rosiepath$model[, Unassim  := pathmodel$Unassim ]
Rosiepath$model[, BioAcc  := pathmodel$BioAcc ]
Rosiepath$diet = as.data.table(diets)

check.rpath2(Rosiepath)
Rosiepath_testrun <- rpath(Rosiepath, eco.name = 'test ecosystem')

return(Rosiepath_testrun)

}


# Check parameters
#check.rpath.params(Rosiepath)

CacheInside = rpathcreation("Cache Slough Inside", pathModel)

print(CacheInside, morts = F)


summary(CacheInside)
CacheInside$TL #trophic level
CacheInside$Unassim #
CacheInside_plot =bind_cols(Group = CacheInside$Group, Biomass = CacheInside$Biomass, TL = CacheInside$TL)
CacheInside_plot2 = summarize.for.webplot(CacheInside)

Rosiewebplot = function(Rpathob, Scenario) {

  plotob = summarize.for.webplot(Rpathob)
p <- ggplot() + geom_segment(aes(x = pred.x, y = pred.y, 
                                 xend = prey.x, yend = prey.y), color = "grey",  
                             data = plotob$connections) + labs(x = "", y = "Trophic position") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  geom_point(aes(x = x.pos, y = TL, size = log(Biomass), color = log(Biomass)),  data = plotob$pointmap)+
  scale_size(range = c(0.5, 20), breaks = c(-5,0,5,10,15))+
  ggrepel::geom_text_repel(aes(x = x.pos, y = TL, label = Group),  
            data = plotob$pointmap, color = "black")+
  theme_bw()+
  scale_color_viridis_c()+ggtitle(Scenario)
p  }

Rosiewebplot(CacheInside, "Cache Slough Wetland")

CacheOutside = rpathcreation("Cache Slough Outside", pathModel)
print(CacheOutside)
Rosiewebplot(CacheOutside, "Cache Slough Open Water")


GrizzlyOutside = rpathcreation("Grizzly Bay Outside", pathModel)
print(GrizzlyOutside)
Rosiewebplot(GrizzlyOutside, "Grizzly Bay Open Water")

GrizzlyInside = rpathcreation("Grizzly Bay Inside", pathModel)
print(GrizzlyInside)
Rosiewebplot(GrizzlyInside, "Grizzly Bay Wetland")


# simulation stuff - for later ###########################
# Create a 50 yr Rsim scenario
Rsim.scenario <- rsim.scenario(Rosiepath_testrun, Rosiepath, years = 1:50)
# Run the Rsim simulation
Rsim.output <- rsim.run(Rsim.scenario, method = "RK4", years = 1:50)
# Extract a prey's biomass loss from each predator over the model run
rsim.mort(Rsim.output, group = "Gammaroidea")

#I definitely need something eating my bugs or they are just going to accumulate uselessly. 

#Questions:
#What do i do about loss/consumption?
#Omnivory
#direct consumption of vegetatuion versus deteritus
#data pedigree
#how do i get TSTP?

#The total system throughput is the sum of all flows in a system, expressed, e.g., in t · km-2 · year-1. It is estimated as the sum of four flow components, i.e.,
#Total consumption
#+ Total export
#+ Total respiration
#+ Total flows to detritus
#= Total system throughput.
#Total system throughput represents the ‘size of the entire system in terms of flow’ (Ulanowicz, 1986). 
#As such, it is an important parameter for comparisons of flow networks.


