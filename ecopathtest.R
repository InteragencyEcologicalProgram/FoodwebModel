#ok, now let's try ecopath for real

library(Rpath) # we should use this for this effort
library(tidyverse)
library(data.table)
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
library(readxl)

pathModel = read_excel("MattsRpath/RPath_parameterdraftlist.xlsx", sheet = "testparams", na = "NA") %>%
  filter(!is.na(Group))
diets = read_excel("MattsRpath/RPath_parameterdraftlist.xlsx", sheet = "dietmatrix", na = "NA")


Rosiepath <- create.rpath.params(group = pathModel$`Group`,
                                        type = pathModel$Type, stgroup = NA)


Rosiepath$model[, Biomass := pathModel$Biomass]

Rosiepath$model[,Detritus := pathModel$Detritus]

Rosiepath$model[, Fishing := 0]
Rosiepath$model[, Fishing.disc := 0]
Rosiepath$model[, PB := pathModel$PB]
Rosiepath$model[, QB := pathModel$QB_calc]
Rosiepath$model[, Unassim  := pathModel$Unassim ]
Rosiepath$model[, BioAcc  := pathModel$BioAcc ]

Rosiepath$diet = as.data.table(diets)

# Check parameters
#check.rpath.params(Rosiepath)
source("checkRpath.R")
check.rpath2(Rosiepath)
Rosiepath_testrun <- rpath(Rosiepath, eco.name = 'test ecosystem')

Rosiepath_testrun

print(Rosiepath_testrun, morts = F)
print(Rosiepath_testrun, morts = T, skip_absent = T)


summary(Rosiepath_testrun)
Rosiepath_testrun$TL #trophic level
Rosiepath_testrun$Unassim #
Rosiepath_testrun$DetFate

webplot(Rosiepath_testrun)
webplot(Rosiepath_testrun,eco.name="Test",labels = T)



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


