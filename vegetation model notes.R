#food web model including vegetation
#modified from Power et al. 1995
#https://doi.org/10.2307/1312555

#and from Power et al. 1995b
#https://doi.org/10.1016/0169-555X(95)00039-8

#Biomass balance equations for trophic dynamics subject to hydraulic constraints
#Detritus - dD/dt = I + mvV - chHD - mdD
#Vegtation - dV/dt = rV(K-V/K)-chHV - mvV
#Grazers - dH/dt = bhChHV + bhchHD - cpHP-mhH
#predators - dP/dt = bpcpHP-mpP

#D - detritus standing stock g/m2
#V - vegetation biomass g/m2
#H - grazer biomass g/m2
#P - predator biomass g/m2
#bh - conversion efficiency for grazers eating vegetation or detritus g/g 0.025 (detritus) or (0.05 vegetation)
#bp - conversion efficiency for predators eating grazers g/g (0.20)
#ch - per capita grazing rate on detritus or vegetation m2/g/wk 0.025 OK, so this is the proportion of the vegetation in a sqwuare meter that is consumed per gram predator biomass
#cp - per capita predation rate on grazers m2/g/wk 0.025
#r - maximum intrinsic rate of increase for vegetation 1/wk (0.5)
#I - allochthonous detritus g/m2wk
#K - carrying capacity of vegetation g/m2 - varoes by species, max from Lacey et al 2021 for 
#Egeria was 851 g/m2 (dry mass)
#md - loss rate of detritus 1/wk 0.002
#mv - non-grazing mortality of vegetation 1/wk 0.001
#mh - non-predation mortality of grazers 1/wk 0.001
#mp = predator mortality 1/wk 0.005

#Vegetation carrying capacity depends on depth, turbidity, and velocity.

#all the emerent vegetation turns into detritus once per year
#what about the submerged veg?

library(tidyverse)
library(readxl)

load("data/AllWetlandBugs.RData")
names(AllBugs)


savproduction = read_excel("data/AV_productivity_rates_lit_values_tidy_20220216.xlsx")
egeria = filter(savproduction, species == "Egeria densa")
names(egeria)

ggplot(egeria, aes(x = 1, y = productivity_365_days_gCm2)) + geom_boxplot()
#averages 500 g/m2/year

AllvegForReal = read_csv("data/AllvegAreas.csv")

#how much do the "grazers" actually graze on the vegtation versus periphyton?
# strong inverse relationship between algal productivity and mesoherbivore 
#grazing suggests that gammarids prefer decomposing algae over fresh plants (Kotta et al. 2006). 
#https://link.springer.com/article/10.1007/s10530-008-9274-6
#depends on season and macrophyte species, but generally 0.001-0.015 gplants/amphipod/day. This is about 10-40% of production
#Algae was more on the lines of 0.001-0.003 g/amphipod/day. 
#his is severl hundred percent of production . Not sure if this is per day or perseason 

#Cloern et al just said periphyton production was 44 gC/m2/yr on marsh plants, 
#24 gC/m2 per year on aquatic plant

#set up empty data frame for model

Production= data.frame(D_detritus = NA, V_veg = NA,
                             H_grazers = NA, P_predators = NA, Time = 1:200)

#model coefficients - start with egeria
bv = 0.05 #conversion efficiency for grazers eating vegetation (0.05 vegetation)
bd = 0.025 #conversion efficiency for predators eating or detritus g/g 0.025 (detritus) or 
bp = 0.2# - conversion efficiency for predators eating grazers g/g (0.20)
ch = 0.005 #per capita grazing rate on detritus or vegetation m2/g/wk 0.025 
cp = 0.025  #per capita predation rate on grazers m2/g/wk 0.025

#need to change this one for different vegetation
r = 0.5# - maximum intrinsic rate of increase for vegetation 1/wk (0.5)
# 0.009 to 0.016 day-1, for egeria, Pistori 2004, Bianchini et al 
# put it at 0.02 to 0.18/day, which sounds more reasonable. 
K =851 # carrying capacity of vegetation g/m2 - varoes by species, max from Lacey et al 2021 for 
#Egeria was 851 g/m2 (dry mass)
md = 0.002 # loss rate of detritus 1/wk 0.002
mv = 0.001# - non-grazing mortality of vegetation 1/wk 0.001
mh = 0.001# - non-predation mortality of grazers 1/wk 0.001
mp = 0.001#predator mortality 1/wk 0.005
I_allotchinous =100 #put some random rate of alotchinous input

#look over time
#starting values
Production[1,] = c(100,800,20,5,1)

for(t in 2:nrow(Production)) {
  Production$D_detritus[t] = I_allotchinous + mv*Production$V_veg[t-1] - 
    ch*Production$H_grazers[t-1]*Production$D_detritus[t-1] - 
    md*Production$D_detritus[t-1] +Production$D_detritus[t-1]

    Production$V_veg[t] = r*Production$V_veg[t-1]*(K-Production$V_veg[t-1])/K - 
    ch*Production$H_grazers[t-1]*Production$V_veg[t-1]-
    mv*Production$V_veg[t-1]+ Production$V_veg[t-1]
  Production$H_grazers[t] = bv*ch*Production$H_grazers[t-1]*Production$V_veg[t-1]+
    bd*ch*Production$H_grazers[t-1]*Production$D_detritus[t-1]-
    cp*Production$H_grazers[t-1]*Production$P_predators[t-1]-
    mh*Production$H_grazers[t-1] + Production$H_grazers[t-1]
  Production$P_predators[t] = bp*cp*Production$H_grazers[t-1]*Production$P_predators[t-1]-
    mp*Production$P_predators[t-1]
  Production[Production < 0] = 0

}

ggplot(Production)+ geom_line(aes(x = Time, y = D_detritus), color = "orange")+
  geom_line(aes(x = Time, y = V_veg), color = "green4")+
  geom_line(aes(x = Time, y = H_grazers), color = "brown")+
  geom_line(aes(x = Time, y = P_predators), color = "black")

Production_long = pivot_longer(Production, cols = c(D_detritus:P_predators),
                               names_to = "Parameter", values_to = "Biomass")

ggplot(Production_long, aes(x = Time, y = Biomass, color = Parameter)) +
  geom_line()+
  scale_color_manual(values = c("brown", "orange", "black", "green4"))

#try expanding, include periphyton, two bug taxa, ###############################################################
#think about what time step I want to use, daily, weekly, monthly, annual?

#set up empty data frame for model

ProductionX= data.frame(D_detritus = NA, V_veg = NA, PF_periphyton = NA,
                       H_grazers = NA,P_predators = NA, Time = 1:200)

#model coefficients - start with egeria
bv = 0.025 #conversion efficiency for grazers eating vegetation (0.05 vegetation)
bd = 0.025 #conversion efficiency for herbivores eating or detritus g/g 0.025 (detritus) or 

bpf = 0.1# - conversion efficiency for herbivores eating periphtyon g/g
bp = 0.2# - conversion efficiency for predators eating grazers g/g (0.20)

kpfv = 0.1# - maximum amount of periphyton that can grow on vegetation g/g

chd = 0.2 #per capita grazing rate on detritus g/g/day
chv = 0.009 #per capita grazing rate on vegetation g/g/day
chpf = 0.02 #per capita grazing rate on periphyton g/g/day
cp = 0.1  #per capita predation rate on grazers g/g/day 0.025

#need to change this one for different vegetation
r = 1.016# - maximum intrinsic rate of increase for vegetation 1/day (0.5)
# 0.009 to 0.016 day-1, for egeria, Pistori 2004, Bianchini et al 
rph = 1.016# - maximum intrinsic rate of increase for periphyton 1/day - how to incoprate veg?

K =851 # carrying capacity of vegetation g/m2 - varoes by species, max from Lacey et al 2021 for 
#Egeria was 851 g/m2 (dry mass)

md = 0.0005 # loss rate of detritus 1/day 
mv = 0.0005# - non-grazing mortality of vegetation 1/dat
mh = 0.0005# - non-predation mortality of grazers 1/day
mp = 0.0005#predator mortality 1/day 
I_allotchinous =50 #put some random rate of alotchinous input

#look over time
#starting values
ProductionX[1,] = c(100,800,80,20,5,1)

for(t in 2:nrow(ProductionX)) {
  ProductionX$D_detritus[t] = I_allotchinous + mv*ProductionX$V_veg[t-1] - 
    ch*ProductionX$H_grazers[t-1]- 
    md*ProductionX$D_detritus[t-1] +ProductionX$D_detritus[t-1]

  ProductionX$V_veg[t] = r*ProductionX$V_veg[t-1]*(K-ProductionX$V_veg[t-1])/K - 
    ch*ProductionX$H_grazers[t-1]-
    mv*ProductionX$V_veg[t-1]+ ProductionX$V_veg[t-1]
  ProductionX$PF_periphyton[t] = rph*ProductionX$PF_periphyton[t-1]*(kpfv*ProductionX$V_veg[t-1]-ProductionX$PF_periphyton[t-1])/K - 
    chpf*ProductionX$H_grazers[t-1]+
     ProductionX$PF_periphyton[t-1]
  ProductionX$H_grazers[t] = bv*chv*ProductionX$H_grazers[t-1]*ProductionX$V_veg[t-1]+
    bpf*chpf*ProductionX$H_grazers[t-1]+
    bd*chd*ProductionX$H_grazers[t-1]-
    cp*ProductionX$H_grazers[t-1]-
    mh*ProductionX$H_grazers[t-1] + ProductionX$H_grazers[t-1]
  ProductionX$P_predators[t] = bp*cp*ProductionX$H_grazers[t-1]-
    mp*ProductionX$P_predators[t-1]
  ProductionX[ProductionX < 0] = 0
  
}

ggplot(ProductionX)+ geom_line(aes(x = Time, y = D_detritus), color = "orange")+
  geom_line(aes(x = Time, y = V_veg), color = "green4")+
  geom_line(aes(x = Time, y = H_grazers), color = "brown")+
  geom_line(aes(x = Time, y = P_predators), color = "black")+
  geom_line(aes(x = Time, y = PF_periphyton), color = "cyan")

ProductionX_long = pivot_longer(ProductionX, cols = c(D_detritus:P_predators),
                               names_to = "Parameter", values_to = "Biomass")

ggplot(ProductionX_long, aes(x = Time, y = Biomass, color = Parameter)) +
  geom_line()+
  scale_color_manual(values = c("brown", "orange", "black", "cyan", "green4"))+
  coord_cartesian(xlim = c(0,100), ylim = c(0,2000))


