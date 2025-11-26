#Clam modeling

#let's look at clam data

#we published everything through 2021 with the drought project
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1653.1

library(tidyverse)
library(sf)
library(deltamapr)
library(spacetools)

EMPclams = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/407336fbf9a2005780935acc5f223e34") 
GRTSclams = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/8a1292d209f5ed2a7f1de1f140b0b837") 
clamregressions = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1653/1/0f11bd41ec1b41be92e502a9fa47209e") 

#meterCubedPerMeterSquaredPerDay
TotalFiltration = group_by(EMPclams, Station, Date, Latitude, Longitude) %>%
  summarize(Filtration = sum(Filtration_Rate), Turnover = sum(Turnover_Rate)) %>%
  mutate(Year = year(Date), Month = month(Date), Longitude = case_when(Longitude >0 ~ Longitude*-1,
                                                                       TRUE ~ Longitude))

ggplot(TotalFiltration, aes(x = Station, y = Filtration))+ geom_boxplot()

ggplot(TotalFiltration, aes(x = as.factor(year(Date)), y = Filtration))+ geom_boxplot()

#now the data from the suisun publication
siusunclams = read_csv("data/SMSCG_clam_EDI_2018_2021_corr.csv")
#ok, grazing turnover per day i sin there.

# rates we need:
# phytoplankton growth rate per day
#zooplankton growth rate per day
#zooplankton food needs
#water residence time?


ggplot(siusunclams, aes(x = Month, y = Total_grazing_turnover_per_day)) + geom_boxplot()+
  facet_wrap(~Year)

#Primary productivity ###################
#Jassby et al 2002
# P = Theta*B*I*Z = 4.61theta(BI)/k
# Theta = 0.728
# P = primary productivity (mgC/m2/day maybe)
#B is chla concentration in mg/m3
# I is surface PAR
#z is photic zone depth
# k is vertical attenuation coefficient for PAR

#from Richardson et al . 2023, k = 0.52+(Turbidity*0.11)
#now I just need PAR

#maybe start with 2019, since that's the year they are doing the modelign for. 

#Daily average of incident solar radiation (SR) in watts per square meter 
#(CIMIS 2021) was added into the data set because of its likelihood to influence light 
#availability in water (Jassby 2008). The California Irrigation Management Information System 
#(CIMIS) continuous monitoring station at Twitchell Island (station 140; Figure 1) was used as the 
#site location for SR because of its data completeness and central location within the Delta (CIMIS 2021). 

#Another formula from Cloern et al 2017 Gross Primary Production (mgC/m2/day)- GPP = 3.77*E*C/k,where 
#E is mean incident PAR (moles quanta/m2/d) 
#for the month of sampling based on measurements with a LiCor 190s quantum sensor from 1979 through 2004, 
#and C is Chl a (ug/L).

#Solar radiation is in Watt/m2. Lucas and Thomsan 2012 says it's 0.18* solar radiation in W/m2 to 
#get PAR in moles quanta/m2/day


#So now I need chlorophyll concentrations.
#should I use continuous sondes? OR satellite data? Or discrete grabs?

# I think I'll start with the chlorophyll taken at the same time as the clams.
#Jassby and Cloern 2002 say C:chl is 35
#Lucas and Thompson says C:chl is 32

#Lopez shallow water paper - Einsteins= PAR*.18 
#(looks like Einsteins are the same as moles quanta)

#here's another formula
#https://www.canr.msu.edu/floriculture/uploads/files/MakingSenseofLightSensors.pdf
#1 W/m2 * 1.96 umol/W = 1.96 umol/m2/sec 
# 1.96*86400sec/day = 169344/1000000 = moles/m2/day
#this gives us a conversion rate of 0.169, which is pretty close to 0.18

PAR2 = read_csv("data/CIMIS_daily.csv") %>%
  mutate( Date = mdy(Date),
          PAR = 0.45*`Sol Rad (W/sq.m)`, #Par in Watts/m2
          PAR_moles = (PAR*3.6/1000)*2.0514/1000000,
          Par_moles2 = `Sol Rad (W/sq.m)`*1.96*86400/1000000,#par in mole/m2*day maybe?
          Par_moles3= `Sol Rad (W/sq.m)`*0.18) #version used Lucas adn Thompson conversion

#I'll use the Lucas and Thompson value from now on, for consistancy

#now From Lucs and Thompson 2012, Effective productiiyt is depth-averaged, day-averaged
#net growth rate minus grazing rate divided by depth 

#they have terms for zooplankton and benthic grazing, but i'll stick to benithinc. 

# Productivity_eff = Productivity_Net - Grzing/Depth
#U gross= producitivty/[C:chl]
#resp = u gross*0.15 + 0.015

susproductivity2 = siusunclams %>%
  select(Year, Month, Station, Date, North_decimal_degrees, West_decimal_degrees,
         Total_grazing_turnover_per_day, #number of times water column is turned over per day
         Chlorophyll_a_ug_per_L, #this is derrived from lab data collected at the same time as the clams
         Depth_m, Habitat, 
         Turbidity_NTU,
         Total_grazing_rate_m3_per_m2_per_day) %>% #grazing rate in volume per day for 1 m2 fo clams
  left_join(select(PAR2, Date, PAR, Par_moles3)) %>%
  mutate(Productivity = (4.61*0.728*(Chlorophyll_a_ug_per_L)*Par_moles3)/(0.52+(Turbidity_NTU*0.11)), #Version from Jassby and CLeron 2002
         Carbon = (Chlorophyll_a_ug_per_L*35),
         Productivity2 = 3.77*Par_moles3*Chlorophyll_a_ug_per_L/(0.52+(Turbidity_NTU*0.11)), #instantaneous rate of photosynthesis at a given depth
         PhytoTurnover = Productivity2*Depth_m/Carbon,
         GrowthToGrazing = PhytoTurnover/Total_grazing_turnover_per_day, #ratio of growth rate to grazing rate
         Respiration = 0.015*35*Chlorophyll_a_ug_per_L*Depth_m + 0.15*Productivity2, #instantaneous rate of phytoplnkton biomass lost to respiration at a given depth
         NetGrowth = Productivity2 - Respiration, #growth minus resporiation at a given depth
         PhytoTurnover2 = NetGrowth*Depth_m/Carbon,
         GrowthToGrazing2 = PhytoTurnover2/Total_grazing_turnover_per_day,
         
         u_gross = Productivity2/Carbon, #instantaneous growth rate
         resp = u_gross*.15+0.015, 
         u_net = u_gross-resp,#instantagneous net growth rate
         u_eff = u_net- Total_grazing_rate_m3_per_m2_per_day/Depth_m, #effective growht rate (minus clam grazing.)
         SampleID = paste(Station, Date))
#not 100% sure I did this right, but we'll see
#the two different productivity estimates are lining up beterm at least.


ggplot(susproductivity2, aes(x = Habitat, y = log(GrowthToGrazing))) + geom_boxplot()+
  facet_wrap(~Year)

ggplot(susproductivity2, aes(x = Habitat, y = PhytoTurnover2)) + geom_boxplot()+
  facet_wrap(~Year)

ggplot(susproductivity2, aes(x = Habitat, y = Turbidity_NTU)) + geom_boxplot()+
  facet_wrap(~Year)

ggplot(susproductivity2, aes(x = Habitat, y = u_eff)) + geom_boxplot()+
  facet_wrap(~Year)

ggplot(susproductivity2, aes(x = Habitat, y = log(u_eff))) + geom_boxplot()+
  facet_wrap(~Year)
#turbidity is higher in the small sloughs. 
#hey now....

ggplot(susproductivity2, aes(x = Total_grazing_rate_m3_per_m2_per_day, y = u_eff))+ geom_point()


ggplot(susproductivity2, aes(x = Productivity2, y = u_eff))+ geom_point()


ggplot(susproductivity2, aes(x = u_net, y = u_eff))+ geom_point(aes(color = Habitat))

#OK, so I need to spatially average the clams? Or average by habitat and season?


ggplot(susproductivity2, aes(x = Habitat, y = u_eff))+ geom_boxplot(aes(color = Habitat))+ 
  facet_wrap(~Month)

#let's make some maps
SP_sf = st_as_sf(susproductivity2, coords = c("West_decimal_degrees","North_decimal_degrees"),
                 crs = 4326, remove = F)

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = SP_sf, aes(color = u_eff), size =3)+
  coord_sf(xlim = c(-122.2, -121.85), ylim = c(38.08, 38.3))+
  scale_color_viridis_b(breaks = c(-1, 0, 1,    2, 4, 8))

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = SP_sf, aes(size = Total_grazing_rate_m3_per_m2_per_day, color = Total_grazing_rate_m3_per_m2_per_day))+
  coord_sf(xlim = c(-122.15, -121.85), ylim = c(38.08, 38.24))+
  scale_size(range = c(0.1, 6), name = "Clam Grazing Rate", breaks = c( 0, 1,2,4,10,15))+
  scale_color_viridis_b(name = "Clam Grazing Rate",
                        breaks = c( 0, 1,2,4,10))+
  theme_bw()+
  annotation_scale()+
  annotation_north_arrow(location = "tl")

#join clams to regions

SP_sf_joined = SP_sf %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_join(WW_Delta)

#try to get all the sampling locatiosn in the water

# Calculate the distance between each point and each polygon
distance_matrix = as.data.frame(st_distance(st_transform(Points_fixed, crs = st_crs(WW_Delta)), WW_Delta))

# reshape the matrix
df_distance  =distance_matrix %>%
  mutate(SampleID = SP_sf_joined$SampleID) %>%
  pivot_longer(cols = c(V1:V282), names_to = "SloughID", values_to = "Distance")

# Subset the closest polygon of each point
df_nearest <- df_distance %>% group_by(SampleID) %>% 
  arrange(Distance) %>% slice(1) 

Polygons = WW_Delta %>%
  mutate(Numb = 1:nrow(WW_Delta), SloughID = paste("V", Numb, sep = "")) %>%
  select(SloughID, AREA, HNAME)

#everythign together....

susprod = susproductivity2 %>%
  left_join(df_nearest) %>%
  left_join(Polygons)

#now calculate average area, productivity, chlorophyll, etc.

susprod_summary = susprod %>%
  group_by(SloughID, HNAME, AREA) %>%
  summarize(chl = mean(Chlorophyll_a_ug_per_L, na.rm =T),
            chlsd = sd(Chlorophyll_a_ug_per_L, na.rm =T),
            u_netmean = mean(u_net, na.rm =T),
            u_effmean = mean(u_eff, na.rm =T),
            u_effsd = sd(u_eff, na.rm =T),
            u_effse = sd(u_eff, na.rm =T)/sqrt(n()),
            Grazing_mean = mean(Total_grazing_rate_m3_per_m2_per_day, na.rm =T),
            Grazing_sd = sd(Total_grazing_rate_m3_per_m2_per_day, na.rm =T))


#I have area, but I think i need total volume of each region. Darn.

#but assumign it's equal depth across.....

#Remove clams from 100m2, 200m2, etc.
#do a few regions as exampels
#start small

Marsh = filter(WW_Delta, HNAME %in% susprod_summary$HNAME) %>%
  filter(!HNAME %in% c("GRIZZLY BAY", "SACRAMENTO RIVER", "BROAD SLOUGH"))

ggplot(Marsh) + geom_sf(aes(fill = AREA))+
  scale_fill_viridis_c()

#now try and remove clams from some area of each slough

examples = filter(susprod_summary) %>%
  mutate( E0.1acres = u_netmean- (AREA-405)/AREA * Grazing_mean, # 0.1 acres
          E0.5acres = u_netmean- (AREA-2023)/AREA * Grazing_mean, #0.5 acres
          E01Acre = u_netmean- (AREA - 4046)/AREA * Grazing_mean, #1 acre
          E10Acres = u_netmean- (AREA - 40460)/AREA * Grazing_mean, #10 acres
          E100Acres = case_when(AREA < 404600 ~ u_netmean, #100 acres
                                AREA >= 404600 ~ u_netmean - (AREA - 404600)/AREA * Grazing_mean), 
          E_all_clams = u_netmean) %>%
  pivot_longer(cols = c(E0.1acres, E0.5acres, E01Acre, E10Acres, E100Acres, E_all_clams), 
               values_to = "u_effexample", names_to = "Scenario") %>%
  mutate(Scenario = factor(Scenario, levels = c("E0.1acres",  "E0.5acres", "E01Acre", "E10Acres", 
                                                "E100Acres", "E_all_clams"),
                           labels = c("0.1 acres", "0.5 acres", "1 acre", "10 acres", "100 acres",
                                      "All clams gone!")))

ggplot(examples, aes(x = HNAME, y = u_effexample, fill = Scenario)) + geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = u_effexample-u_effse, ymax = u_effexample+u_effse, group = Scenario), position = "dodge")+
  ylab("Effective production rate")+ xlab("Slough")+
  scale_fill_brewer(palette = "Set2", name = "Clam Reduction\nScenario")+
  facet_wrap(~HNAME, scales = "free")

#OK, how can I scale this up to zooplankton?

#Uh, I probably need Matt's zooplankton scenario

#Or I can use the chlorophyll-zoop relationships from my paper.

#but right now I have productivity, not chla. That's hard. 



#Wait, let's just see how much clams will eat in a day and calculate how much they eat over the
#entire summer, then divide by 10 for zoops?

susprod_sum2 = susprod_summary %>%
  mutate(food_eaten_perday = Grazing_mean*chl*u_netmean*AREA*35, #ug Carbon
         food_eaten_total = food_eaten_perday*150/1000000000, #kg Carbon
         zoops_not_grown = food_eaten_total/10,
         food_eaten_perday2 = Grazing_mean*chl*AREA*35*u_netmean, #ug Carbon#or maybe I should also multiply by u_net?
         food_eaten_total2 = food_eaten_perday2*150/1000000000, #kg Carbon
         zoops_not_grown2 = food_eaten_total2/10,) 

ggplot(susprod_sum2) + geom_col(aes(x = HNAME, y = log(zoops_not_grown)))

#look at the relationship between density and grazing rates. 
#do they have a satiation point? Cna they eat more? 

#if you have half as many clams, do you have twice as much clorophyll? No one knows?

#clams are aselective feeders? 

#simplified ecopath model, mass-balance. 

#Dig for productivity data. 

#from Lucas and thompson:
#Effective growth rate was used to calculate phytoplankton biomass B (mg chl a/m3)
#asa function of time. Calculated indices of habitat function include daily water 
#column net primary productivity (NPP,mgC/m2/d); gross productivity minus losses 
#to respiration calculated for day 7 of the simulation) and phytoplankton biomass at 
#7 days normalized by the initial
#biomass (B7:B0). We refer to the latter index as ‘‘biomass potential,’’ 
#since the chosen 7-day simulation length is arbitrary 

#H. Phytoplankton biomass B (mg chl a/ m3) was advanced through time according to:
#  B(t+ delta t) = B(t)exp(u_eff deltat/24)
#where t is time in hours

#Depth-averaged, day averaged phytoplankton net primary productivity 
#(NPP,mgC/m2/day) was calculated as: NPP = B*u_net*H*35 

#except i want 153 days. And day lenght is changing....