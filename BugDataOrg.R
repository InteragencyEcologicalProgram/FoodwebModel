#Bug data organization and integration

#this filters out all data from 2010-2024,
#targets the wetlands with the best data,
#and selects data from channel sites within 2 km of the wetland

library(tidyverse)
library(sf)
library(deltamapr)
library(readxl)

#load integrated bug dataset
load('data/AllWetlandBugs_2010onwards.RData')

#check that I have all the data I think i do
table(Allbugs_Oct2025$Source, Allbugs_Oct2025$Year)

library(zooper)
newdat = Zoopsynther(Data_type = "Community", Sources = c("EMP","FMWT", "STN"),
                     Years = c(2022:2025))

table(newdat$Source, newdat$Year)

FRP = Zoopsynther(Data_type = "Community", Sources = "FRP",
                  Years = c(2023:2025))

twentymil = Zoopsynther(Data_type = "Community", Sources = "20mm",
                  Years = c(2010:2025))
Allbugs_Mar2026 = filter(Allbugs_Oct2025,  Source != "20mm")  %>%
  bind_rows(FRP) %>%
  bind_rows(twentymil) %>%
  filter(!SampleID %in% newdat$SampleID) %>%
  bind_rows(newdat)

table(Allbugs_Mar2026$Source, Allbugs_Mar2026$Year)

save(Allbugs_Mar2026, file = 'data/AllWetlandBugs_2010onwards.RData')
#Bring in shapefile
#top priority sites from Dan's analysis
#Flyway Farms, Winter Island, LICB, Webb Tract, Tule Red, Ryer Island, LHT,
#Liberty, Decker, Chipps, Browns

 allsites = st_read("GIS dta/wetlandsites.shp") %>%
   st_make_valid()
# 
 PrioritySites = filter(allsites, Project_na %in% c("Flyway Farms", "Winter", "LICB", "Browns",
                                                                                 "Chipps", "Liberty", "Ryer", "Web Tract Berms",
                                                                        "Tule Red", "Decker", "LHT"))
# 
  save(allsites, file = "data/wetlandsites.RData")
  save(PrioritySites, file = "data/PrioritySites.RData")



load("data/wetlandsites.RData")
load("data/PrioritySites.RData")

ggplot(allsites)+geom_sf()

ggplot(PrioritySites)+geom_sf()


#I"m going to buffer the sites by 100m, then 2 km, 
#then remove sites on the inside of all wetlands for the "outside" definiton
#I origionall removed all samples within 100 m of wetlands, but i think that drops too many from small channels
samples = Allbugs_Mar2026 %>%
  filter(!is.na(Latitude)) %>%
  select(SampleID, Longitude, Latitude, Source) %>%
  distinct() %>%
  mutate(Longitude = case_when(Longitude >0 ~ Longitude * -1,
                               TRUE ~ Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) 
  
  

inside_all = samples %>%
  st_transform(crs = st_crs(allsites)) %>%
  st_join(allsites) %>%
  st_drop_geometry() %>%
  filter(!is.na(Project_na)) %>%
  mutate(Type = "Inside", Region = case_when(Project_na %in% c("Winter", "Browns", "Chipps") ~ "Confluence",
                                            Project_na %in% c("Tule Red", "Ryer") ~ "Grizzly Bay",
                                            Project_na %in% c("Decker") ~ "Decker",
                                            Project_na == "Web Tract Berms" ~ "Web Tract Berms",
                                            Project_na %in% c("LHT", "Liberty", "Flyway Farms", "LICB") ~ "Cache Slough")) 
  


#allsites_buff_100m = st_buffer(allsites, 100)

allsites_buff_2k = st_buffer(allsites, 2000)
# 
# samples_100m = samples %>%
#   st_transform(crs = st_crs(allsites)) %>%
#   st_join(allsites_buff_100m) %>%
#   st_drop_geometry() %>%
#   filter(!is.na(Project_na))

priority_buff_2k = st_buffer(PrioritySites, 2000)%>%
  mutate(Region = case_when(Project_na %in% c("Winter", "Browns", "Chipps") ~ "Confluence",
                                             Project_na %in% c("Tule Red", "Ryer") ~ "Grizzly Bay",
                                             Project_na %in% c("Decker") ~ "Decker",
                                             Project_na == "Web Tract Berms" ~ "Web Tract Berms",
                                             Project_na %in% c("LHT", "Liberty", "Flyway Farms", "LICB") ~ "Cache Slough")) 

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = priority_buff_2k, aes(fill = Region), alpha = 0.5)+
  geom_sf(data = PrioritySites)+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38.0, 38.4))



#exterior samples

#filter samples to 2km, then calculate distance and choose the sshorter distance to apply project names

outside_samples = samples %>%
  st_transform(crs = st_crs(allsites)) %>%
  st_join(allsites_buff_2k) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_join(select(WW_Delta, HNAME)) %>% 
filter(!is.na(HNAME)) %>%
  st_transform(crs = st_crs(PrioritySites)) 

outdistances = outside_samples%>%
  st_distance(PrioritySites) %>%
  as.data.frame() %>%
  mutate(SampleID = outside_samples$SampleID)

names(outdistances) = c( PrioritySites$Project_na,"SampleID") 

outdistances = outdistances%>%
  pivot_longer(cols = c(Decker:LICB), names_to = "Project_na", values_to = "Distance") %>%
  group_by(SampleID) %>%
  filter(Distance == min(Distance)) %>%
  distinct()

outside_samples2 = outside_samples  %>%
  st_drop_geometry() %>%
  filter(Project_na %in% PrioritySites$Project_na,
         !SampleID %in% inside_all$SampleID) %>%
  mutate(Type = "Outside",
         Region = case_when(Project_na %in% c("Winter", "Browns", "Chipps") ~ "Confluence",
                            Project_na %in% c("Tule Red", "Ryer") ~ "Grizzly Bay",
                            Project_na %in% c("Decker") ~ "Decker",
                            Project_na == "Web Tract Berms" ~ "Web Tract Berms",
                            Project_na %in% c("LHT", "Liberty", "Flyway Farms", "LICB") ~ "Cache Slough")) %>%
  select(-Project_na) %>%
  distinct() %>%
    left_join(outdistances)


test_oustide = st_as_sf(outside_samples2, coords = c("Longitude", "Latitude"), crs = 4326)
ggplot()+
  geom_sf(data = PrioritySites)+
  geom_sf(data = test_oustide, aes(color = Project_na, shape = Region))


inside_priority =  inside_all%>%
  filter(Project_na %in% PrioritySites$Project_na)

#put them all together

Bugs_spatialfilters = bind_rows(inside_priority, outside_samples2) %>%
  select(-site_type, -Source) %>%
  distinct() %>%
  left_join(select(Allbugs_Mar2026,-Longitude, -Latitude), by = "SampleID")



#it's a many-to-many relationshpi because one of the sample ID's is duplicated and I don't know why.

#remove pre-restoration data
restored_dates = read_excel("data/raw data/Copy of FRP_Restored_Dates.xlsx", na = "na") %>%
  rename(Project_na = Site) %>%
  select(Project_na, `Restoration Date`)

Bugs_allfilters = Bugs_spatialfilters %>%
  left_join(restored_dates) %>%
  filter((Type == "Inside" & (is.na(`Restoration Date`) | Date > `Restoration Date` ) )| Type == "Outside") %>%
  filter(Year > 2016, Year < 2024)

#add distance to nearest breach

breaches = st_read("GIS dta/breaches/Breach_Midpoints_2.shp") %>%
  filter(!is.na(EcologBrea)) %>%
  mutate(BreachNo = c(1:130)) %>%
  st_transform(crs = 4326) %>%
  dplyr::mutate(Longitude = sf::st_coordinates(.)[,1],
                Latitude = sf::st_coordinates(.)[,2])


Bugs_allfilters_sf = Bugs_allfilters %>%
  select(Longitude, Latitude, SampleID) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) 

bdist =Bugs_allfilters_sf  %>%
st_nearest_feature(st_transform(breaches, crs = 4326))

# library(spacetools)
Bugs_allfilters_sf = mutate(Bugs_allfilters_sf, NearestBreach = breaches$BreachNo[bdist])


ggplot()+
  geom_sf(data = spacetools::Delta)+
  geom_sf(data = breaches)+
  coord_sf(xlim = c(-121.57, -122.1), ylim = c(38, 38.4))

breachesbugs = breaches[bdist,]

distancestobreaches = st_distance(Bugs_allfilters_sf, breachesbugs, by_element = T)

Bugs_allfilters_sf = mutate(Bugs_allfilters_sf, DistanceToBreach = distancestobreaches)

Bugs_allfilters = left_join(Bugs_allfilters, Bugs_allfilters_sf)

table(Bugs_allfilters$Region, Bugs_allfilters$Year,  Bugs_allfilters$Type)
#some qc
table(Bugs_allfilters$Project_na, Bugs_allfilters$Year)
#matches my expectstions

#more checks
filtersamples = Bugs_allfilters %>%
  select(Source, Latitude, Longitude, SampleID, Type) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#close up of cache
ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites)+
  geom_sf(data = filtersamples, aes(color = Type))+
  
  coord_sf(xlim = c(-121.7, -121.65), ylim = c(38.280, 38.34))

#close up of confluence
ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites)+
  geom_sf(data = filtersamples, aes(color = Type))+
  
  coord_sf(xlim = c(-121.95, -121.8), ylim = c(38.0, 38.08))

#close up of Tule red

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites)+
  geom_sf(data = filtersamples, aes(color = Type))+
  
  coord_sf(xlim = c(-122.01, -121.95), ylim = c(38.11, 38.14))

#Add habitat types
Bugs_allfilters = Bugs_allfilters %>%
  st_drop_geometry() %>%
  mutate(TowType = replace_values(TowType, "NT" ~ "Neuston")) %>%
  mutate(Habitat = case_when(TowType %in% c("Oblique", "Surface", "Bottom") ~ "Open Water",
                             TowType %in% c("NT", "Neuston") ~ "Surface",
                                         TowType == "SAV" | (TowType == "SN" & str_detect(SampleID, "SAV")) ~ "SAV",
                                         TowType == "EAV" | (TowType == "SN" & str_detect(SampleID, "EAV")) ~ "EAV",
                                         TowType == "FAV" | (TowType == "SN" & str_detect(SampleID, "FAV")) ~ "FAV",
                                         TowType %in% c("Ponar", "PPG", "PVC") ~ "Benthic")) 


#OK, now consolidate by larger taxonomic groups ##########################
#sample level information, to add zeros in later
sample_info = Bugs_allfilters%>%
  select(SampleID, Longitude, Latitude, Region, Project_na, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Volume, Habitat, DistanceToBreach) %>%
  distinct()

Cyclopoids = filter(Bugs_allfilters, Order == "Cyclopoida") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Lifestage,Volume, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info, !TowType  %in% c("PVC", "PPG", "Ponar"))) %>% #remove benthic samples
  mutate(Taxon = "Cyclopoid", CPUE = case_when(is.na(CPUE) ~ 0,
                                               TRUE ~ CPUE))
  

Calanoids = filter(Bugs_allfilters, Order == "Calanoida") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Lifestage,Volume, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
left_join(filter(sample_info, !TowType  %in% c("PVC", "PPG", "Ponar"))) %>% #remove benthic samples
  mutate(Taxon = "Calanoid", CPUE = case_when(is.na(CPUE) ~ 0,
                                               TRUE ~ CPUE))

#why are there more samples with calanoids than cyclopoids?
#oh, we have larval calanoids, no larval cyclopoids

#amphipods - corophiids versus gammarids
Amphipoda = filter(Bugs_allfilters, Order == "Amphipoda") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Family,Volume, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info, !(SizeClass  %in% c("Meso", "Micro") & Source %in% c("EMP", "FMWT", "STN", "20mm")))) %>% #remove zoop samples that don't count amphipods
  mutate(Taxon = "Amphipod", CPUE = case_when(is.na(CPUE) ~ 0,
                                              TRUE ~ CPUE),
         AmphGroup = case_when(Family %in% c("Gammaridae", "Crangonycitidae", "Hyalellidae", "Anisogammaridae", "Gammaroidea") ~ "Gammaridae and friends",
                               Family %in% c("Corophiidae" ) ~ "Corophiidae",
                               TRUE ~ "Other")) %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, AmphGroup,Volume, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T))

#now cladocera

Cladocera = filter(Bugs_allfilters, Class == "Branchiopoda") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Lifestage,Volume, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info, !TowType  %in% c("PVC", "PPG", "Ponar"))) %>% #remove benthic samples
  mutate(Taxon = "Cladocera", CPUE = case_when(is.na(CPUE) ~ 0,
                                              TRUE ~ CPUE))

#insects - probably need to break this down into more categories, but this for now

Insects = filter(Bugs_allfilters, Class == "Insecta") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass,Volume, Family, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info,  !(SizeClass  %in% c("Meso", "Micro") & Source %in% c("EMP", "FMWT", "STN", "20mm")))) %>% #remove zoop samples that don't count amphipods
  mutate(Taxon = "Insect", CPUE = case_when(is.na(CPUE) ~ 0,
                                               TRUE ~ CPUE),
         InsectGroup = case_when(Family == "Chironomidae" ~ "Chironomid",
                               TRUE ~ "Other"))%>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, InsectGroup,Volume, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T))

#Bivalves

Bivalves = filter(Bugs_allfilters, TowType %in% c("PVC", "PPG", "Ponar"), Class == "Bivalvia") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass,Volume, Family, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info,  TowType %in% c("PVC", "PPG", "Ponar"))) %>% 
  mutate(Taxon = "Bivalves", CPUE = case_when(is.na(CPUE) ~ 0,
                                            TRUE ~ CPUE),
         ClamGroup = case_when(Family == "Corbiculidae" ~ "Corbicula",
                               Family == "Corbulidae" ~ "Potamocorbula",
                                 TRUE ~ "Other"))%>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, ClamGroup,Volume, Habitat, DistanceToBreach) %>%
  summarize(CPUE = sum(CPUE, na.rm = T))

#save all outputs ##############################
save(Bugs_allfilters, file = "data/Bugs_allfilters.RData")
save(Bivalves, file = "data/Bivalves.RData")
save(Insects, file = "data/Insects.RData")
save(Calanoids, file = "data/Calanoids.RData")
save(Cyclopoids, file = "data/Cyclopoids.RData")
save(Amphipoda, file = "data/Amphipods.RData")
save(Cladocera, file = "data/Cladocera.RData")

write.csv(Bugs_allfilters, file = "data/Bugs_allfilters.csv", row.names = F)
write.csv(Bivalves, file = "data/Bivalves.csv", row.names = F)
write.csv(Insects, file = "data/Insects.csv", row.names = F)
write.csv(Calanoids, file = "data/Calanoids.csv", row.names = F)
write.csv(Cyclopoids, file = "data/Cyclopoids.csv", row.names = F)
write.csv(Amphipoda, file = "data/Amphipods.csv", row.names = F)
write.csv(Cladocera, file = "data/Cladocera.csv", row.names = F)

# extra datasets to parse zooplankton by functional group ################
#unidentified juvenile copepods are currently excluded, since most of the more abundant species are currently ID'd

badcop = filter(Bugs_allfilters, Order %in%c("Cyclopoida","Calanoida")) %>%
                  filter(Genus%in%c("Acanthocyclops","Tortanus","Acartia","Acartiella"))%>%
                  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
                           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
                           TurbidityFNU, SizeClass, Lifestage,Volume) %>%
                  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
                  left_join(filter(sample_info, !TowType  %in% c("PVC", "PPG", "Ponar"))) %>% #remove benthic samples
                  mutate(Taxon = "PredCop", CPUE = case_when(is.na(CPUE) ~ 0,
                                                             TRUE ~ CPUE))
goodcop = filter(Bugs_allfilters, Order %in%c("Cyclopoida","Calanoida")) %>%
  filter(Genus%in%c("Eurytemora","Pseudodiaptomus","Sinocalanus"))%>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Lifestage,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info, !TowType  %in% c("PVC", "PPG", "Ponar"))) %>% #remove benthic samples
  mutate(Taxon = "NonPredCop", CPUE = case_when(is.na(CPUE) ~ 0,
                                              TRUE ~ CPUE))

weirdcop = filter(Bugs_allfilters, Genus == "Limnoithona") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Lifestage,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info, !TowType  %in% c("PVC", "PPG", "Ponar"))) %>% #remove benthic samples
  mutate(Taxon = "Limnoithona", CPUE = case_when(is.na(CPUE) ~ 0,
                                              TRUE ~ CPUE))

save(goodcop, file = "data/NonPredatoryCopepods.RData")
save(badcop, file = "data/PredatoryCopepods.RData")
save(weirdcop, file = "data/Limnoithona.RData")

write.csv(goodcop, file = "data/NonPredatoryCopepods.csv", row.names = F)
write.csv(badcop, file = "data/PredatoryCopepods.csv", row.names = F)
write.csv(weirdcop, file = "data/Limnoithona.csv", row.names = F)

#exploritory plots of each dataset ################################
load("data/Bivalves.RData")

Biv = st_as_sf(Bivalves, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(Biv) +
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites, aes(fill = Project_na))+
  geom_sf()+
  coord_sf(ylim = c(38, 38.35), xlim = c(-122.1, -121.56))

ggplot(Bivalves, aes(x = as.factor(Year), fill = Type)) + geom_histogram(stat = "count")+
  facet_wrap(~Project_na)

load("data/Amphipods.RData")
Amph = st_as_sf(Amphipoda, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(Amph) +
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites, aes(fill = Project_na))+
  geom_sf()+
  coord_sf(ylim = c(38, 38.35), xlim = c(-122.1, -121.56))

ggplot(Amphipoda, aes(x = as.factor(Year), fill = Type)) + geom_histogram(stat = "count")+
  facet_wrap(~Project_na)

load("data/Calanoids.RData")
Cal= st_as_sf(Calanoids, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(Cal) +
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites, aes(fill = Project_na))+
  geom_sf()+
  coord_sf(ylim = c(38, 38.35), xlim = c(-122.1, -121.56))

ggplot(Calanoids, aes(x = as.factor(Year), fill = Type)) + geom_histogram(stat = "count")+
  facet_wrap(~Project_na)


load("data/Insects.RData")
Ins= st_as_sf(Insects, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(Ins) +
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites, aes(fill = Project_na))+
  geom_sf()+
  coord_sf(ylim = c(38, 38.35), xlim = c(-122.1, -121.56))

ggplot(Insects, aes(x = as.factor(Year), fill = Type)) + geom_histogram(stat = "count")+
  facet_wrap(~Project_na)


#sigh
empben = filter(Allbugs_Oct2025, Source == "EMP_Benthic")

empben_sites = empben %>%
  select(Station, Latitude, Longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

load("~/FoodwebModel/data/PrioritySites.RData")
ggplot()+
  geom_sf(data = priority_buff_2k, fill = "lightblue", alpha = 0.5)+
  geom_sf(data = empben_sites)+
  geom_sf(data = PrioritySites, alpha = 0.5)

insectsX = filter(Bugs_allfilters, Class == "Insecta") %>%
  group_by(Order) %>%
  summarize(CPUE = sum(CPUE))

ephem = filter(Bugs_allfilters, Order == "Ephemeroptera", CPUE !=0) 

Hemiptera = filter(Bugs_allfilters, Order == "Hemiptera", CPUE !=0) 

Diptera = filter(Bugs_allfilters, Order == "Diptera", CPUE !=0) 

ggplot(Diptera) + aes(x = Region, y = CPUE, fill = Family) + geom_col(position = "fill")
