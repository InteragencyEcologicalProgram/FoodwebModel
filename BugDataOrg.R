#Bug data organization and integration

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

#exterior samples

outside_samples = samples %>%
  st_transform(crs = st_crs(allsites)) %>%
  st_join(allsites_buff_2k) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_join(select(WW_Delta, HNAME)) %>% 
filter(!is.na(HNAME)) %>%
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
  distinct()


test_oustide = st_as_sf(outside_samples, coords = c("Longitude", "Latitude"), crs = 4326)
ggplot()+
  geom_sf(data = PrioritySites)+
  geom_sf(data = test_oustide, aes(color = Region))


inside_priority =  inside_all%>%
  filter(Project_na %in% PrioritySites$Project_na)

#put them all together

Bugs_spatialfilters = bind_rows(inside_priority, outside_samples) %>%
  select(-site_type, -Source) %>%
  distinct() %>%
  left_join(select(Allbugs_Mar2026,-Longitude, -Latitude), by = "SampleID")



#it's a many-to-many relationshpi because one of the sample ID's is duplicated and I don't know why.

#remove pre-restoration data
restored_dates = read_excel("data/Copy of FRP_Restored_Dates.xlsx", na = "na") %>%
  rename(Project_na = Site) %>%
  select(Project_na, `Restoration Date`)

Bugs_allfilters = Bugs_spatialfilters %>%
  left_join(restored_dates) %>%
  filter((Type == "Inside" & (is.na(`Restoration Date`) | Date > `Restoration Date` ) )| Type == "Outside") %>%
  filter(Year > 2016, Year < 2024)

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

#OK, now consolidate by larger taxonomic groups ##########################
#sample level information, to add zeros in later
sample_info = Bugs_allfilters%>%
  select(SampleID, Longitude, Latitude, Region, Project_na, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Volume) %>%
  distinct()

Cyclopoids = filter(Bugs_allfilters, Order == "Cyclopoida") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Lifestage,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info, !TowType  %in% c("PVC", "PPG", "Ponar"))) %>% #remove benthic samples
  mutate(Taxon = "Cyclopoid", CPUE = case_when(is.na(CPUE) ~ 0,
                                               TRUE ~ CPUE))
  
foo = filter(Bugs_allfilters, !SampleID  %in% Cyclopoids$SampleID)

Calanoids = filter(Bugs_allfilters, Order == "Calanoida") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Lifestage,Volume) %>%
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
           TurbidityFNU, SizeClass, Family,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info, !(SizeClass  %in% c("Meso", "Micro") & Source %in% c("EMP", "FMWT", "STN", "20mm")))) %>% #remove zoop samples that don't count amphipods
  mutate(Taxon = "Amphipod", CPUE = case_when(is.na(CPUE) ~ 0,
                                              TRUE ~ CPUE),
         AmphGroup = case_when(Family %in% c("Gammaridae", "Crangonycitidae", "Hyalellidae", "Anisogammaridae", "Gammaroidea") ~ "Gammaridae and friends",
                               Family %in% c("Corophiidae" ) ~ "Corophiidae",
                               TRUE ~ "Other")) %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, AmphGroup,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T))

#now cladocera

Cladocera = filter(Bugs_allfilters, Class == "Branchiopoda") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, Lifestage,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info, !TowType  %in% c("PVC", "PPG", "Ponar"))) %>% #remove benthic samples
  mutate(Taxon = "Cladocera", CPUE = case_when(is.na(CPUE) ~ 0,
                                              TRUE ~ CPUE))

#insects - probably need to break this down into more categories, but this for now

Insects = filter(Bugs_allfilters, Class == "Insecta") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass,Volume, Family) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info,  !(SizeClass  %in% c("Meso", "Micro") & Source %in% c("EMP", "FMWT", "STN", "20mm")))) %>% #remove zoop samples that don't count amphipods
  mutate(Taxon = "Insect", CPUE = case_when(is.na(CPUE) ~ 0,
                                               TRUE ~ CPUE),
         InsectGroup = case_when(Family == "Chironomidae" ~ "Chironomid",
                               TRUE ~ "Other"))%>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, InsectGroup,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T))

#Bivalves

Bivalves = filter(Bugs_allfilters, TowType %in% c("PVC", "PPG", "Ponar"), Class == "Bivalvia") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass,Volume, Family) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  left_join(filter(sample_info,  TowType %in% c("PVC", "PPG", "Ponar"))) %>% 
  mutate(Taxon = "Bivalves", CPUE = case_when(is.na(CPUE) ~ 0,
                                            TRUE ~ CPUE),
         ClamGroup = case_when(Family == "Corbiculidae" ~ "Corbicula",
                               Family == "Corbulidae" ~ "Potamocorbula",
                                 TRUE ~ "Other"))%>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, ClamGroup,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T))

#save all outputs ##############################

