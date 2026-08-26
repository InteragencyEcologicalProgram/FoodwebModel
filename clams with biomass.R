#clam data with biomass

library(tidyverse)
library(sf)
library(deltamapr)

load("Data/wetlandclams.RData")


load("data/wetlandsites.RData")
load("data/PrioritySites.RData")

wetlandclams = wetlandclams%>%
  mutate(SampleID = paste(Source, Date, Latitude),
         Longitude = case_when(Longitude > 1 ~ Longitude*-1,
                               TRUE ~ Longitude))

clamsites = select(wetlandclams, Source, Date, Latitude, Longitude) %>%
  mutate(SampleID = paste(Source, Date, Latitude)) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove =F)

ggplot(WW_Delta)+
  geom_sf()+
  geom_sf(data = allsites_buff_2k )+
  geom_sf(data = clamsites, aes(color = Source))

inside_clams = clamsites  %>%
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


#exterior samples

#filter samples to 2km, then calculate distance and choose the sshorter distance to apply project names

outside_clamsamples = clamsites %>%
  st_transform(crs = st_crs(allsites)) %>%
  st_join(allsites_buff_2k) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_join(select(WW_Delta, HNAME)) %>% 
  filter(!is.na(HNAME)) %>%
  st_transform(crs = st_crs(PrioritySites)) 


outdistances = outside_clamsamples%>%
  st_distance(PrioritySites) %>%
  as.data.frame() %>%
  mutate(SampleID = outside_clamsamples$SampleID)

names(outdistances) = c( PrioritySites$Project_na,"SampleID") 

outdistances = outdistances%>%
  pivot_longer(cols = c(Decker:LICB), names_to = "Project_na", values_to = "Distance") %>%
  group_by(SampleID) %>%
  filter(Distance == min(Distance)) %>%
  distinct()

outside_clamsamples2 = outside_clamsamples  %>%
  st_drop_geometry() %>%
  filter(Project_na %in% PrioritySites$Project_na,
         !SampleID %in% inside_clams$SampleID) %>%
  mutate(Type = "Outside",
         Region = case_when(Project_na %in% c("Winter", "Browns", "Chipps") ~ "Confluence",
                            Project_na %in% c("Tule Red", "Ryer") ~ "Grizzly Bay",
                            Project_na %in% c("Decker") ~ "Decker",
                            Project_na == "Web Tract Berms" ~ "Web Tract Berms",
                            Project_na %in% c("LHT", "Liberty", "Flyway Farms", "LICB") ~ "Cache Slough")) %>%
  select(-Project_na) %>%
  distinct() %>%
  left_join(outdistances)


#put them all together

clams_spatialfilters = bind_rows(inside_clams, outside_clamsamples2) %>%
  select(-site_type, -Source) %>%
  distinct() %>%
  left_join(select(wetlandclams,-Longitude, -Latitude, -Date), by = "SampleID") %>%
  filter(Project_na %in% PrioritySites$Project_na)

table(clams_spatialfilters$Project_na, clams_spatialfilters$Type)

#it's a many-to-many relationshpi because one of the sample ID's is duplicated and I don't know why.

#remove pre-restoration data
restored_dates = read_excel("data/raw data/Copy of FRP_Restored_Dates.xlsx", na = "na") %>%
  rename(Project_na = Site) %>%
  select(Project_na, `Restoration Date`) %>%
  mutate(RestoredYear = year(`Restoration Date`))

clams_allfilters = clams_spatialfilters %>%
  left_join(restored_dates) %>%
  mutate(Year = case_when(is.na(Year) ~ year(Date),
                          TRUE ~Year)) %>%
  filter((Type == "Inside" & (is.na(`Restoration Date`) | Year >= RestoredYear) )| Type == "Outside") %>%
  filter(Year > 2016, Year < 2024)

table(clams_allfilters$Project_na, clams_allfilters$Type)

save(clams_allfilters, file = "data/clams_withbiomass.RData")


ggplot(clams_allfilters, aes(x = Project_na, y = Biomass, fill = Species)) + geom_boxplot() +
  facet_wrap(~Type)

