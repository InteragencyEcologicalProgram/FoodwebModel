#maps

library(sf)
library(tidyverse)
library(deltamapr)



load("data/wetlandsites.RData")
load("data/PrioritySites.RData")

ggplot(allsites)+geom_sf()

ggplot(PrioritySites)+geom_sf()


allsites_buff_2k = st_buffer(allsites, 2000)

PrioritySites = PrioritySites%>%
  mutate(Region = case_when(Project_na %in% c("Winter", "Browns", "Chipps") ~ "Confluence",
                            Project_na %in% c("Tule Red", "Ryer") ~ "Grizzly Bay",
                            Project_na %in% c("Decker") ~ "Decker",
                            Project_na == "Web Tract Berms" ~ "Web Tract Berms",
                            Project_na %in% c("LHT", "Liberty", "Flyway Farms", "LICB") ~ "Cache Slough")) 

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

#OK, now I just need the waterways bwithin 2k of the sites

#and remove the site iteself

WW_Delta2 = st_transform(WW_Delta, crs = st_crs(PrioritySites))

buff_2k1 = st_difference(priority_buff_2k, PrioritySites) %>%
  filter(Project_na == Project_na.1) %>%
  st_intersection(WW_Delta2) %>%
  select(Project_na, Region, HNAME, site_type)

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = buff_2k1, aes(fill = Region))+
#  geom_sf(data = PrioritySites)+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38.0, 38.4))


ggplot()+
  geom_sf(data = WW_Delta)+
  #geom_sf(data = buff_2k1, aes(fill = Region), alpha = 0.5)+
  geom_sf(data = PrioritySites, aes(fill = Region))+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38.0, 38.4))

#I think i need a lHT that isn't just the water. Also veteteated plain

library(leaflet)

map <- leaflet() %>%
  addPolygons(data = WW_Delta, label = ~HNAME)
map
LHT2 = st_read("data/LHT/LHT.shp") 
LHT2 = LHT2  %>%
  mutate(Region = "Cache Slough") %>%
  select(Project_na, Region, site_type)

PrioritySites = filter(PrioritySites, Project_na != "LHT") %>%
  bind_rows(LHT2) %>%
  st_make_valid()
allsites = filter(allsites, Project_na != "LHT") %>%
  bind_rows(LHT2) %>%
  st_make_valid()

save(allsites, file = "data/wetlandsites.RData")
save(PrioritySites, file = "data/PrioritySites.RData")

st_write(PrioritySites, "GIS dta/PrioritySites.shp")
st_write(buff_2k1, "GIS dta/Exteriors.shp")


#figure out where EMP samples are going
load('data/AllWetlandBugs_2010onwards.RData')
load("data/Insects.RData")
load("data/Bivalves.RData")

unique(Bivalves$Source)


EMPben = filter(Allbugs_Mar2026, Source == "EMP_benthic")
EMPben_sites = select(EMPben, Station, Latitude, Longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove =F)
