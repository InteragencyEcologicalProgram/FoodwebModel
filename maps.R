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
  coord_sf(xlim = c(-122.88, -121.82), ylim = c(38.02, 38.4))


ggplot()+
  geom_sf(data = WW_Delta)+
  #geom_sf(data = buff_2k1, aes(fill = Region), alpha = 0.5)+
  geom_sf(data = PrioritySites, aes(fill = Region))+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38.0, 38.4))

#I think i need a lHT that isn't just the water. Also veteteated plain###############

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


#figure out where EMP samples are going ############################
load('data/AllWetlandBugs_2010onwards.RData')
load("data/Insects.RData")
load("data/Bivalves.RData")

unique(Bivalves$Source)


EMPben = filter(Allbugs_Mar2026, Source == "EMP_benthic")
EMPben_sites = select(EMPben, Station, Latitude, Longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove =F)

#breach locations ###############################

breaches = st_read("GIS dta/breaches/Breach_Midpoints_2.shp") %>%
  filter(!is.na(EcologBrea)) %>%


ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites)+
  geom_sf(data = breaches)+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38.0, 38.4))

#winter

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites)+
  geom_sf(data = breaches)+
  coord_sf(xlim = c(-121.88, -121.82), ylim = c(38.02, 38.06))

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites)+
  geom_sf(data = breaches)+
  coord_sf(xlim = c(-121.88, -121.82), ylim = c(38.02, 38.06))

#vegetation shapefile from Bailey

vegshp = st_read("GIS dta/boundary_w_ss_yolo_07152026/boundary_w_ss_yolo_07152026.shp")


ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = allsites,fill = "orange")+
  geom_sf(data = vegshp, alpha = 0.3, fill = "green3")+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.78, 38.7))

#shapefiles from Shruti
#oh, this is just what I gave here, minus the suisun sites
extshr = st_read("GIS dta/Exteriors_Delta_UTM/Exteriors_Delta_UTM.shp")
intshr = st_read("GIS dta/PrioritySites_Delta_UTM/PrioritySites_Delta_UTM.shp")
ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = intshr,fill = "orange")+
  geom_sf(data = extshr, alpha = 0.3, fill = "green3")+
  coord_sf(xlim = c(-122.0, -121.5), ylim = c(38, 38.4))


vegarea1 = read_csv("GIS dta/RH_Exteriors_area_m2_2026-07-22.csv") %>%
  mutate(Type = "Outside")

vegarea2 = read_csv("GIS dta/RH_PrioritySites_area_m2_2026-07-22.csv") %>%
  mutate(Type = "Inside")

vegarea = bind_rows(vegarea1, vegarea2) %>%
  pivot_longer(cols = c(water, SAV, whyacinth, spongeplant, emergent, NPV, wprimrose, shadow, EMPR, 
                        alligweed, pennywort, FAV, land, soil, riparian), names_to = "VegType", values_to = "Area")

mypal = c("green3", "yellowgreen", "darkgreen", "orange", "tan", "gold", "seagreen", "lightgreen", 
          "darkolivegreen", "grey", "goldenrod4", "cyan3", "lightblue", "purple", "yellow")
ggplot(vegarea, aes(x = yyyy, y = Area, fill = VegType)) + geom_area(position = "fill")+
  facet_grid(Type~proj_name)+ scale_fill_manual(values = mypal)

ggplot(vegarea, aes(x = yyyy, y = Area, fill = VegType)) + geom_area()+
  facet_grid(Type~proj_name)+ scale_fill_manual(values = mypal)+
  geom_hline(aes(yintercept = ttlSRarea))

vegarea_bg = filter(vegarea, VegType %in% c("water", "SAV","land", "emergent", "NPV", "shadow",  
                                             "FAV", "soil", "riparian"))

vegpal = c("water" = "lightblue", "FAV" = "green3", "emergent"= "darkgreen", land = "tan",NPV =  "gold", "SAV" = "lightgreen", 
                   "riparian" =  "darkolivegreen","shadow"= "grey","soil"= "goldenrod4")


ggplot(vegarea_bg, aes(x = yyyy, y = Area, fill = VegType)) + geom_area()+
  facet_grid(Type~proj_name)+ scale_fill_manual(values = vegpal)+
  geom_point(aes(y = ttlSRarea))

ggplot(vegarea_bg, aes(x = yyyy, y = Area, fill = VegType)) + geom_area(position = "fill")+
  facet_grid(Type~proj_name)+ scale_fill_manual(values = vegpal)

test = vegarea_bg  %>%
  group_by(yyyy, VegType) %>%
  summarize(tot = sum(Area, na.rm =T))

