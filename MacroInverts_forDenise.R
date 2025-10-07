#just load the macroinvertebrate data for Denise.

library(tidyverse)
library(sf)
library(zooper)


#bring in wetland data and subset to sites we decided to prioritize
load("data/wetlandsites.RData")
prioritysites = filter(allsites, Project_na %in% c("LICB", "Rush Ranch", "Decker", "Blacklock",
                                                   "Tule Red", "Liberty", "Wings Landing", "Dutch Sl.",
                                                   "Flyway Farms","Browns",
                                                   "Lower Yolo Ranch"))

#buffer by 500 m so we capture data collected outside the site
prioritysitesbuffer = st_buffer(prioritysites, 500)

Allsitesbuffer = st_buffer(allsites, 1000)


#load bug data
load("data/AllWetlandBugs.RData")
names(AllBugs)

#calculate CPUE for sweep nets
AllBugs = AllBugs %>% mutate(Volume = case_when(TowType %in% c("SN", "EAV", "SAV", "FAV") ~ 0.3,
                                                TRUE ~ Volume),
                             CPUE = case_when(TowType %in% c("SN", "EAV", "SAV", "FAV") ~ CPUE/0.3,
                                              TRUE ~ CPUE))

#some of the logitude values were off, fix those and join in project name
AllBugsx = mutate(AllBugs, Longitude = case_when(Longitude >1 ~ Longitude *-1,
                                                 TRUE ~ Longitude))%>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)  %>%
  st_join(prioritysitesbuffer) 


#got to add in zeros and add vegetation type
Taxonomy = zooper::crosswalk %>%
  select(Taxname, Phylum, Class, Order, Family, Genus, Species) %>%
  distinct()

AllbugsWetlands = AllBugsx %>%
  st_drop_geometry() %>%
  filter(!is.na(Project_na), SizeClass != "Micro",
         !(Source == "YBFMP" & Project_na == "Lower Yolo Ranch")) %>% #keep YBFMP dta with Flyway, not lower yolo
  
  mutate( VegType = case_when(str_detect(SampleID, "FAV") ~ "Floating",
                              str_detect(SampleID, "EAV") ~ "Emergent",
                              str_detect(SampleID, "SAV") ~ "Submersed",
                              TowType %in% c("PPG", "PVC", "Ponar") ~ "Benthic",
                              TowType %in% c("NT", "Neuston") ~ "Neuston/drift",
                              TRUE ~ "Plankton")) %>%
  pivot_wider(id_cols = c(Source, Date, Latitude, Longitude, Station, SalSurf, TowType, SampleID, Year, Month, 
                          Project_na, site_type, VegType), names_from = Taxname, values_from = CPUE, values_fn = sum,
              values_fill = 0) %>%
  pivot_longer(cols = c("Copepoda":last_col()), names_to = "Taxname", values_to = "CPUE") %>%
  left_join(Taxonomy) %>%
  filter(!(Source == "Dutch Slough" & CPUE ==0)) #dutch slough didn't look at most stuff, so don't include all the zeros
