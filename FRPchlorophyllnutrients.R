#get something resembling GPS coordinates for FRP stations

library(tidyverse)
library(discretewq)
library(sf)
library(deltamapr)
load("GIS dta/wetlandsites.RData")
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.269.5
wq_all = wq(Sources = c("20mm", "Baystudy", "DJFMP", "EMP", "FMWT", "NCRO", "EDSM", "SLS",
                        "STN", "Suisun", "USBR", "USGS_CAWSC", "USGS_SFBS", "YBFMP")) %>%
  filter(year(Date)>2009)

FRP_nutrients = read_csv("data/nutrients_FRP2023.csv")
FRP_fish =  read_csv("data/fish_FRP2023.csv")
FRP_Mac =  read_csv("data/macroinvert_FRP2023.csv")
visits = read_csv("data/sitevisit_FRP2023.csv")

Locationssf = bind_rows(FRP_fish, FRP_Mac) %>%
  group_by(Location) %>%
  summarize(Latitude = mean(LatitudeStart, na.rm =T), Longitude = mean(LongitudeStart, na.rm =T)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)


Locations = st_drop_geometry(Locationssf)

prioritysites = filter(allsites, Project_na %in% c("LICB", "Rush Ranch", "Decker", "Blacklock",
                                                   "Tule Red", "Liberty", "Wings Landing", "Dutch Sl.",
                                                   "Flyway Farms",
                                                   "Lower Yolo Ranch", "Browns"))

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = prioritysites, fill = "green")+
  geom_sf(data = Locationssf) +
  coord_sf(xlim = c(-122.2, -121.5), ylim = c(38, 38.4))



test = left_join(FRP_nutrients, visits) %>%
  left_join(Locations)|>
  dplyr::summarise(n = dplyr::n(), .by = c(VisitNo, SampleID_frp, Date, SampleType, ParentSample, Flag1Blank, Flag1Sample,
                                           Flag2Sample, Location, LatitudeStart, LongitudeStart, StartTime, LAB_NAME, Comments, Temp, SC, pH, DO, Turbidity,
                                           Chlorophyll, Phycocyanin, FDOM, Secchi, Microcystis, Tide, Weather, WindWaves, Flagged_Data, SiteComments, Latitude,
                                           Longitude, geometry, Analyte)) |>
  dplyr::filter(n > 1L) 

test2 = filter(FRP_nutrients, SampleID_frp %in% test$SampleID_frp)
#huh. Some have two values. I dunno why.

FRP_nuts = left_join(FRP_nutrients, visits) %>%
  left_join(Locations) %>%
  filter(SampleType == "Normal Sample") %>%
  select(-RptLimit, -Units, -Flag1Sample, -Flag1Blank, -Flag2Sample) %>%
  mutate(Source = "FRP", Sign = case_when(str_detect(Result, "<") ~ "<",
                                          TRUE ~ "="),
         Result = as.numeric(str_remove(Result, "<"))) %>%
  pivot_wider(names_from = Analyte, values_from = c(Result, Sign), values_fn = list(Result = mean, Sign = first)) %>%
  select(SampleID = SampleID_frp, Source, "Station" = Location, Latitude, Longitude, Date, Tide, Secchi, 
         Temperature = Temp, Conductivity = SC,
         TurbidityFNU = Turbidity, pH,
         Notes = SiteComments, DissolvedOxygen = DO, Microcystis, Chlorophyll_sonde = Chlorophyll,
         Chlorophyll_Sign = `Sign_Chlorophyll a`,
         Chlorophyll = `Result_Chlorophyll a`,  
         Pheophytin_Sign = `Sign_Pheophytin a`,
         Pheophytin = `Result_Pheophytin a`,
         DisAmmonia_Sign = `Sign_Dissolved Ammonia`,
         DisAmmonia = `Result_Dissolved Ammonia`,
         DissOrthophos = `Result_Dissolved ortho-Phosphate`,
         DissOrthophos_Sign = `Sign_Dissolved ortho-Phosphate`,
         DissNitrateNitrite = `Result_Dissolved Nitrate + Nitrite`,
         DissNitrateNitrite_Sign = `Sign_Dissolved Nitrate + Nitrite`,
         TotPhos= `Result_Total Phosphorus`,
         TotPhos_Sign = `Sign_Total Phosphorus`,
         TOC= `Result_Total Organic Carbon`,
         TOC_Sign = `Sign_Total Organic Carbon`,
         
         DOC= `Result_Dissolved Organic Carbon`,
         DOC_Sign = `Sign_Dissolved Organic Carbon`,
         
         TKN= `Result_Total Kjeldahl Nitrogen`,
         TKN_Sign = `Sign_Total Kjeldahl Nitrogen`,
         DON = `Result_Dissolved Organic Nitrogen`,
         DON_Sign = `Sign_Dissolved Organic Nitrogen`)

write.csv(FRP_nuts, "data/FRP_nutrients.csv", row.names = F)
