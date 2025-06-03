#mapping

library(sf)
library(deltamapr)
library(tidyverse)
library(discretewq)
library(cder)

load("GIS dta/wetlandsites.RData")
prioritysites = filter(allsites, Project_na %in% c("LICB", "Rush Ranch", "Decker", "Blacklock",
                                                   "Tule Red", "Liberty", "Wings Landing", "Dutch Sl.",
                                                   "Flyway Farms",
                                                   "Lower Yolo Ranch"))

prioritysitesbuffer = st_buffer(prioritysites, 1000)

#phytoplankton data
#FRP phytos
FRPphyto = read_csv("https://pasta.lternet.edu/package/data/eml/edi/269/5/5a9a37ecf7fe110f8b69a2cea62b9b89" )

FRPphytopoints = select(FRPphyto, Location, SampleID_frp, Substrate, LatitudeStart, LongitudeStart, Date, VisitNo) %>%
  distinct() %>%
  filter(!is.na(LatitudeStart)) %>%
  st_as_sf(coords = c("LongitudeStart", "LatitudeStart"), crs = 4326) %>%
  mutate(Year = year(Date), Source = "FRP")

#EMP phytos

phytosQ = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1320/9/effebf596630b9b9bf4c01718f1cf93c")
EMPphytopoints = select(phytosQ, Station, Date, Latitude, Longitude) %>%
  distinct() %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Year = year(Date), Source = "EMP")

#USGS phytos
#little holland tract
#https://www.sciencebase.gov/catalog/item/5d1ba85fe4b0941bde621071
fluxsamples = read_csv("data/FLUX_Sample_Table.csv", locale=locale(encoding="latin1")) %>%
  filter(!is.na(`Latitude Start WGS 84`)) %>%
  select(`Sample Date`, `Sample Number`, `Latitude Start WGS 84`,`Longitude Start WGS 84`, `Chlorophyll µg/L`) 

fluxphytos = read_csv("data/FLUX_Phytoplankton_Table.csv")
fluxpoints = fluxphytos %>%
  select(`Sample Date`, `Sample Number`) %>%
  left_join(fluxsamples)%>%
  filter(!is.na(`Latitude Start WGS 84`)) %>%
  st_as_sf(coords = c("Longitude Start WGS 84", "Latitude Start WGS 84"), crs = 4326) %>%
  mutate(Date = mdy(`Sample Date`), Year = year(Date), Source = "USGS wetlands")

#Main estuary monitoring 2014-2018 - are there more recent updates?
#https://www.sciencebase.gov/catalog/item/598b755de4b09fa1cb0eadd9 
USGS2014_2016 = read_csv("data/Phytoplankton_San_Francisco_Bay_2014_2016.csv")
USGS2017_2018 = read_csv("data/Phytoplankton_San_Francisco_Bay_2017-2018.csv")
USGSstations = read_csv("data/SFBay_TableofStationLocations.csv") %>%
  mutate(latminutes = as.numeric(str_remove(`North Latitude Minutes`, "'")),
         longminutes = as.numeric(str_remove(`West Longitude Minutes`, "'")),
         Latitude = `North Longitude Degrees`+ latminutes/60,
         Longitude =(`West Longitude Degrees`+ -1*longminutes/60)) %>%
  filter(str_detect(Comments, "earlier", negate = TRUE)|is.na(Comments))

USGSphytopoints = bind_rows(USGS2014_2016, USGS2017_2018) %>%
  select(`Station Number`, Date) %>%
  distinct() %>%
  left_join(USGSstations)  %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Date = mdy(Date), Year = year(Date), Source = "USGS SFBS")

#SMSCG phytoplankton
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.876.8
SMSCGphyto = read_csv("data/smscg_phytoplankton_samples_2020-2023.csv")
SMSCGstations = select(SMSCGphyto, station, latitude, longitude, date) %>%
  distinct() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(Year = year(date), Source = "SMSCG")


allphyto = bind_rows(SMSCGstations, USGSphytopoints, EMPphytopoints, FRPphytopoints, fluxpoints)
phytowetlands = st_join(allphyto, prioritysitesbuffer) %>%
  filter(!is.na(Project_na))
###################
#DOP phytoplankton- not online anywhere?

#all phytoplankton
ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = prioritysites, fill = "green") +
  geom_sf(data = allphyto, aes(shape = Source, color = Source))+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38, 38.4)) +
  facet_wrap(~Year)


#phytoplankton in/near weltands
ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = prioritysites, fill = "green") +
  geom_sf(data = phytowetlands, aes(shape = Source, color = Source))+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38, 38.4)) +
  facet_wrap(~Year)

#epiphytic and epibenthci algae (just FRP?)

ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = prioritysites, fill = "green") +
  geom_sf(data = filter(FRPphytopoints, Substrate != "pelagic"), shape = 22, fill = "yellow")+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38, 38.4)) +
  facet_wrap(~Year)


########################
#chlorophyll grab samples

chla = wq(Sources = c("EMP", "DOP", "NCRO", "USGS_SFBS", "USGS_CAWSC", "YBFMP"), Start_year = 2010) %>%
  filter(!is.na(Chlorophyll), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Year = year(Date))

#FRP isn't in there, but need to wait until their next data update for lab data anyway. 
#but we have some sonde data
FRPfish = read_csv("data/fish_FRP2022.csv") %>%
  select(SampleID_frp, VisitNo, LatitudeStart,LongitudeStart) %>%
  distinct() %>%
  filter(!is.na(LatitudeStart), !is.na(LongitudeStart)) %>%
  st_as_sf(coords = c("LongitudeStart", "LatitudeStart"), crs = 4326)
  
samples = bind_rows(FRPphytopoints, FRPfish)  
  
  
FRPchl = samples %>%
  left_join(read_csv("data/sitevisit_FRP2022.csv") ) %>%
  filter(!is.na(Chlorophyll)) %>%
  mutate(Year = year(Date), Source = "FRP")


#data from NERR
#this is all the discrete and continuous data from all stations
RR = read_csv("data/413436.csv", guess_max = 10000)
RRstations = read_csv("data/sampling_stations.csv") %>%
  filter(`NERR Site ID`== "sfb", `Station Name` %in% c("First Mallard","Second Mallard", "Rush Ranch")) %>%
  select(`Station Code`, `Station Name`, `Latitude`, `Longitude`) 

#the main file is in wide format, one column for each station and each parameter. Sigh. 
RRchla = select(RR, DateTimeStamp, contains("CHLA_N")) %>%
  select(-"SFBCCNUT_CHLA_N", -"SFBCCNUT_F_CHLA_N", -"SFBFMNUT_F_CHLA_N", -"SFBGCNUT_F_CHLA_N", -"SFBSMNUT_F_CHLA_N", -"SFBGCNUT_CHLA_N")%>%
  pivot_longer(cols = c(SFBFMNUT_CHLA_N:SFBSMNUT_CHLA_N), names_to = "Station", values_to = "chla") %>%
  filter(!is.na(chla)) %>%
  mutate(Date = mdy_hm(DateTimeStamp), Year = year(Date), `Station Code` = tolower(str_sub(Station, 1, 8))) %>%
  left_join(RRstations) %>%
  mutate(Longitude = -1*as.numeric(Longitude), Source = "NERR")%>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

Allchla = bind_rows(chla, RRchla, select(FRPchl, -Microcystis), fluxpoints)
AllchlaWetlands = st_join(Allchla, prioritysitesbuffer)%>%
  filter(!is.na(Project_na))


ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = prioritysites, fill = "green") +
  geom_sf(data = Allchla, aes(color = Source, fill = Source, shape = Source))+
scale_shape_manual(values = c(1,2,15,16,21,22,23,24,25))+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38, 38.4))+
  facet_wrap(~Year)

ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = prioritysites, fill = "green") +
  geom_sf(data = AllchlaWetlands, aes(color = Source, fill = Source, shape = Source))+
  scale_shape_manual(values = c(1,2,15,16,21,22,23,24,25))+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38, 38.4))+
  facet_wrap(~Year)


######################
#chlorophyll sondes

#NWIS sondes
library(dataRetrieval)
ch = filter(parameterCdFile, str_detect(parameter_nm, "chlorophyll"))
fdom = filter(parameterCdFile, str_detect(parameter_nm, "fDOM"))
siteListPhos <- whatNWISsites(parameterCd = ch$parameter_cd, bBox = c(-122.1, 38, -121.6, 38.4))


siteListfdom <- whatNWISsites(parameterCd = fdom$parameter_cd, bBox = c(-122.1, 38, -121.6, 38.4))

NWISchl = siteListPhos %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326)


NWISfdom = siteListfdom %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326)

#the cdec sondes
chlsondes = read_csv("data/CDEC_w_chl.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = prioritysites, fill = "green") +
  geom_sf(data = chlsondes, color = "orange", size =3)+
  geom_sf(data = NWISchl, color = "red", size =3)+
  geom_sf(data = RRchla, shape = 22, fill = "blue", size = 4)+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38, 38.4))


##############################
#doc and fdom
frpnuts = read_csv("data/nutrients_FRP2023.csv") %>%
  filter(Analyte == "Dissolved Organic Carbon", !is.na(LongitudeStart))%>%
  mutate(Year = year(Date))%>%
  st_as_sf(coords = c("LongitudeStart", "LatitudeStart"), crs = 4326)


DOC = wq(Sources = c("EMP", "DOP", "NCRO", "USGS_SFBS", "USGS_CAWSC", "YBFMP"), Start_year = 2010) %>%
  filter(!is.na(DOC), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Year = year(Date))

ggplot()+
  geom_sf(data = WW_Delta) +
  geom_sf(data = prioritysites, fill = "green") +
  geom_sf(data = DOC, color = "orange", size =3)+
  geom_sf(data = frpnuts, color = "yellow", size =3)+
  geom_sf(data = NWISfdom, color = "red", size =3)+
  coord_sf(xlim = c(-122.1, -121.6), ylim = c(38, 38.4))




################DOC###########################
#vegetation

#https://knb.ecoinformatics.org/view/doi%3A10.5063%2FF1HH6HJX
library(terra)
library(stars)

filenames <- list.files("data/vegetation", pattern="*.tif", full.names=TRUE)
vegdf <- lapply(filenames, read_stars)

ggplot()+
  geom_stars(data = vegdf[[1]])
#Error: cannot allocate vector of size 2.3 Gb
#damn it

#try just the wetlands

wetlandveg = lapply(vegdf, function(x){
  x = st_transform(x, crs = st_crs(prioritysites))
  y = st_crop(x, prioritysites)
  return(y)
  })
#OMG THEY HAVE DIFFERENT CRS? WTF??
#Error: cannot allocate vector of size 8.0 Gb