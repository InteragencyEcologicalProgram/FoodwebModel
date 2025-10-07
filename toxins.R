#microcystis sites for Ellen

library(tidyverse)
library(sf)
library(deltamapr)
library(stars)

toxins = read_csv("data/toxins_8.22.25_forarcmap.csv")
wetlands = st_read("GIS dta/DARIwetlands.shp") %>%
  filter(Tidal != "Nontidal") %>%
  mutate(Habitat = factor(WetlandTyp, levels = c("CV", "TC", "TCU", "TGPOWU", "TP", "TV", "TVw"),
                          labels = c("Vegetated channel", "Tidal channel", "Tidal channel (unnatural)", "Flooded Island",
                                     "Tidal Marsh Panne", "Vegetated tidal wetland", "Vegetated tidal wetland (woody)")))

toxinssf = st_as_sf(toxins, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)


ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = wetlands, aes(fill = Habitat), alpha = 0.5)+
  geom_sf(data = toxinssf, aes(shape = habitat), size =3)+
  geom_sf_text(data = toxinssf, aes(label = site), nudge_x = 0.002, nudge_y = 0.002,hjust =0)+
  scale_fill_manual(values = c("green", "skyblue", "skyblue3", "cyan2", "darkgreen", "green3", "olivedrab"))+
  coord_sf(xlim = c(-121.65, -121.7), ylim = c(38.23, 38.34))

bathy = read_stars("data/dem_bay_delta_10m_20250312.tif")

bbox = st_bbox(c(xmin = -121.65, xmax = -121.7, ymin = 38.23, ymax = 38.34), crs = 4326) %>%
  st_transform(crs = st_crs(bathy))

 bathy2 = bathy%>%
  st_crop(bbox)

ggplot()+
  geom_stars(data = bathy2)+
  geom_sf(data = toxinssf, aes(shape = habitat), size =3)+
  coord_sf(xlim = c(-121.65, -121.7), ylim = c(38.23, 38.34))

toxins2 = st_join(bathy2, st_transform(toxinssf, crs = st_crs(bathy)), what = "inner")
toxins2a =rename(toxins2, Depth = dem_bay_delta_10m_20250312.tif)
