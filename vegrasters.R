#check out shruti's vegetation layers

library(tidyverse)
library(stars)
library(terra)
library(sf)
library(deltamapr)

#first try stars
liberty2024 = read_stars("data/vegetationsubset/LibertyCache_D202409_class_v5_v6_oldmask_sub_tif.tif")
#now try terra
liberty2024a = rast("data/vegetationsubset/LibertyCache_D202409_class_v5_v6_oldmask_sub_tif.tif")

BrownsWinters2024a = rast("data/vegetationsubset/BrownsWinters_D202409_class_v5_v6_oldmask_sub_tif.tif")

vegpal = attributes(liberty2024$LibertyCache_D202409_class_v5_v6_oldmask_sub_tif.tif)$colors
vegpal2 = c("unclassified" = "#000000FF", "soil" = "#B46E46FF",
            "water" = "skyblue","SAV" = "#009696FF", "Emergent"= "#007D00FF","Hyacinth"= "#C864C8FF",
            "Spongeplant" = "#0000FFFF","Primrose"= "#FFFF00FF","NPV"= "#FFA500FF",
            "Riparian" = "green3","Shadow"= "#C8C8C8FF","Marsh Invaded by Primrose" ="#969600FF",
            "Alligatorweed"= "#FFFFFFFF")
# ggplot() +
#   geom_stars(data = liberty2024, aes(fill = LibertyCache_D202409_class_v5_v6_oldmask_sub_tif.tif))+
#   scale_fill_manual(values = attributes(liberty2024$LibertyCache_D202409_class_v5_v6_oldmask_sub_tif.tif)$colors)

#OK, can I get coverage of each vegeation type at each wetland site?


load("GIS dta/wetlandsites.RData")
prioritysites = filter(allsites, Project_na %in% c("LICB", "Rush Ranch", "Decker", "Blacklock", "Browns",
                                                   "Tule Red", "Liberty", "Wings Landing", "Dutch Sl.",
                                                   "Flyway Farms",
                                                   "Lower Yolo Ranch", "Winter")) %>%
  st_transform(crs = st_crs(liberty2024))


#this takes forever and crashes my computer
#SitesWithVeg = st_join(liberty2024, prioritysites, what = "inner")
?extract

SitesWVeg = extract(liberty2024a, prioritysites) %>%
  rename(VegType = LibertyCache_D202409_class_v5_v6_oldmask_sub_tif)

SitesWVegBW = extract(BrownsWinters2024a , prioritysites) %>%
  rename(VegType = BrownsWinters_D202409_class_v5_v6_oldmask_sub_tif)
#oh, much, much better, but weird output I wasn't expecting

SitesWVegsum = bind_rows(SitesWVeg, SitesWVegBW) %>%
  group_by( ID, VegType) %>%
  summarize(Area = n()) %>%
  filter(!is.na(VegType))

SitesWVeg2 = mutate(prioritysites, ID = c(1:nrow(prioritysites))) %>%
  right_join(SitesWVegsum) %>%
  mutate(VegType = factor(VegType, levels = c(0:12), labels = c("unclassified", "soil", "water", "SAV", "Emergent", "Hyacinth",
                                                                "Spongeplant", "Primrose", "NPV", "Riparian", "Shadow", "Marsh Invaded by Primrose",
                                                                "Alligatorweed")))

ggplot(SitesWVeg2, aes(x = Project_na, y = Area, fill = as.factor(VegType)))+
  geom_col()+
  scale_fill_manual(values = vegpal2)

#can i write a function to go through all rasters, label them by year, and join them to site?
#now do it with all sites/years

rastlist <- list.files(path = "data/vegetationsubset/", pattern='.tif$', 
                       all.files=TRUE, full.names=T)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, rast)

#to check the index numbers of all imported raster list elements
names(allrasters[[1]])

vegsites = function(Raster){
  name = names(Raster)
  size = +cellSize(Raster)[1]
  Year = str_extract(name, "\\d{4}")
  veges = extract(Raster, prioritysites) %>%
    rename(VegType = !!name)
  #oh, much, much better, but weird output I wasn't expecting
  
  SitesWVegsum = veges %>%
    group_by( ID, VegType) %>%
    summarize(Area = n()) %>%
    filter(!is.na(VegType))
  
  SitesWVeg2 = mutate(prioritysites, ID = c(1:nrow(prioritysites))) %>%
    right_join(SitesWVegsum) %>%
    mutate(VegType = factor(VegType, levels = c(0:12), labels = c("unclassified", "soil", "water", "SAV", "Emergent", "Hyacinth",
                                                                  "Spongeplant", "Primrose", "NPV", "Riparian", "Shadow", "Marsh Invaded by Primrose",
                                                                  "Alligatorweed")),
           Year = Year)
  return(SitesWVeg2)
}

allveg = lapply(allrasters, vegsites)


#put them together
AllvegForReal = bind_rows(allveg) %>%
  st_drop_geometry() %>%
  mutate(Area = Area$area)

ggplot(AllvegForReal, aes(x = Year, y = Area, fill = VegType))+
  geom_col(position = "fill")+facet_wrap(~Project_na)+
  scale_fill_manual(values = vegpal2)

write.csv(AllvegForReal, "data/AllVegAreas.csv", row.names = F)

AllvegForReal = read_csv("data/AllVegAreas.csv")
#let's just look at the wet area
VegWet = filter(AllvegForReal, VegType %in% c("SAV", "water", "Emergent", "Hyacinth", "Spongeplant", 
                                              "NPV",
                                              "Primrose", "Marsh Invaded by Primrose", "Alligatorweed"))


vegpalWet = c("water" = "skyblue","SAV" = "#009696FF", "Emergent"= "#007D00FF","Hyacinth"= "#C864C8FF",
            "Spongeplant" = "#0000FFFF","Primrose"= "#FFFF00FF", "NPV"= "darkorange",
            "Marsh Invaded by Primrose" ="#969600FF",
            "Alligatorweed"= "#FFFFFFFF")

ggplot(AllvegForReal, aes(x = Year, y = Area, fill = VegType))+
  geom_col()+facet_wrap(~Project_na)+ #, scales = "free_y")+
  scale_fill_manual(values = vegpal2)

ggplot(VegWet, aes(x = Year, y = Area, fill = VegType))+
  geom_col(position = "fill")+facet_wrap(~Project_na)+
  scale_fill_manual(values = vegpalWet)

ggplot(VegWet, aes(x = Year, y = Area, fill = VegType))+
  geom_col()+facet_wrap(~Project_na)+
  scale_fill_manual(values = vegpalWet)


plot(allrasters[[1]])

plot(allrasters[[2]])
plot(allrasters[[3]])
plot(allrasters[[4]])
plot(allrasters[[5]])

cellSize(allrasters[[1]])[2]
