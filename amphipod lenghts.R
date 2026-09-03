#amphipod lengths

library(tidyverse)
library(readxl)

amphlengths = read_csv("data/amphlenghts.txt") %>%
  mutate(Month = month(Date))
ggplot(amphlengths, aes(x = length)) + geom_histogram()
#length in microns


ggplot(amphlengths, aes(x = length)) + geom_histogram()+
  facet_wrap(~CommonName)

ggplot(amphlengths, aes(x = length)) + geom_histogram()+
  facet_wrap(~Month)

ggplot(amphlengths, aes(x = length)) + geom_histogram()+
  facet_wrap(~Location)

#biomass crosswalk from cage studies is one of the most comprehensive I have
crosswalk = read_excel("data/Cage Studies Biomass Crosswalk.xlsx", sheet = "Macro-zooplankton LW")
crosswalkfrp = read_excel("data/Cage Studies Biomass Crosswalk.xlsx", sheet = "FRPcrosswalk")

amphbiomass = amphlengths %>%
  left_join(crosswalkfrp) %>%
  left_join(filter(crosswalk, Preservative == "Ethanol")) %>%
  mutate(Biomass = a_grams*(length/10)^b)

ggplot(amphbiomass, aes(x = log(Biomass)))+ geom_histogram()+
  facet_wrap(~CommonName)

#now attach to bigger dataset, I guess

load("data/amphipods.RData")

amphbiomasssum = amphbiomass %>%
  mutate(AmphGroup = case_when(Family %in% c("Gammaridae", "Crangonycitidae", "Hyalellidae", 
                                           "Anisogammaridae", "Gammaroidea") ~ "Gammaridae and friends",
                             Family %in% c("Corophiidae" ) ~ "Corophiidae",
                             TRUE ~ "Other")) %>%
  group_by(Month, AmphGroup) %>%
  summarize(N = n(), Mean_Length = mean(length, na.rm =T), Median_Length = median(length, na.rm =T), 
            GeomMean_Length = exp(mean(log(length), na.rm =T)),
            Mean_Biomass = mean(Biomass, na.rm =T), Median_Biomass = median(Biomass, na.rm =T),
            GeomMean_Biomass = exp(mean(log(Biomass), na.rm =T)))

amphbiomass_long = amphbiomasssum %>%
  pivot_longer(cols = c(Mean_Length:last_col()), names_to = c( "SummaryType","Metric"), names_sep = "_",
               values_to = "Value")
  
ggplot(amphbiomass_long, aes(x = Month, y = Value, fill = SummaryType)) + geom_col(position = "dodge")+
  facet_grid(Metric~AmphGroup, scales = "free_y") +
  geom_label(aes(y = 0, label = N, x = Month), inherit.aes = F)

ggplot(amphbiomasssum, aes(x = length)) + geom_histogram()
