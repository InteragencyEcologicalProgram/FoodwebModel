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
