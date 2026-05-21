# This loads the Insect data that Rosie organized.
# Initial data exploration and modeling.

# questions ------
# I thought CPUE would control for volume, yet Volume is retained in the data set...
# which term(s) distinguish between restored and unrestored locations?
## somehow had that in my head that this was outside v inside 
## but those sound like rather different habitats, regardless of any restoration actions

# next steps -----
# concentrating on the sweep nets and ponar grabs/benthic cores might be more useful


# load libraries -----
library(here)
library(sf)
library(deltamapr)
library(tidyverse)

# insect data -----
load("data/Insects.RData")

glimpse(Insects) # InsectGroup includes "Chironomid" and "Other" only

levels(as.factor(Insects$Source))
levels(as.factor(Insects$TowType)) # oof!
Insects %>% group_by(Source, TowType) %>% summarise(n = n()) %>% select(Source, TowType, n)

# set up for plotting sample locations [from Rosie's code]
Ins <- st_as_sf(Insects, coords = c("Longitude", "Latitude"), crs = 4326)
# focus on: 20mm/Oblique; FRP/Surface; USGSbenthic/Ponar(?)

# map insect sampling locations/effort
ggplot(Ins) +
  geom_sf(data = WW_Delta)+
  geom_sf(data = PrioritySites, aes(fill = Project_na)) +
  geom_sf() +
  coord_sf(ylim = c(38, 38.35), xlim = c(-122.1, -121.56))

# compare sampling effort inside vs outside
ggplot(Insects, aes(x = as.factor(Year), fill = Type)) + 
  geom_histogram(stat = "count") +
  facet_wrap(~Project_na)

insects <- 
  Insects %>% 
  filter(Source == "20mm" | 
           (Source == "FRP" & TowType == "Surface") |
           Source == "USGSbenthic") %>% 
  ungroup() %>% 
  mutate(Season = case_when(month(Datetime) %in% c(3,4,5) ~ "Spring",
                            month(Datetime) %in% c(6,7,8) ~ "Summer",
                            month(Datetime) %in% c(9,10,11) ~ "Fall",
                            month(Datetime) %in% c(12,1,2) ~ "Winter",),
         year_meanCentered = Year - mean(Year)
  ) %>% 
  mutate(Season = factor(Season,
                         levels = c("Spring", "Summer", "Fall", "Winter"))) %>% 
  relocate(Season, year_meanCentered, .before = Date) %>% 
  select(-c(Datetime, Month))

# check
insects %>%
  count(Source, TowType)

# chironomids -----

bmod_chir1 <- 
  brm(data = insects %>% filter(InsectGroup == "Chironomid"),
      CPUE ~ Type + Project_na + Season +
                (1|Source) +
                (1|Year),
                family = gaussian,
                iter = 2000, warmup = 1000, chains = 4, cores = 4,
                control = list(adapt_delta = 0.95),
                seed = 42,
                file = here("outputs/bmod_chir1"),
                file_refit = "on_change")

pp_check(bmod_chir1) # need to think about priors tho this worked okay
summary(bmod_chir1)
plot(bmod_chir1)

bmod_chir2 <- 
  brm(data = insects %>% filter(InsectGroup == "Chironomid"),
      CPUE ~ Type + Project_na + Season +
        (1|Source) +
        (1|year_meanCentered),
      family = gaussian,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.95),
      seed = 42,
      file = here("outputs/bmod_chir2"),
      file_refit = "on_change")

pp_check(bmod_chir2) # again, think about priors
summary(bmod_chir2) # inside v out! Chipps & Decker stand out, curious no seasonality
plot(bmod_chir2)
