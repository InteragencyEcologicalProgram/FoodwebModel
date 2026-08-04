# Tidal wetland food web synthesis
# insect data exploration
# Pete Nelson, DWR
# created: 17 May 2026

# This loads the Insect data that Rosie organized.
# initial data exploration and modeling

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
library(brms)
library(tidyverse)

# insect data -----
load("data/Insects.RData")
load("data/Bugs_allfilters.RData")

glimpse(Insects) # InsectGroup includes "Chironomid" and "Other" only
glimpse(Bugs_allfilters) # I may want finer control on things...

# which combos Source*TowType are actually useful for sampling insects?
Bugs_allfilters %>%
  filter(Class == "Insecta") %>%
  group_by(Source, TowType, SampleID) %>%
  summarise(sample_CPUE = sum(CPUE, na.rm = TRUE), 
            .groups = "drop") %>%   # one row per tow
  group_by(Source, TowType) %>% 
  # Source=monitoring program (eg 20mm, YBFMP)
  # TowType=sampling style (eg oblique tow, sweep of SAV, neuston)
  summarise(
    n_samples = n_distinct(SampleID),
    n_zero_samples = sum(sample_CPUE == 0), # tows that caught zero insects
    prop_zero = mean(sample_CPUE == 0), # handy for the hurdle question
    mean_CPUE = mean(sample_CPUE), # mean catch per tow, zeros included
    total_CPUE = sum(sample_CPUE),
    .groups = "drop"
  )
# Source*TowType where prop_zero=1 NOT useful for looking at insects!

levels(as.factor(Insects$Source))
levels(as.factor(Insects$TowType)) # oof!
Insects %>% group_by(Source, TowType) %>% summarise(n = n()) %>% select(Source, TowType, n)

# 5/21/2026 problems w Insects as-is (missing EMP data)
load("data/Bugs_allfilters.RData")
sample_info = Bugs_allfilters%>%
  select(SampleID, Longitude, Latitude, Region, Project_na, Type, Source, Date, Station, Microcystis, Chlorophyll,
         Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
         TurbidityFNU, SizeClass, Volume) %>%
  distinct() # includes EMP

Insects = filter(Bugs_allfilters, Class == "Insecta") %>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass,Volume, Family) %>%
  summarize(CPUE = sum(CPUE, na.rm = T)) %>%
  # left_join(filter(sample_info,  !(SizeClass  %in% c("Meso", "Micro") & Source %in% c("EMP", "FMWT", "STN", "20mm")))) %>% #remove zoop samples that don't count amphipods
  # excluded code dropped rows that included both meso/micro size classes and came from 1 of the 4 surveys; not helpful for insects!
  mutate(Taxon = "Insect", CPUE = case_when(is.na(CPUE) ~ 0,
                                            TRUE ~ CPUE),
         InsectGroup = case_when(Family == "Chironomidae" ~ "Chironomid",
                                 TRUE ~ "Other"))%>%
  group_by(SampleID, Longitude, Latitude, Project_na,Region, Type, Source, Date, Station, Microcystis, Chlorophyll,
           Secchi, Temperature, SalSurf, TurbidityNTU, TowType, Year, Month, BottomDepth, Tide, Datetime, DO, 
           TurbidityFNU, SizeClass, InsectGroup,Volume) %>%
  summarize(CPUE = sum(CPUE, na.rm = T))

## EMP surveys ----
# want to know if there are any insects in the EMP surveys
chir_by_source <- 
  Bugs_allfilters %>% 
  group_by(Project_na, Region, Type, Source, Date, TowType, SizeClass, Volume, Family) %>%
  summarize(CPUE = sum(CPUE, na.rm = T), .groups = "drop") %>% 
  select(c(Source, TowType, SizeClass, Family, CPUE)) %>% 
  group_by(Family, Source) %>% 
  summarise(total = sum(CPUE)) %>% 
  filter(Family == "Chironomidae") %>% 
  print()
# no chironomids from EMP!

# (Class == "Insecta")
# (Order, Family, Taxlifestage)
insects_by_source <- 
  Bugs_allfilters %>% 
  filter(Class == "Insecta") %>% 
  group_by(SampleID, Longitude, Latitude, Project_na, Region, Type, Source, Date, 
           Station, Secchi, Temperature, SalSurf, 
           TurbidityNTU, TowType, BottomDepth, Tide, DO, 
           TurbidityFNU, SizeClass, Volume, 
           Order, Family, Taxlifestage) %>% 
  summarise(CPUE = sum(CPUE, na.rm = T), .groups = "drop") %>% 
  group_by(Source, Order) %>% 
  summarise(total_cpue = sum(CPUE)) %>% 
  print(n = 60)
# dipterids (prob mostly chironomids) are the best represented insects but other groups may be worth looking at too
write_csv(insects_by_source, "~/Library/CloudStorage/OneDrive-CaliforniaDepartmentofWaterResources/3-Projects/11-Foodweb Synthesis/insects_by_source.csv")

## sites & effort ------
# set up for plotting sample locations [from Rosie's code]
Ins <- st_as_sf(Insects, coords = c("Longitude", "Latitude"), crs = 4326)
# focus on: 20mm/Oblique; FRP/Surface; USGSbenthic/Ponar(?)

load("data/wetlandsites.RData")
load("data/PrioritySites.RData")

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

## tidy data -----
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

# first models ----
## chironomids -----

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

pp_check(bmod_chir1) # so cpue is 0-dominated and right-skewed, plus fit w gaussian
summary(bmod_chir1)
plot(bmod_chir1)

chir <- insects %>% filter(InsectGroup == "Chironomid")

chir %>% summarise(
  n = n(), 
  n_zero = sum(CPUE == 0), 
  prop_zero = mean(CPUE == 0),
  min = min(CPUE), 
  median = median(CPUE), 
  mean = mean(CPUE), 
  max = max(CPUE)
)

# zoom the same plot into the bulk (coord_cartesian keeps all data, just rescales the view)
pp_check(bmod_chir1) + ggplot2::coord_cartesian(xlim = c(0, 50))

# does the model predict impossible values / miss the zeros?
pp_check(bmod_chir1, type = "stat", stat = "min") # will sit far below 0
prop_zero <- function(x) mean(x == 0)
pp_check(bmod_chir1, type = "stat", stat = "prop_zero") # observed off in the tail

bmod_chir2 <- 
  brm(
    # magnitude of the positive catches
    bf(CPUE ~ Type + Project_na + Season + (1|Source) + (1|Year),
       # probability of zero chironomids
         hu  ~ Type + Project_na + Season + (1|Source) + (1|Year)),
      data = chir,
      family = hurdle_lognormal(),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.95),
      seed = 42,
      file = here("outputs/bmod_chir2"), file_refit = "on_change")

pp_check(bmod_chir2) # again, think about priors
summary(bmod_chir2) # inside v out! Chipps & Decker stand out, curious no seasonality
plot(bmod_chir2)

loo(bmod_chir1, bmod_chir2)
