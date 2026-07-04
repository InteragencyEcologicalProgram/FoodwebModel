
# exploration of insect data
# tl;dr: use the Allbugs_Mar2026 data if working only w 'Chironomid' or 'Other'
# to get the semi-raw data: 'Bugs_allfilters.RData'

# libraries ----
library(here)
library(sf) # load package 'sf' for mapping
library(brms)
library(janitor)
library(tidyverse)

# data ----
load(here("data/PrioritySites.RData")) # object "PrioritySites" only
load(here("data/AllWetlandBugs_2010onwards.RData")) # object "Allbugs_Mar2026" only
load(here("data/AllWetlandBugs.RData")) # object "AllBugs" only

# explore ----
# first the '_2010onwards...' object
temp <- Allbugs_Mar2026 %>% filter(Class == "Insecta")

temp_sf <- temp %>%
  # drops 5 rows
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

insects <- temp_sf %>%
  st_join(PrioritySites %>% select(Project_na, Region), 
          join = st_within) %>%
  filter(!is.na(Project_na)) %>% # drops rows outside all priority sites
  st_drop_geometry() %>% # back to a regular tibble
  relocate(c(Region, Project_na), .after = Date)

# now the 'AllBugs' object
temp <- AllBugs %>% filter(Class == "Insecta")

temp_sf <- temp %>%
  # drops 5 rows
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

insects_old <- temp_sf %>%
  st_join(PrioritySites %>% select(Project_na, Region), 
          join = st_within) %>%
  filter(!is.na(Project_na)) %>% # drops rows outside all priority sites
  st_drop_geometry() %>% # back to a regular tibble
  relocate(c(Region, Project_na), .after = Date)

## compare Rosie's data files ----
insects <- insects %>%
  mutate(dataset = "Mar2026") %>%
  group_by(dataset, Project_na, Region) %>%
  summarise(
    date_min  = min(Date, na.rm = TRUE),
    date_max  = max(Date, na.rm = TRUE),
    n_records = n(),
    .groups   = "drop"
  )

insects_old <- insects_old %>%
  mutate(dataset = "AllWetland") %>%
  group_by(dataset, Project_na, Region) %>%
  summarise(
    date_min  = min(Date, na.rm = TRUE),
    date_max  = max(Date, na.rm = TRUE),
    n_records = n(),
    .groups   = "drop"
  )

# are these the same underlying data, or separate pulls?
range(bugs_Mar2026_with_sites$Date, na.rm = TRUE)
range(bugs_with_sites$Date, na.rm = TRUE)

nrow(bugs_Mar2026_with_sites)
nrow(bugs_with_sites)

coverage_compare <- bind_rows(insects, insects_old) %>%
  pivot_wider(
    names_from  = dataset,
    values_from = c(date_min, date_max, n_records)
  )

## all insects -----
load(here("data/Bugs_allfilters.RData")) # gets you Bugs_allfilters, whatever that means and wherever it's actually from
names(Bugs_allfilters)
# this is the data file I want to work with. I think.

# prep & tidy insect data -----
# all insects data
# zero-insect samples don't have insect rows; have to build these zeros explicitely
sample_meta <- Bugs_allfilters %>%
  distinct(SampleID, .keep_all = TRUE) %>%
  select(SampleID, Project_na, Type, Region, Source, Date, Year, Month,
         Station, TowType, Longitude, Latitude,
         Secchi, Temperature, Chlorophyll, SalSurf, SalBott, TurbidityNTU)  # need other env data?``
# wind up 7,733 unique SampleIDs

# insects come in different life stages...we're going to recode into 3: larva, adult, undifferentiated
Bugs_allfilters %>%
  filter(Class == "Insecta") %>%
  mutate(Lifestage_simple = case_when(
    Lifestage %in% c("Larva", "Pupa", "Juvenile") ~ "Larva",
    Lifestage == "Adult" ~ "Adult",
    Lifestage == "Undifferentiated" ~ "Undifferentiated"  # or NA_character_ to drop
  ))
#...so I'm going to retain life stage info, but sum across them at least for now: won't save the above recode now

# taxonomy lookup — one row per Taxname
taxonomy_lookup <- Bugs_allfilters %>%
  filter(Class == "Insecta") %>%
  distinct(Taxname, Order, Family, Genus, Species)

# sum CPUE across lifestages, complete the grid
insect_cpue <- Bugs_allfilters %>%
  filter(Class == "Insecta") %>%
  group_by(SampleID, Taxname) %>%
  summarise(CPUE = sum(CPUE, na.rm = TRUE), .groups = "drop") %>%
  complete(
    SampleID = sample_meta$SampleID,
    Taxname,
    fill = list(CPUE = 0)
  )

# pivot wide, then join sample metadata and taxonomy
insect_wide <- insect_cpue %>%
  pivot_wider(names_from = Taxname, values_from = CPUE, values_fill = 0) %>%
  left_join(sample_meta, by = "SampleID") %>%
  mutate(Month = month(Date),
         Season = case_when(
    Month %in% c(12, 1, 2) ~ "Winter",
    Month %in% c(3, 4, 5) ~ "Spring",
    Month %in% c(6, 7, 8) ~ "Summer",
    Month %in% c(9, 10, 11) ~ "Fall",
    is.na(Month) ~ NA_character_),
    Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall"))) %>% 
  relocate(names(sample_meta), .before = everything()) %>% 
  relocate(Season, .after = Month) %>% 
  left_join(sample_diversity, by = "SampleID") %>%
  mutate(across(c(shannon, simpson), 
                ~ if_else(total_CPUE == 0, NA_real_, .)))

# check for dupes
taxonomy_lookup %>% 
  count(Taxname) %>% 
  filter(n > 1)
# nope!

# biodiversity -----
sample_diversity <- insect_cpue %>%
  group_by(SampleID) %>%
  summarise(
    total_CPUE = sum(CPUE),
    # note: considers a single sp rep by >1 lifestage as richness >1
    richness = sum(CPUE > 0),
    shannon = {p <- CPUE[CPUE > 0] / sum(CPUE[CPUE > 0])
    -sum(p * log(p))},
    simpson = {p <- CPUE[CPUE > 0] / sum(CPUE[CPUE > 0])
    1 - sum(p^2)},
    .groups = "drop"
  ) %>% 
  # fix zero catch diversity
  mutate(across(c(shannon, simpson), 
                ~ if_else(total_CPUE == 0, NA_real_, .)))

diversity_long <- insect_wide %>%
  pivot_longer(cols = c(richness, shannon, simpson),
               names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

## site diversity -----
ggplot(diversity_long, aes(x = reorder(Project_na, value, median), y = value)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by site")

## regional diversity -----
ggplot(diversity_long, aes(x = reorder(Region, value, median), y = value)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by region")

## seasonal diversity -----
ggplot(diversity_long, aes(x = Season, y = value)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by season")

## inside v outside diversity -----
ggplot(diversity_long, aes(x = Type, y = value)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity Inside vs Outside")

## methods diversity -----
ggplot(diversity_long, aes(x = Source, y = value)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by sampling method")

# conclusion ----

# clearly, the Allbugs_Mar2026 data are far more complete! but only if you want to focus exclusively on chironomids



# SCRATCH #################

# select all rows from priority sites
df <- Allbugs_Mar2026 %>%
  # drops ca 4,000 rows w no lat lon
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  # spatial join
  st_join(PrioritySites %>% select(Project_na, Region), 
          join = st_within) %>%
  # drops rows outside all priority sites
  filter(!is.na(Project_na)) %>% 
  # back to a regular tibble
  st_drop_geometry() %>% 
  relocate(c(Region, Project_na), .after = Date)

# list all SampleIDs
all_samples <- df %>% distinct(SampleID)

insect_wide <- df %>%
  filter(Class == "Insecta") %>%
  group_by(SampleID, Order) %>%
  summarise(CPUE = sum(CPUE, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Order, values_from = CPUE, values_fill = 0) %>%
  right_join(all_samples, by = "SampleID") %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

sample_meta <- df %>%
  distinct(SampleID, Date, Region, Project_na, Year, Month, TowType, Source)

insect_wide <- insect_wide %>%
  left_join(sample_meta, by = "SampleID")
