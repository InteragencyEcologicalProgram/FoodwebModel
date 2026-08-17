
# exploration of insect data
# tl;dr: use the Allbugs_Mar2026 data if working only w 'Chironomid' or 'Other'
# to get the semi-raw data: 'Bugs_allfilters.RData'

# libraries ----
library(here)
library(sf) # load package 'sf' for mapping
library(brms)
library(janitor)
library(vegan)
library(bayesplot)
library(tidyverse)

# data ----
load(here("data/PrioritySites.RData")) # object "PrioritySites" only
load(here("data/AllWetlandBugs_2010onwards.RData")) # object "Allbugs_Mar2026" only
load(here("data/AllWetlandBugs.RData")) # object "AllBugs" only

# explore ----
## Allbugs_Mar2026 -----
# first the '_2010onwards...' object
temp <- Allbugs_Mar2026 %>% filter(Class == "Insecta")

temp_sf <- temp %>%
  # req lat lon; drops 5 rows, Station=LHTinterior, 2018-04-19 to -04-26
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

insects <- temp_sf %>%
  # spatial join
  st_join(PrioritySites %>% select(Project_na, Region), 
          join = st_within) %>%
  filter(!is.na(Project_na)) %>% # drops rows outside all priority sites
  st_drop_geometry() %>% # back to a regular tibble
  relocate(c(Region, Project_na), .after = Date)

## AllBugs -----
temp <- AllBugs %>% filter(Class == "Insecta")

temp_sf <- temp %>%
  # drops 5 rows
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

insects_old <- temp_sf %>%
  # spatial join
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
    date_min = min(Date, na.rm = TRUE),
    date_max = max(Date, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  )

insects_old <- insects_old %>%
  mutate(dataset = "AllWetland") %>%
  group_by(dataset, Project_na, Region) %>%
  summarise(
    date_min = min(Date, na.rm = TRUE),
    date_max = max(Date, na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  )

# are these the same underlying data, or separate pulls?

coverage_compare <- bind_rows(insects, insects_old) %>%
  pivot_wider(
    names_from  = dataset,
    values_from = c(date_min, date_max, n_records)
  )

# more records in the Mar2026 data: use 'insects' not 'insects_old'

## all insects -----
load(here("data/Bugs_allfilters.RData")) # gets you Bugs_allfilters, whatever that means and wherever it's actually from
names(Bugs_allfilters)
# this is the data file I want to work with. I think.

# prep & tidy insect data -----
## sample meta -----
# all insects data
# zero-insect samples don't have insect rows; have to add these zeros explicitly
sample_meta <- Bugs_allfilters %>%
  distinct(SampleID, .keep_all = TRUE) %>%
  select(SampleID, Project_na, Type, Region, Source, Date, Year, Month,
         Station, TowType, Habitat, Longitude, Latitude,
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

## CPUE ----
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

## pivot wide -----

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

diversity_long <- insect_wide %>%
  pivot_longer(cols = c(richness, shannon, simpson),
               names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))


# check for dupes
taxonomy_lookup %>% 
  count(Taxname) %>% 
  filter(n > 1)
# nope!

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, title = "Insect diversity by sampling method")

## habitat diversity ------
ggplot(diversity_long, aes(x = Habitat, y = value)) +
  geom_boxplot(fill = "darkgreen", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by habitat")

# re-group ----
## sample methods -----
# consider which sampling methods are best suited for insects (kind of impossible)
# going to look for methods (Source x TowType) that yield the lowest proportion of zeros
method_summary <- Bugs_allfilters %>%
  filter(Class == "Insecta") %>%
  group_by(Source, TowType, SampleID, Project_na) %>%
  summarise(sample_CPUE = sum(CPUE, na.rm = TRUE), .groups = "drop") %>%
  group_by(Source, TowType) %>%
  summarise(
    n_samples = n(),
    n_sites   = n_distinct(Project_na),
    prop_zero = mean(sample_CPUE == 0),
    mean_CPUE = mean(sample_CPUE),
    .groups   = "drop"
  ) %>%
  arrange(prop_zero)

method_summary

ggplot(method_summary, aes(x = reorder(paste(Source, TowType, sep = " / "), prop_zero),
                           y = prop_zero)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Proportion zero-catch samples",
       title = "Sampling method effectiveness: Insects")

ggsave(here("outputs/insect_sampling_tools.png"))
# exclude types w prop_zero > 0.75

method_summary %>% 
  # filter(prop_zero < 0.75, n_samples < 500) %>% 
  ggplot(aes(n_samples, prop_zero, 
             # paste() inside aes() creates the combo wo add'l column
             color = paste(Source, TowType, sep = " / "))) +
  geom_point(size = 3) +
  labs(title = "Are number of samples & proportion of 0 catches correlated?",
       x = "number of samples",
       y = "proportion of zero catches") +
  theme_bw() +
  labs(color = "Source / TowType")

good_methods <- method_summary %>%
  filter(prop_zero < 0.75) %>%    
  select(Source, TowType)

Bugs_filtered <- Bugs_allfilters %>%
  semi_join(good_methods, by = c("Source", "TowType"))

## Update sample_meta ------ 
# filtered methods only
sample_meta_filtered <- Bugs_filtered %>%
  distinct(SampleID, .keep_all = TRUE) %>%
  select(SampleID, Project_na, Type, Region, Source, Date,
         Station, TowType, Longitude, Latitude)

## alt taxonomic level ----- 
# add Family_clean to Bugs_filtered
Bugs_filtered <- Bugs_filtered %>%
  mutate(Family_clean = case_when(
    !is.na(Family) ~ Family,
    !is.na(Order) ~ paste0(Order, "_UnID"),
    TRUE ~ "Insecta_UnID"
  )) 

## re-calc cpue ----
# family-level CPUE per sample
family_cpue_filtered <- Bugs_filtered %>%
  filter(Class == "Insecta") %>%
  group_by(SampleID, Family_clean) %>%
  summarise(CPUE = sum(CPUE, na.rm = TRUE), .groups = "drop") %>%
  complete(
    SampleID = sample_meta_filtered$SampleID,
    Family_clean,
    fill = list(CPUE = 0)
  )

## new tax lookup ----- 

family_taxonomy <- Bugs_filtered %>%
  filter(Class == "Insecta") %>%
  distinct(Family_clean, Order, Family)

## re-calc diversity metrics -----
family_diversity_filtered <- family_cpue_filtered %>%
  group_by(SampleID) %>%
  summarise(
    total_CPUE = sum(CPUE),
    richness = sum(CPUE > 0),
    shannon = {p <- CPUE[CPUE > 0] / sum(CPUE[CPUE > 0]); -sum(p * log(p))},
    simpson = {p <- CPUE[CPUE > 0] / sum(CPUE[CPUE > 0]); 1 - sum(p^2)},
    .groups = "drop"
  ) %>%
  mutate(across(c(shannon, simpson), ~ if_else(total_CPUE == 0, NA_real_, .)))

## pivot wide ------
# wide format with metadata, diversity, and season
family_wide_filtered <- family_cpue_filtered %>%
  pivot_wider(names_from = Family_clean, values_from = CPUE, values_fill = 0) %>%
  left_join(sample_meta_filtered, by = "SampleID") %>%
  left_join(family_diversity_filtered, by = "SampleID") %>%
  mutate(
    Month = month(Date),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall"
    ),
    Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall"))
  ) %>%
  relocate(names(sample_meta_filtered), total_CPUE, richness, shannon, simpson, Month, Season,
           .before = everything())

## pivot long -----
diversity_long_filtered <- family_wide_filtered %>%
  pivot_longer(cols = c(richness, shannon, simpson),
               names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

### by site -----
ggplot(diversity_long_filtered, 
       aes(x = reorder(Project_na, value, median), y = value)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by site")

### by region -----
ggplot(diversity_long_filtered, 
       aes(x = reorder(Region, value, median), y = value)) +
  geom_boxplot(fill = "darkorange2", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by region")

### by season -----
diversity_long_filtered %>%
  filter(!is.na(Season)) %>%
  ggplot(aes(x = value, 
             y = factor(Season, levels = c("Fall", "Winter", "Spring", "Summer")))) +
  scale_y_discrete(limits = rev) +
  geom_boxplot(fill = "chartreuse3", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by season")

### inside v outside diversity -----
ggplot(diversity_long_filtered, aes(x = value, y = Type)) +
  geom_boxplot(fill = "darkgoldenrod4", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity Inside vs Outside")

### methods diversity -----
ggplot(diversity_long_filtered, aes(x = value, y = Source)) +
  geom_boxplot(fill = "antiquewhite4", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by sampling program")

# proportion of zero-richness samples by method
method_diversity <- family_wide_filtered %>%
  group_by(Source, TowType) %>%
  summarise(
    n_samples = n(),
    prop_zero_rich = mean(richness == 0),
    median_rich = median(richness),
    median_shannon = median(shannon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_zero_rich))

method_diversity

family_wide_filtered %>%
  mutate(method = paste(Source, TowType, sep = " / ")) %>%
  pivot_longer(cols = c(richness, shannon, simpson),
               names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value, y = reorder(method, value, median))) +
  geom_boxplot(fill = "darkolivegreen1", outlier.size = 0.3) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Diversity by sampling method")

# if USGS/oblique is concentrated at 1 or 2 sites it could biase in/outside or 
# regional comparisons...check:
family_wide_filtered %>%
  filter(Source == "USGS", TowType == "Oblique") %>%
  count(Project_na)
# looks fine

## drop methods ----
# dropping USGSbenthic/ponar and USGS/oblique
good_methods <- good_methods %>%
  filter(!Source %in% c("USGS", "USGSbenthic"))

family_wide_filtered <- family_wide_filtered %>%
  filter(!Source %in% c("USGS", "USGSbenthic"))

# rebuild diversity_long_filtered from the updated wide table
diversity_long_filtered <- family_wide_filtered %>%
  pivot_longer(cols = c(richness, shannon, simpson),
               names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

## plot in/out versus -----
# inside/outside (Type) × Season
diversity_long_filtered %>%
  filter(!is.na(Season), !is.na(Type)) %>%
  ggplot(aes(x = value, y = Season, fill = Type)) +
  geom_boxplot(outlier.size = 0.3, position = position_dodge(0.8)) +
  scale_y_discrete(limits = rev) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Diversity by SEASON and habitat type")

# inside/outside (Type) × Region
diversity_long_filtered %>%
  filter(!is.na(Type)) %>%
  ggplot(aes(x = value, y = reorder(Region, value, median), fill = Type)) +
  geom_boxplot(outlier.size = 0.3, position = position_dodge(0.8)) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Diversity by REGION and habitat type")

# inside/outside (Type) × Site
diversity_long_filtered %>%
  filter(!is.na(Type)) %>%
  ggplot(aes(x = value, y = reorder(Project_na, value, median), fill = Type)) +
  geom_boxplot(outlier.size = 0.3, position = position_dodge(0.8)) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Diversity by SITE and habitat type")

# what's going on Site=LHT?
family_wide_filtered %>%
  count(Project_na, Type) %>%
  pivot_wider(names_from = Type, values_from = n, values_fill = 0)
# LHT contributes nothing to inside v outside; same w LICB

# working data -----
# drop selected sites
family_wide_filtered <- family_wide_filtered %>%
  filter(!Project_na %in% c("LHT", "LICB"))

diversity_long_filtered <- family_wide_filtered %>%
  pivot_longer(cols = c(richness, shannon, simpson),
               names_to = "metric", values_to = "value") %>%
  filter(!is.na(value))

## re-run plots -----
### by site -----
ggplot(diversity_long_filtered, 
       aes(x = reorder(Project_na, value, median), y = value)) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by site")

### by region -----
ggplot(diversity_long_filtered, 
       aes(x = reorder(Region, value, median), y = value)) +
  geom_boxplot(fill = "darkorange2", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by region")

### by season -----
diversity_long_filtered %>%
  filter(!is.na(Season)) %>%
  ggplot(aes(x = value, 
             y = factor(Season, levels = c("Fall", "Winter", "Spring", "Summer")))) +
  scale_y_discrete(limits = rev) +
  geom_boxplot(fill = "chartreuse3", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by season")

### inside v outside diversity -----
ggplot(diversity_long_filtered, aes(x = value, y = Type)) +
  geom_boxplot(fill = "darkgoldenrod4", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity Inside vs Outside")

### methods diversity -----
# originally looked at diversity by sampling program, but FRP includes
# eight different methods, so...

# proportion of zero-richness samples by method
method_diversity <- family_wide_filtered %>%
  group_by(Source, TowType) %>%
  summarise(
    n_samples = n(),
    prop_zero_rich = mean(richness == 0),
    median_rich = median(richness),
    median_shannon = median(shannon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_zero_rich))

method_diversity

# plot diversity by program/method
diversity_long_filtered %>%
  mutate(method = paste(Source, TowType, sep = " / ")) %>%
  ggplot(aes(x = value, y = reorder(method, value, median))) +
  geom_boxplot(fill = "darkolivegreen1", outlier.size = 0.5) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Insect diversity by sampling program/method")

### plot in/out versus -----
# inside/outside (Type) × Season
diversity_long_filtered %>%
  filter(!is.na(Season), !is.na(Type)) %>%
  ggplot(aes(x = value, y = Season, fill = Type)) +
  geom_boxplot(outlier.size = 0.3, position = position_dodge(0.8)) +
  scale_y_discrete(limits = rev) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Diversity by SEASON and habitat type")

# inside/outside (Type) × Region
diversity_long_filtered %>%
  filter(!is.na(Type)) %>%
  ggplot(aes(x = value, y = reorder(Region, value, median), fill = Type)) +
  geom_boxplot(outlier.size = 0.3, position = position_dodge(0.8)) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Diversity by REGION and habitat type")

# inside/outside (Type) × Site
diversity_long_filtered %>%
  filter(!is.na(Type)) %>%
  ggplot(aes(x = value, y = reorder(Project_na, value, median), fill = Type)) +
  geom_boxplot(outlier.size = 0.3, position = position_dodge(0.8)) +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Diversity by SITE and habitat type")

# what next? ----
## more data reduction ----
# for now, let's filter to richness>0 and do some NMDS ordinations

# filter to non-zero samples
family_nonzero <- family_wide_filtered %>%
  filter(richness > 0)

# create community matrix
comm_matrix <- family_nonzero %>%
  select(-c(SampleID, Project_na, Type, Region, Source, Date,
            Station, TowType, Longitude, Latitude,
            total_CPUE, richness, shannon, simpson, Month, Season))

# run NMDS
set.seed(123)
nmds <- metaMDS(comm_matrix, distance = "bray", k = 2, trymax = 100)
nmds$stress  # check stress — want < 0.2, ideally < 0.1

# warnings suggest that all-zero rows snuck through?
comm_matrix %>% 
  filter(rowSums(.) == 0) %>% 
  nrow()
# nope!

# maybe all-zero columns, families entirely absent in the filtered data?
comm_matrix %>% 
  select(where(~ sum(.) == 0)) %>% 
  names()
# yup! 27 all-zero columns — families in full dataset but dropped after 
# filtering methods and removing zero-catch samples

# drop these before rerunning NMDS
comm_matrix <- comm_matrix %>%
  select(where(~ sum(.) > 0))

## rerun NMDS ----
# rerun NMDS on cleaned matrix
set.seed(123)
nmds <- metaMDS(comm_matrix, distance = "bray", k = 2, trymax = 100)

# extract scores & plot
nmds_scores <- as_tibble(scores(nmds, display = "sites")) %>%
  bind_cols(family_nonzero %>% 
              select(SampleID, Project_na, Type, Region, Season, Source, TowType))

## plot ----
# plot colored by Type (inside v outside)
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Type)) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_ellipse(level = 0.95) +
  theme_bw() +
  labs(title = paste0("NMDS (stress = ", round(nmds$stress, 3), ")"))

# Region groups, Season, Project_na
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Region)) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_ellipse(level = 0.95) +
  theme_bw() +
  labs(title = paste0("NMDS (stress = ", round(nmds$stress, 3), ")"))

# Season groups
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Season)) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_ellipse(level = 0.95) +
  theme_bw() +
  labs(title = paste0("NMDS (stress = ", round(nmds$stress, 3), ")"))

# Project_na groups
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Project_na)) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_ellipse(level = 0.95) +
  theme_bw() +
  labs(title = paste0("NMDS (stress = ", round(nmds$stress, 3), ")"))

## why bad plots? -----
# all of the NMDS ordinations look similar with a single outlier on nmds1
# and another outlier on nmds2; the remainder are clustered close to the
# origin

# 1. which samples are the outliers?
nmds_scores %>%
  filter(NMDS1 > 1 | NMDS2 > 0.05) %>%
  select(SampleID, Project_na, Source, TowType, NMDS1, NMDS2)
# both from Decker, FRP, Surface

# 2. which families have extreme scores on each axis?
species_scores <- as_tibble(scores(nmds, display = "species"), rownames = "Family") %>%
  arrange(desc(abs(NMDS1)))

species_scores %>% slice_max(abs(NMDS1), n = 10)
species_scores %>% slice_max(abs(NMDS2), n = 10)
# Notonectidae & Noteridae stand out

# 3. look at the actual CPUE values for the outlier samples
outlier_ids <- nmds_scores %>%
  filter(NMDS1 > 1 | NMDS2 > 0.05) %>%
  pull(SampleID)

family_nonzero %>%
  filter(SampleID %in% outlier_ids) %>%
  select(SampleID, Project_na, Source, TowType, all_of(species_scores$Family[1:10]))

# Why is the first sample an outlier if its scores are all zero?
family_nonzero %>%
  filter(SampleID == "FRP MAC2-HORS-24MAR2021") %>%
  select(where(is.numeric)) %>%
  select(where(~ . > 0))

# Why didn't Noteridae show up?
# for the NMDS1 outlier — top families by NMDS1
species_scores %>% slice_max(abs(NMDS1), n = 10) %>% pull(Family)

# for the NMDS2 outlier — top families by NMDS2
species_scores %>% slice_max(abs(NMDS2), n = 10) %>% pull(Family)

# which samples contain Noteridae?
family_nonzero %>%
  filter(Noteridae > 0) %>%
  select(SampleID, Project_na, Source, TowType, Noteridae) %>%
  left_join(nmds_scores %>% select(SampleID, NMDS1, NMDS2), by = "SampleID")

## MORE data reduction -----
# so the NMDS outliers are the result of two samples w richness=1 for 
# Noteridae and Notonectidae; I'm going to filter for richness>=2

family_nonzero <- family_nonzero %>%
  filter(richness >= 2)

comm_matrix <- family_nonzero %>%
  select(where(is.numeric)) %>%
  select(-c(Latitude, total_CPUE, richness, shannon, simpson, Month)) %>%
  select(where(~ sum(.) > 0))  # drop any newly-zeroed columns

## rerun NMDS -----
set.seed(123)
nmds <- metaMDS(comm_matrix, distance = "bray", k = 2, trymax = 100)
nmds$stress
# stress to0 high for 2D, so

set.seed(123)
nmds3 <- metaMDS(comm_matrix, distance = "bray", k = 3, trymax = 100)
nmds3$stress

# extract scores & plot
nmds_scores <- as_tibble(scores(nmds3, display = "sites")) %>%
  bind_cols(family_nonzero %>% 
              select(SampleID, Project_na, Type, Region, Season, Source, TowType))

# plot colored by Type (inside v outside)
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Type)) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_ellipse(level = 0.95) +
  theme_bw() +
  labs(title = "Habitat Type: inside vs outside", 
       subtitle = paste0("NMDS (stress = ", round(nmds3$stress, 3), ")"))

# plot Region
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Region)) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_ellipse(level = 0.95) +
  theme_bw() +
  labs(title = "Regions",
       subtitle = paste0("NMDS (stress = ", round(nmds3$stress, 3), ")"))

# Season groups
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Season)) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_ellipse(level = 0.95) +
  theme_bw() +
  labs(title = "Seasons",
       subtitle = paste0("NMDS (stress = ", round(nmds3$stress, 3), ")"))

# Project_na groups
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Project_na)) +
  geom_point(alpha = 0.5, size = 1.5) +
  stat_ellipse(level = 0.95) +
  theme_bw() +
  labs(title = "Projects",
       subtitle = paste0("NMDS (stress = ", round(nmds3$stress, 3), ")"))

# status -----
## NMDS -----
# shows next to no separation; insect communities seem to be v similar across all groups

## div indices -----
# may be some richness differences across sites, maybe in vs outside

## plan -----
# PERMANOVA to test community differences despite nmds
# model diversity indices w mixed models

## PERMANOVA ----
permanova_results <- 
  adonis2(comm_matrix ~ Type + Region + Season, 
        data = family_nonzero, 
        permutations = 999, 
        method = "bray")
# statistically significant but ecologically weak: type, region & season only 
# explain about 4% of the variance
# F=6.3855, p=0.001; centroids are more different than you'd expect by chance
# but w n=1206 samples, tiny effects may be statistically significant

# which factor(s) is driving the effect?
permanova_results_by_type <- 
  adonis2(comm_matrix ~ Type + Region + Season, 
        data = family_nonzero, 
        permutations = 999, 
        method = "bray",
        by = "term") |> print()

# even the "best" (Region) explains only about 2.3% of the variation; not ecologically
# meaningful. community composition just doesn't structure meaningfully along these 
# gradients. That's itself an interesting finding — insect communities in these 
# wetlands appear to be drawn from a largely common regional species pool, with 
# habitat type and season having minimal influence on which families show up.

# richness models -----
## method variable -----
# create combined method variable
family_nonzero <- family_nonzero %>%
  mutate(method = paste(Source, TowType, sep = " / "))

## priors -----
# set up weakly informative priors for log-scale coefficients
priors <- c(
  prior(normal(0, 2.5), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(exponential(1), class = "sd")
)

## fit model -----
fit_richness <- brm(
  richness ~ Type + Region + Season + method + (1 | Project_na),
  data = family_nonzero,
  family = negbinomial(),
  prior = priors,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 42
)

# convergence diagnostics
summary(fit_richness) # Rhat should be <1.01 for all parameters
plot(fit_richness) # trace plots should look like "fuzzy caterpillars"
pp_check(fit_richness) # posterior predictive check

fit_richness <- brm(
  richness ~ Type + Region + Season + method + (1 | Project_na),
  data    = family_nonzero,
  family  = negbinomial(),
  prior   = priors,
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = 4,
  seed    = 42,
  control = list(adapt_delta = 0.95) # bc divergent transitions
)

summary(fit_richness)
plot(fit_richness)
pp_check(fit_richness)

# Shannon models -----
# swap the family from negbinomial() to Gamma(link = "log") — appropriate for 
# positive continuous data that's likely right-skewed
## fit model -----
fit_shannon <- brm(
  shannon ~ Type + Region + Season + method + (1 | Project_na),
  data    = family_nonzero,
  family  = Gamma(link = "log"),
  prior   = priors, # priors as before are fine bc still using a log link
  chains  = 4,
  iter    = 2000,
  warmup  = 1000,
  cores   = 4,
  seed    = 42,
  control = list(adapt_delta = 0.99)
)

summary(fit_shannon)
plot(fit_shannon)
pp_check(fit_shannon)

# lost the ability to detect a Type effect
hypothesis(fit_shannon, "abs(TypeOutside) < 0.1") # practically zero
# is the inside/outside effect practically negligible, defined 
# as smaller than 0.1 in absolute value on the log scale (roughly a 10% 
# difference in Shannon)?
# Evid.Ration=44.45, so data are 44x more consistent w a neglible effect
# than w a meaningful one
# Post.Prob=0.98 that the true inside/outside effect is smaller than our
# practical threshold

## conclusion -----
# positive evidence for a null result, not just a failure to detect an effect
# the distinction matters: we're not saying "we couldn't find an effect," we're 
# saying "we have strong evidence the effect is too small to be ecologically 
# meaningful." this is a strong and useful conclusion 

# plot models -----
library(tidybayes)

# posterior predictions by Type
fit_richness %>%
  epred_draws(newdata = distinct(family_nonzero, Type, Region, Season, method),
              re_formula = NA) %>%  # marginalizes over random effects
  filter(.epred <= 15) %>% # trim extreme tail
  ggplot(aes(x = .epred, y = Type)) +
  stat_halfeye(fill = "darkgoldenrod4") + # terrible color!
  theme_bw() +
  labs(x = "Predicted richness", y = NULL)

# posterior predictions by Region
fit_richness %>%
  epred_draws(newdata = distinct(family_nonzero, Type, Region, Season, method),
              re_formula = NA) %>%  # marginalizes over random effects
  filter(.epred <= 12) %>% # trim extreme tail
  ggplot(aes(x = .epred, y = Region)) +
  stat_halfeye(fill = "darkorange2") + 
  theme_bw() +
  labs(x = "Predicted richness", y = NULL)

# posterior predictions by Season
fit_richness %>%
  epred_draws(newdata = distinct(family_nonzero, Type, Region, Season, method),
              re_formula = NA) %>%  # marginalizes over random effects
  filter(.epred <= 12) %>% # trim extreme tail
  ggplot(aes(x = .epred, y = Season)) +
  stat_halfeye(fill = "chartreuse3") + 
  theme_bw() +
  labs(x = "Predicted richness", y = NULL)

## combined plots -----
library(patchwork)

# shared draw step
richness_draws <- fit_richness %>%
  epred_draws(newdata = distinct(family_nonzero, Type, Region, Season, method),
              re_formula = NA)

p_type <- richness_draws %>%
  filter(.epred <= 15) %>%
  ggplot(aes(x = .epred, y = Type)) +
  stat_halfeye(fill = "steelblue") +
  theme_bw() +
  labs(x = NULL, y = NULL)

p_region <- richness_draws %>%
  filter(.epred <= 15) %>%
  ggplot(aes(x = .epred, y = reorder(Region, .epred, median))) +
  stat_halfeye(fill = "steelblue") +
  theme_bw() +
  labs(x = NULL, y = NULL)

p_season <- richness_draws %>%
  filter(.epred <= 15) %>%
  ggplot(aes(x = .epred, y = Season)) +
  stat_halfeye(fill = "steelblue") +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  labs(x = "Predicted richness", y = NULL)

p_type / p_region / p_season +
  plot_annotation(title = "Posterior predicted insect family richness") &
  xlim(0, 10)

# regional env data -----
# Step 1: fetch all California stream stations
ca_stations <- read_waterdata_monitoring_location(
  state_name = "California",
  site_type  = "Stream"
)

# Step 2: check column names
names(ca_stations)

# Step 3: spatial filter
bbox <- st_bbox(c(xmin = -122.15, ymin = 38.05,
                  xmax = -121.55, ymax = 38.45),
                crs = 4326) %>%
  st_as_sfc()

delta_stations <- ca_stations %>%
  st_filter(bbox)

delta_stations %>%
  select(monitoring_location_id, monitoring_location_name) %>%
  st_drop_geometry()

# select the stations located in the regions of interest
delta_stations_regions <- delta_stations %>%
  st_join(PrioritySites %>% select(Project_na, Region),
          join = st_within) %>%
  filter(!is.na(Region)) %>%
  st_drop_geometry() %>%
  select(monitoring_location_id, monitoring_location_name, Project_na, Region)

delta_stations_regions

# at these stations, what data are available?
param_codes <- c("00095",  # specific conductance (salinity proxy)
                 "00010",  # water temperature
                 "63680",  # turbidity (FNU)
                 "00060",  # discharge/flow
                 "00300",  # dissolved oxygen
                 "32316")  # chlorophyll a

station_ids <- delta_stations_regions$monitoring_location_id

meta <- read_waterdata_combined_meta(
  monitoring_location_id = station_ids,
  parameter_code         = param_codes
)

# What's available where, and for how long?
meta_summary <- meta %>%
  st_drop_geometry() %>%
  filter(!is.na(begin)) %>%
  mutate(
    record_years = as.numeric(difftime(end, begin, units = "days")) / 365.25
  ) %>%
  select(monitoring_location_id, parameter_name, begin, end, record_years) %>%
  arrange(monitoring_location_id, parameter_name)

meta_summary |> arrange(desc(record_years), monitoring_location_id) |> print(n=42)

# Which station/parameter combinations have >= 8 years?
good_records <- meta_summary %>%
  filter(record_years >= 8)

good_records
# one station?!
