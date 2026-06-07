

library(sf)

load(here("data/PrioritySites.RData"))
load(here("data/AllWetlandBugs_2010onwards.RData"))

temp <- Allbugs_Mar2026 %>% filter(Class == "Insecta")

temp_sf <- temp %>%
  # drops 5 rows
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

temp_with_sites <- temp_sf %>%
  st_join(PrioritySites %>% select(Project_na, Region), 
          join = st_within) %>%
  filter(!is.na(Project_na)) %>% # drops rows outside all priority sites
  st_drop_geometry() %>% # back to a regular tibble
  relocate(c(Region, Project_na), .after = Date)

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
