# compare alt data sources: Allbugs_Mar2026, AllBugs, Bugs_allfilters
# focus on insects
# Pete Nelson, DWR, 2026-07-29

# tl;dr: use Bugs_allfilters tho only 2017-2023

# 1. Schema comparison — what columns does each dataset have?
tibble(
  column = union(union(names(Allbugs_Mar2026), names(AllBugs)), names(Bugs_allfilters)),
  in_Mar2026 = column %in% names(Allbugs_Mar2026),
  in_AllBugs = column %in% names(AllBugs),
  in_allfilters = column %in% names(Bugs_allfilters)
) %>% print(n = Inf)

# 2. Basic dimensions
tibble(
  dataset = c("Allbugs_Mar2026", "AllBugs", "Bugs_allfilters"),
  n_rows = c(nrow(Allbugs_Mar2026), nrow(AllBugs), nrow(Bugs_allfilters)),
  n_cols = c(ncol(Allbugs_Mar2026), ncol(AllBugs), ncol(Bugs_allfilters)),
  date_min = c(min(Allbugs_Mar2026$Date, na.rm = TRUE),
                  min(AllBugs$Date, na.rm = TRUE),
                  min(Bugs_allfilters$Date, na.rm = TRUE)),
  date_max = c(max(Allbugs_Mar2026$Date, na.rm = TRUE),
                  max(AllBugs$Date, na.rm = TRUE),
                  max(Bugs_allfilters$Date, na.rm = TRUE))
)

# 3. Insects only — how many records and sites in each?
list(
  Mar2026 = Allbugs_Mar2026 %>% filter(Class == "Insecta"),
  AllBugs = AllBugs %>% filter(Class == "Insecta"),
  allfilters = Bugs_allfilters %>% filter(Class == "Insecta")
) %>%
  map_dfr(~ summarise(.x,
                      n_insect_records = n(),
                      n_sources = n_distinct(Source),
                      n_towtypes = n_distinct(TowType)
  ), .id = "dataset")

# 4. Sources in each dataset
list(
  Mar2026 = Allbugs_Mar2026,
  AllBugs = AllBugs,
  allfilters = Bugs_allfilters
) %>%
  map_dfr(~ count(.x, Source, TowType), .id = "dataset") %>%
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
  arrange(Source, TowType) |> print(n=26)

# 5. Site coverage — which priority sites appear in each?
make_insect_sf <- function(df) {
  df %>%
    filter(Class == "Insecta", !is.na(Latitude), !is.na(Longitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_join(PrioritySites %>% select(Project_na, Region), join = st_within) %>%
    filter(!is.na(Project_na)) %>%
    st_drop_geometry()
}

insects_Mar2026 <- make_insect_sf(Allbugs_Mar2026)
insects_AllBugs <- make_insect_sf(AllBugs)
# Bugs_allfilters already has Project_na, no join needed

sites_Mar2026 <- insects_Mar2026 %>% distinct(Project_na) %>% mutate(Mar2026 = TRUE)
sites_AllBugs <- insects_AllBugs %>% distinct(Project_na) %>% mutate(AllBugs = TRUE)
sites_allfilters <- Bugs_allfilters %>% distinct(Project_na) %>% mutate(allfilters = TRUE)

sites_Mar2026 %>%
  full_join(sites_AllBugs, by = "Project_na") %>%
  full_join(sites_allfilters, by = "Project_na") %>%
  replace_na(list(Mar2026 = FALSE, AllBugs = FALSE, allfilters = FALSE)) %>%
  arrange(Project_na) %>%
  print(n = Inf)
