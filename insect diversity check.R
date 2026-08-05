library(vegan)

# Community matrix — family CPUE columns only
comm_matrix <- family_wide_filtered %>%
  select(-c(SampleID, Project_na, Type, Region, Source, Date,
            Station, TowType, Longitude, Latitude,
            total_CPUE, richness, shannon, simpson, Month, Season))

# Calculate indices
vegan_diversity <- tibble(
  SampleID = family_wide_filtered$SampleID,
  richness = specnumber(comm_matrix),
  shannon = diversity(comm_matrix, index = "shannon"),
  simpson = diversity(comm_matrix, index = "simpson")
)

# Compare vegan vs manual calculations
family_diversity_filtered %>%
  left_join(vegan_diversity, by = "SampleID", suffix = c("_manual", "_vegan")) %>%
  summarise(
    shannon_match = all(near(shannon_manual, shannon_vegan, tol = 1e-10), na.rm = TRUE),
    simpson_match = all(near(simpson_manual, simpson_vegan, tol = 1e-10), na.rm = TRUE),
    richness_match = all(richness_manual == richness_vegan, na.rm = TRUE)
  )
