# Tidal wetland food web synthesis
# Bivalve data exploration
## see BugDataOrg.R code for original zooper data wrangling by Rosemary Hartman, DWR

# Created: K. Arend, USFWS; 03/24/2026
## last modified: 04/10/2026

# Start with basic data exploration

## Map of sites: "~/Food Web synthesis/Ks food web stuff/FW_Bivalves/FW_Wetlands_map.png"

# LOAD PACKAGES
library(lubridate)
library(janitor) # clean up or examine messy data
library(dplyr)
library(tidyr)
library(fitdistrplus) # fit distributions (e.g., gamma, lognormal)
library(ggplot2)
library(ggrepel)
library(legendry) # lets you have multiple variables on a single axis
library(viridis) # color palette
library(patchwork) # combine plots into one figure
library(here) # set working directory



# LOAD data
biv <- load(here("data/Bivalves.RData"))

## convert to a dataframe
biv_df <- as.data.frame(Bivalves)

## separate out month and year

### format Date
biv_df$Date_d <- as.Date(biv_df$Date, tz = "America/Los_Angeles")

biv_1 <- biv_df %>%
  mutate(
    Year_d = year(Date_d),
    Month_d = month(Date_d),
    Season_d = case_when(Month_d %in% c(3,4,5)~ "Spring",
                         Month_d %in% c(6,7,8) ~ "Summer",
                         Month_d %in% c(9,10,11) ~ "Fall",
                         Month_d %in% c(12,1,2) ~ "Winter"),
    Season_d = factor(Season_d, levels = c("Spring", "Summer", "Fall", "Winter")),
    Yr_Mo = make_date(year = Year_d, month = Month_d, day = 1),
    Yr_Ssn = paste0(Year_d, "_", Season_d),
    Yr_Ssn = factor(Yr_Ssn, levels = c("2017_Winter", "2017_Spring", "2017_Summer", "2017_Fall",
                                       "2018_Winter", "2018_Spring", "2018_Summer", "2018_Fall",
                                       "2019_Winter", "2019_Spring", "2019_Summer", "2019_Fall",
                                       "2020_Winter", "2020_Spring", "2020_Summer", "2020_Fall",
                                       "2021_Winter", "2021_Spring", "2021_Summer", "2021_Fall",
                                       "2022_Winter", "2022_Spring", "2022_Summer", "2022_Fall")))


## Tallies by location ----
### Tallies of regions by date, inside/outside
biv_reg <- biv_1 %>%
  group_by(Year_d, Month_d, Season_d, Region, Type, Yr_Mo, Yr_Ssn) %>%
  summarise(samples = n()) %>%
  ungroup()

### Tallies of stations by date, inside/outside
Station_List <- unique(biv_1$Station)

Region_Station <- biv_1 %>%
  group_by(Region, Type) %>%
  reframe(Stations = unique(Station))

#### plot locations
viridis_colors <- viridis_pal(option = "inferno")(6)
# remove hash below to see colors in palette
#"#000004FF" "#420A68FF" "#932667FF" "#DD513AFF" "#FCA50AFF" "#FCFFA4FF"
my_colors_inout <- c("#932667FF","#FCA50AFF") 

p_biv_reg_yr <- biv_reg %>%
#  filter(Region == "Cache Slough") %>%
  ggplot(aes(x = Year_d, y = samples, fill = Type)) +
  geom_col() +
  scale_fill_manual(values = my_colors_inout) +
  facet_wrap(~Region) +
  theme_bw()

p_biv_reg_ssn <- biv_reg %>%
  #  filter(Region == "Cache Slough") %>%
  ggplot(aes(x = Yr_Ssn, y = samples, fill = Type)) +
  geom_col() +
  scale_fill_manual(values = my_colors_inout) +
  facet_wrap(~Region) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  



## Tallies of taxa by region, inside/outside ----
biv_summ <- biv_1 %>%
  group_by(Year_d, Month_d, Yr_Ssn, Yr_Mo, Region, Station, Type, Tide, ClamGroup) %>%
  summarise(count = n(), meanCPUE = mean(CPUE)) %>%
  ungroup()

### plot CPUE inside vs outside for each region...just to see
my_colors_taxa <- c("Other" = "#000004FF", 
                    "Corbicula" = "#932667FF",
                    "Potamocorbula" = "#FCA50AFF")

p_CPUE_reg_tax <- biv_1 %>%
  mutate(Yr_Mo = make_date(year = Year_d, month = Month_d, day = 1)) %>%
  ggplot(aes(x = Yr_Ssn, y = CPUE)) +
  geom_point(aes(color = ClamGroup, shape = Type), size = 2.5) +
  facet_wrap(~Region) +
  scale_color_manual(values = my_colors_taxa) +
  scale_shape_manual(values = c("Inside" = 19, "Outside" = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


pc_CPUE_reg_tax <- biv_1 %>%
  mutate(Yr_Mo = make_date(year = Year_d, month = Month_d, day = 1)) %>%
  ggplot(aes(x = Yr_Ssn, y = CPUE, fill = ClamGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Type~Region) +
  scale_fill_manual(values = my_colors_taxa) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_meanCPUE_reg_tax <- biv_summ %>%
  ggplot(aes(x = Yr_Ssn, y = meanCPUE)) +
  geom_point(aes(color = ClamGroup), size = 2.5) +
  facet_wrap(Region~Type) +
  scale_color_manual(values = my_colors_taxa) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Distribution of CPUE across all years ----

### All taxa combined ----

biv_1_wide <- biv_1 %>%
  pivot_wider(names_from = ClamGroup, values_from = CPUE, names_glue = "{ClamGroup}_CPUE") %>%
  rowwise() %>% # treat each row as a group
  mutate(All_CPUE = sum(c_across(ends_with("_CPUE")), na.rm = TRUE)) %>% 
  ungroup() %>% # resolve rowwise grouping
  mutate(All_logCPUE = log(All_CPUE+1))


### Histogram, CPUE

ph_CPUE_all <- biv_1_wide %>%
  ggplot(aes(x=All_CPUE)) +
  geom_histogram(color = "white", fill = "#420A68FF") +
  theme_bw()
  

### Histogram, logCPUE
ph_CPUE_logAll <- biv_1_wide %>%
  ggplot(aes(x=All_logCPUE)) +
  geom_histogram(color = "white", fill = "#420A68FF") +
  theme_bw()



#### Fit different models
# Gamma distribution

All_logCPUE_pos <- biv_1_wide %>%
  filter(All_CPUE > 0)


gamma_CPUE_all <- fitdist(All_logCPUE_pos$All_logCPUE, distr = "gamma", method = "mle")

summary(gamma_CPUE_all)
plot(gamma_CPUE_all)

# lognormal distribution

lognormal_CPUE_all <- fitdist(All_logCPUE_pos$All_CPUE, "lnorm")

summary(lognormal_CPUE_all)
plot(lognormal_CPUE_all)

## Gamma looks a little better



##### Look at other possible variables to explain CPUE > 0 -----
## Salinity...is it correlated with Region?
pbx_SalRegion <- biv_1_wide %>%
  ggplot(aes(x = Region, y = SalSurf)) +
  geom_boxplot() +
  theme_bw()
  
pp_SalCPUE <- biv_1_wide %>%
  filter(All_CPUE > 0) %>%
  ggplot(aes(x = SalSurf, y = All_CPUE, col = Region)) +
  geom_point() +
  theme_bw()

pbx_TurbRegion <- biv_1_wide %>%
  ggplot(aes(x = Region, y = TurbidityNTU)) +
  geom_boxplot() +
  theme_bw()


pp_TurbCPUE <- biv_1_wide %>%
  filter(All_CPUE > 0) %>%
  ggplot(aes(x = TurbidityNTU, y = All_CPUE, col = Region)) +
  geom_point() +
  facet_wrap(~Type) +
  theme_bw()


### By taxon ----
#### Corbicula ----

### Histogram, CPUE
ph_CPUE_Corbicula <- biv_1_wide %>%
  ggplot(aes(x=Corbicula_CPUE)) +
  geom_histogram(color = "white", fill = "#420A68FF") +
  theme_bw()

### Histogram, logCPUE
ph_logCPUE_Corbicula <- biv_1_wide %>%
  mutate(Corbic_logCPUE = log(Corbicula_CPUE+1)) %>%
  ggplot(aes(x=Corbic_logCPUE)) +
  geom_histogram(color = "white", fill = "#420A68FF") +
  theme_bw()
  
#### Fit different models
# Gamma distribution

Corbic_logCPUE_pos <- biv_1_wide %>%
  filter(Corbicula_CPUE > 0) %>%
  mutate(Corbic_logCPUE = log(Corbicula_CPUE+1))


gamma_CPUE_Corbic <- fitdist(Corbic_logCPUE_pos$Corbic_logCPUE, distr = "gamma", method = "mle")

summary(gamma_CPUE_Corbic)
plot(gamma_CPUE_Corbic)

# lognormal distribution

lognormal_CPUE_Corbic <- fitdist(Corbic_logCPUE_pos$Corbicula_CPUE, "lnorm")

summary(lognormal_CPUE_Corbic)
plot(lognormal_CPUE_Corbic)

#### Potamocorbula ----

### Histogram, CPUE
ph_CPUE_Potamo <- biv_1_wide %>%
  ggplot(aes(x=Potamocorbula_CPUE)) +
  geom_histogram(color = "white", fill = "#420A68FF") +
  theme_bw()


### Histogram, logCPUE
ph_logCPUE_Potamo <- biv_1_wide %>%
  mutate(Potamo_logCPUE = log(Potamocorbula_CPUE+1)) %>%
  ggplot(aes(x=Potamo_logCPUE)) +
  geom_histogram(color = "white", fill = "#420A68FF") +
  theme_bw()

#### Fit different models
# Gamma distribution

Potamo_logCPUE_pos <- biv_1_wide %>%
  filter(Potamocorbula_CPUE > 0) %>%
  mutate(Potamocorbula_logCPUE = log(Potamocorbula_CPUE+1))


gamma_CPUE_Potamo <- fitdist(Potamo_logCPUE_pos$Potamocorbula_logCPUE, distr = "gamma", method = "mle")

summary(gamma_CPUE_Potamo)
plot(gamma_CPUE_Potamo)

# lognormal distribution

lognormal_CPUE_Potamo <- fitdist(Potamo_logCPUE_pos$Potamocorbula_CPUE, "lnorm")

summary(lognormal_CPUE_Potamo)
plot(lognormal_CPUE_Potamo)


#### Other ----

### Histogram, CPUE
ph_CPUE_Other <- biv_1_wide %>%
  ggplot(aes(x=Other_CPUE)) +
  geom_histogram(color = "white", fill = "#420A68FF") +
  theme_bw()


### Histogram, logCPUE
ph_logCPUE_Other <- biv_1_wide %>%
  mutate(Other_logCPUE = log(Other_CPUE+1)) %>%
  ggplot(aes(x=Other_logCPUE)) +
  geom_histogram(color = "white", fill = "#420A68FF") +
  theme_bw()

#### Fit different models
# Gamma distribution

Other_logCPUE_pos <- biv_1_wide %>%
  filter(Other_CPUE > 0) %>%
  mutate(Other_logCPUE = log(Other_CPUE+1))


gamma_CPUE_Other <- fitdist(Other_logCPUE_pos$Other_logCPUE, distr = "gamma", method = "mle")

summary(gamma_CPUE_Other)
plot(gamma_CPUE_Other)

# lognormal distribution

lognormal_CPUE_Other <- fitdist(Other_logCPUE_pos$Other_CPUE, "lnorm")

summary(lognormal_CPUE_Other)
plot(lognormal_CPUE_Other)

#some explortory plots

ggplot(Bivalves, aes(x = Type, y = CPUE, fill = ClamGroup)) + geom_boxplot()+
  facet_wrap(~Project_na)+ scale_y_log10()

ggplot(Bivalves, aes(x = ClamGroup, y = CPUE, fill = Type)) + geom_boxplot()+
  facet_wrap(~Project_na)+ scale_y_log10()

#pull in data from further awa\\y\

load('data/AllWetlandBugs_2010onwards.RData')

clams = Allbugs_Mar2026 %>%
  filter(TowType %in% c("PPG", "Ponar", "PVC"), Genus %in% c("Corbicula", "Potamocorbula"), !is.na(Latitude)) %>%
  st_as_sf(coords= c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(PrioritySites)) %>%
  st_join(allsites) %>%
  filter(is.na(Project_na)) %>%
  filter(!SampleID %in% Bivalves$SampleID)

#so these are all the sites outside of wetlands
clams = mutate(clams, Year = year(Date), Month = month(Date))
ggplot(clams, aes(x = as.factor(Year), y = CPUE, fill = Taxname)) + geom_boxplot()+ scale_y_log10()
