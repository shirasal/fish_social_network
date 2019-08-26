# 26 AUG 2019
library(tidyverse)
#### Plotting observations of species as maps ####

raw_med <- read_csv("med_raw.csv")
head(raw_med)

# Test on Sarpa salpa

# Create point data for Sarpa salpa observations:
sarpa.salpa <- raw_med %>%
  select(lon, lat, species) %>% 
  filter(species == "Sarpa.salpa") %>% 
  group_by(lon, lat) %>% 
  summarise() # returns only one incident for each coordinate pair
head(sarpa.salpa)

# Create scatter plot of the observations
ggplot(sarpa.salpa) + geom_point(aes(x = lon, y = lat))

# Create a base map of the mediterranean
library(sf)
med_seas <- st_read("~/GIS/Marine Regions/Mediterraneans_Seas_IHO/Med_Seas.shp")

(ss_p <- ggplot(sarpa.salpa) +
  geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
  geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = 2, alpha = 0.6))
ggsave(filename = "ObserveMaps/sarpa_salpa_obs.png", plot = ss_p)


