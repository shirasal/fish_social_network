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
med_seas <- st_read("~/GIS/Marine Regions/Mediterraneans_Seas_IHO/Med_Seas.shp") # for Lab computer
med_seas <- st_read("~/1Masters/GIS/Med/iho.shp")# For personal laptop

# Map for Sarpa salpa
(ss_p <- ggplot(sarpa.salpa) +
    geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
    geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = 2, alpha = 0.6))
ggsave(filename = "ObserveMaps/sarpa_salpa_obs.png", plot = ss_p)

# Map for Boops boops [This is what needs iteration]
boops.boops <- med_mat_minimal %>% 
  select(lon, lat, Boops.boops) %>% 
  filter(Boops.boops != 0)

(bb_p <- ggplot(boops.boops) +
    geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
    geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = 2, alpha = 0.6))
ggsave(filename = "ObserveMaps/boops_boops_obs.png", plot = bb_p)

##### Now loop this to create a map for each species ####
med_mat <- read_csv("med_species_matrix.csv")

# Create a data frame with lon, lat and species only:
med_mat_minimal <- med_mat %>% 
  select(lon, lat, 17:ncol(.))

# Create a loop to subset each species obaservation
spp_list <- NULL
for(i in 3:ncol(med_mat_minimal)) {
  spp_list[[i]] <- med_mat_minimal %>%
    select(lon, lat, i) %>% 
    filter(.[, 3] > 0)
}
spp_list[[1]] <- NULL # this one is only lon
spp_list[[2]] <- NULL # thi one is only lat

spp_names <- colnames(med_mat_minimal[3:ncol(med_mat_minimal)])
names(spp_list)
# Check if I can find specific object in that list:
spp_list[["Boops.boops"]]
# Great success!
