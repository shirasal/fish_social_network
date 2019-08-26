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

########## 1st try ##########
for (i in med_mat[, 3:ncol(med_mat)]) {
  med_mat[[i]] <- med_mat %>% ggplot(i, aes(x = lon, y = lat)) +
      geom_sf(data = med_seas) +
      geom_point()
}

########## 2nd try ##########
for (i in med_mat) {
  spp_tbl <- tibble(med_mat$lon, med_mat$lat)
}

########## 3rd try ##########
spp_tbl <- NULL
for (i in c(17:ncol(med_mat))) {
  spp_tbl <- tibble(med_mat$lon, med_mat$lat, med_mat[[i]])
  colnames(spp_tbl) <- c("lon", "lat", i)
}

########## Check on one species ##########
spp_tbl <- tibble(med_mat$lon, med_mat$lat, med_mat[["Diplodus.annularis"]])
colnames(spp_tbl) <- c("lon", "lat", "Diplodus.annularis")

########## 4th try ##########
spp_tbl <- NULL
for (i in cols(med_mat_minimal)) {
  spp_tbl <- tibble(med_mat_minimal$lon, med_mat_minimal$lat, med_mat_minimal[[i]])
  colnames(spp_tbl) <- c("lon", "lat", i)
}


