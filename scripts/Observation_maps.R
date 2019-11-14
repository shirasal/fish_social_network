# Required packcages:
library(tidyverse)
library(sf)

#### Plotting observations of species as maps ####

# Create a base map of the mediterranean
med_seas <- st_read("~/GIS/Marine Regions/Mediterraneans_Seas_IHO/Med_Seas.shp") # for Lab computer
med_seas <- st_read("~/1Masters/GIS/Med/iho.shp") # For personal laptop

# # Map for Boops boops [This is what needs iteration]
# boops.boops <- med_mat_minimal %>%
#   select(lon, lat, Boops.boops) %>%
#   filter(Boops.boops != 0)
# 
# (bb_p <- ggplot(boops.boops) +
#     geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
#     geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = 2, alpha = 0.6))
# ggsave(filename = "ObserveMaps/boops_boops_obs.png", plot = bb_p)
# 
# 1st loop: Separate the species into different df's (stored in a list)
med_mat <- read_csv("med_species_matrix.csv")
# Create a data frame with lon, lat and species only:
med_mat_minimal <- med_mat %>%
  select(lon, lat, 17:ncol(.))

# # Create a loop to subset each species obaservation
# spp_list <- NULL # Create an empty list
# for(i in 3:ncol(med_mat_minimal)) { # for each column in this array
#   spp_list[[i-2]] <- med_mat_minimal %>% # create a new list item, relying on this df
#     select(lon, lat, i) %>%  # select only the column of the specified species
#     filter(.[, 3] > 0) # filter for observations (not 0s)
# }
# # there's some kind of a mistake here and the 2 first list elements are duplicates of Anthias anthias, so I will delete them:
# 
# spp_list[[119]]
# 
spp_names <- colnames(med_mat_minimal[3:ncol(med_mat_minimal)])
# names(spp_list) <- spp_names
# names(spp_list)
# # Check if I can find specific object in that list:
# spp_list[["Belone.belone"]]
# # Great success!
# 
# saveRDS(spp_list, file = "spp_obs.rds")

spp_list <- readRDS("spp_obs.rds")

# 2nd loop: create maps:
spp_maps <- list()
for (j in 1:length(spp_list)) {
  
  spp_maps[[j]] <- ggplot(data = spp_list[[j]]) +
    geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
    geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = 5, alpha = 0.6) +
    ggtitle(label = spp_names[j])
  
  ggsave(filename = paste0("ObserveMaps/", spp_names[j], "_obs.png"), plot = last_plot(),
         width = 12, height = 8, units = "in", dpi = 300, device = "png")
  
}

list.files(path = "ObserveMaps") # Check all maps have been written to the directory

# I also wanted to make a Mega_plot of all the species and their observation locations,
# but since the locations overlap, it's useless.

# See how the seasons are spread spatially
med_raw <- read_csv("med_raw.csv")
med_raw$depth <- as.numeric(med_raw$depth) # fix class issue (instead of logic, as is has been parsed)

seasons <- med_raw %>%
  select(country, site, lon, lat, season) %>% 
  group_by(country, site, lon, lat, season) %>% 
  summarise(count = n()) %>% 
  arrange(season)

# Since there is a strong temporal (seasonal) variation in the Med,
# I want to make sure the seasons are spread randomly enough through the Med.
# There might be variation in community temporally, and I don't want to this to create a bias.
seasons_maps <- ggplot(data = seasons) +
    geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
    geom_point(aes(x = lon, y = lat, colour = season, size = count), alpha = 0.5) +
    facet_grid(rows = vars(season))
# ggsave(plot = seasons_maps, filename = "seasons.png", path = "SpatialMaps")

# Maybe the difference is not so great if it's all between spring-autumn - 
# how do we consider the seasons?

