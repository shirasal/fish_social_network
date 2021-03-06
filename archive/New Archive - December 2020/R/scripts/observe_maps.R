# Required packcages:
library(tidyverse)
library(sf)

#### Plotting observations of species as maps ####

# Create a base map of the mediterranean
med_seas <- st_read("~/MSc/MEDATA/Med_World_Seas_IHO_v3_MarineRegions/Medit_seas_IHO.shp")

# 1st loop: Separate the species into different df's (stored as a list)
med_raw <- read_rds("data/medata.Rds")

medata <- med_raw %>%
  group_by(lat, lon, site, trans, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  sample_n(size = 1) %>% 
  ungroup() %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc")

medata %>% as.tibble()
medata %>% rownames() %>% head

# Create a data frame with lon, lat and species only:
med_sps <- medata %>%
  select(lon, lat, 8:ncol(.))

med_sps %>% as.tibble

# Create a loop to subset each species obaservation
spp_list <- NULL # Create an empty list
for(i in 3:ncol(med_sps)) { # for each column in this array
  spp_list[[i-2]] <- med_sps %>% # create a new list item, relying on this df
    select(lon, lat, i) %>%  # select only the column of the specified species
    filter(.[, 3] > 0) # filter for observations (not 0s)
}

spp_list[[30]] # Just to check

# Attach names to species:
spp_names <- colnames(med_sps[3:ncol(med_sps)])
names(spp_list) <- spp_names

spp_list$Epinephelus.marginatus # Just to check

# 2nd loop: create maps
spp_maps <- list()
for (j in 1:length(spp_list)) {
  
  spp_maps[[j]] <- ggplot(data = spp_list[[j]]) +
    geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
    geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = n, alpha = 0.6) +
    ggtitle(label = spp_names[j])
  
  ggsave(filename = paste0("figures/ObserveMaps/", spp_names[j], "_obs.png"), plot = last_plot(),
         width = 12, height = 8, units = "in", dpi = 300, device = "png")
  
}

list.files(path = "figures/ObserveMaps") # Check all maps have been written to the directory
