# source("R/packages.R")
# load("data/data_and_objects.RData")

# Base map of the Mediterranean Sea
med_seas <- sf::st_read("C:/Users/shira/Documents/MSc/GIS/Mediterranean Sea/Mediterranean_Sea_Area.shp")
species_locations <- guilds_data %>% select(lon, lat, species, abundance, guild) %>% arrange(guild)
spp_list <- NULL
for (i in unique(species_locations$species)){
  spp_list[[i]] <- species_locations %>%
    filter(species == i)
}
spp_list[[5]] # Check

spp_maps <- list()
for (j in 1:length(spp_list)) {
  spp_maps[[j]] <- ggplot(data = spp_list[[j]]) +
    geom_sf(data = med_seas, colour = "black", fill = "#b0ddf4") +
    geom_point(aes(x = lon, y = lat), colour = "#f7c11c", size = 5, alpha = 0.6) +
    ggtitle(label = str_replace(spp_list[[j]]$species, "\\.", "\\ ")) +
    xlab("") + ylab("") + 
    theme_bw() +
    theme(title = element_text(face = "italic"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # ggsave(filename = paste0(spp_list[[j]]$species, "_raw_obs.png"), path = "figures/species_maps/", plot = last_plot(),
  #        width = 12, height = 8, units = "in", dpi = 300, device = "png")
  
}

spp_maps[[1]] # Check

# # Plot all together (very time consuming)
# patchwork::wrap_plots(spp_maps) 
# ggsave(filename = "species_maps.png", device = "png", path = "figures", height = 16, width = 30, units = "in")

list.files(path = "figures/species_maps") # Check all maps have been written to the directory


