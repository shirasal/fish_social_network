### Med shapefile creation
library(tidyverse)
library(sf)

world_eez <- read_sf("Spatial/MarReg_World_EEZ_v10/eez_v10.shp")
med_map <- ggplot(data = world_eez) +
  geom_sf() +
  coord_sf(xlim = c(--5.674993, 37.07938), ylim = c(29.00509, 46.75752), expand = TRUE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Mediterranean EEZ map")

med_map

# check pivot functions
