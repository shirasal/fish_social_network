# library(rgbif)
library(sf)
library(sp)
library(tidyverse)

# med_seas <- st_read("~/MEData/medata/Med_World_Seas_IHO_v3_MarineRegions/Medit_seas_IHO.shp")

## Mycteroperca rubra
# name_backbone(name = "Mycteroperca rubra", rank = "species") %>% t
# 
# mrubra_occ <- occ_search(scientificName = "Mycteroperca rubra", return = "data") %>%
#   dplyr::select(latitude = decimalLatitude, longitude = decimalLongitude, key, species = scientificName) %>% 
#   filter(species == "Mycteroperca rubra (Bloch, 1793)") %>% 
#   na.omit()
# 
# mrubra_sf <- mrubra_occ %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# 
# hulls <- mrubra_sf %>%
#   group_by(species) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_convex_hull()
# 
# int_sp <- st_intersection(mrubra_sf, med_seas)
# int <- st_intersection(hulls, med_seas)

mrubra_occ <- read_rds("data/mrubra_gbif.Rds")
mrubra_sf <- st_read("issues/Mrubra_Ecostae/mrubra_gbif_med.shp")
mr_hulls <- st_read("issues/Mrubra_Ecostae/mrubra_hull_med.shp")

med_raw <- read_rds("data/medata.Rds")
mrubra_medata <- med_raw %>% filter(species == "Mycteroperca.rubra") %>% select(lon, lat, species) %>% 
  group_by(lat, lon) %>% 
  summarise(obs = n())

mrubra_medata_sf <- mrubra_medata %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview::mapview(list(mrubra_medata_sf, mr_hulls))


## Epinephelus costae
# name_backbone(name = "Epinephelus costae", rank = "species") %>% t
# 
# ecostae_occ <- occ_search(scientificName = "Epinephelus costae", return = "data") %>%
#   dplyr::select(latitude = decimalLatitude, longitude = decimalLongitude, key, species = scientificName) %>% 
#   filter(species == "Epinephelus costae (Steindachner, 1878)") %>% 
#   na.omit()
# write_rds(ecostae_occ, "data/ecostae_gbif.Rds")
# 
# ecostae_sf <- ecostae_occ %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# 
# ec_hulls <- ecostae_sf %>%
#   group_by(species) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_convex_hull()
# 
# int_sp <- st_intersection(ecostae_sf, med_seas)
# write_sf(int_sp, "issues/Mrubra_Ecostae/ecostae_gbif_obs.shp")
# int <- st_intersection(ec_hulls, med_seas)
# write_sf(int, "issues/Mrubra_Ecostae/ecostae_hull_med.shp")

ecostae_occ <- read_rds("data/ecostae_gbif.Rds")
ecostae_sf <- st_read("issues/Mrubra_Ecostae/ecostae_gbif_obs.shp")
ec_hulls <- st_read("issues/Mrubra_Ecostae/ecostae_hull_med.shp")

med_raw <- read_rds("data/medata.Rds")
ecostae_medata <- med_raw %>% filter(species == "Epinephelus.costae") %>% select(lon, lat, species) %>% 
  group_by(lat, lon) %>% 
  summarise(obs = n())

ecostae_medata_sf <- ecostae_medata %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview::mapview(list(ecostae_medata_sf, ec_hulls))

