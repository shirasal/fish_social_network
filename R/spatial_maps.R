library(raster)
library(sf)
library(tidyverse)
library(sdmpredictors) 

med_shp <- sf::st_read("C:/Users/shira/Documents/MSc/GIS/Mediterranean Sea/Mediterranean_Sea_Area.shp")
med_ext <- raster::extent(med_shp)

# list_layers(marine = TRUE) %>% View

# Temperature -------------------------------------------------------------
tmean <- load_layers("BO_sstmean") # BO21_tempmean_ss
med_temp <- raster::crop(tmean, med_ext)
map_temp <- rasterToPoints(med_temp)
med_temp_df <- data.frame(map_temp) %>% rename(Longitude = x, Latitude = y, Temperature = layer)


# plot(med_temp)
ggplot(data = med_temp_df) +
  aes(y = Latitude, x = Longitude) + 
  geom_raster(aes(fill = Temperature)) +
  scale_fill_gradientn(colours = c("#313695", "#ffffbf", "#a50026"), name = "Temperature °C") +
  theme_bw() +
  coord_quickmap() + 
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
# ggsave("figures/maps/temperature_med_biooracle.png", device = "png", dpi = 300, scale = c(2,2))


# Productivity ------------------------------------------------------------

ppmean <- load_layers("BO21_ppmean_bdmin")
med_prod <- raster::crop(ppmean, med_ext)
map_prod <- rasterToPoints(med_prod)
med_prod_df <- data.frame(map_prod) %>% rename(Longitude = x, Latitude = y, Productivity = layer)


# plot(med_prod_df)
ggplot(data = med_prod_df) +
  aes(y = Latitude, x = Longitude) + 
  geom_raster(aes(fill = Productivity)) +
  scale_fill_gradientn(colours = c("#99d8c9", "#00441b"), name = "Primary Productivity g*m-3*day-1") +
  theme_bw() +
  coord_quickmap() + 
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
# ggsave("figures/maps/productivity_med_biooracle.png", device = "png", dpi = 300, scale = c(2,2))


# Depth -------------------------------------------------------------------

bathy <- load_layers("BO_bathymean")
med_bathy <- raster::crop(bathy, med_ext)
map_bathy <- rasterToPoints(med_bathy)
med_bathy_df <- data.frame(map_bathy) %>% rename(Longitude = x, Latitude = y, Bathymetry = layer)

# med_depth <- med_raw %>% select(Longitude = lon, Latitude = lat, Depth = depth)

# plot(med_bathy_df)
ggplot(data = med_bathy_df) +
  aes(y = Latitude, x = Longitude) + 
  geom_raster(aes(fill = Bathymetry)) +
  # geom_point(data = med_depth %>% na.omit(), aes(y = Latitude, x = Longitude, col = Depth), shape = 15, size = 6, alpha = 0.3) + 
  # scale_color_gradientn(colours = c("#f7fcfd", "#4d004b"), name = " Sampled Depth (m)") +
  scale_fill_gradientn(colours = c("#08306b", "#deebf7"), name = "Depth (m)") +
  theme_bw() +
  coord_quickmap() + 
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
# ggsave("figures/maps/depth_med_biooracle.png", device = "png", dpi = 300, scale = c(2,2))



# MPAs --------------------------------------------------------------------
med_raw %>% 
  ungroup() %>% 
  mutate(mpa = case_when(enforcement == 1 | enforcement == 0 ~ FALSE,
                         enforcement == 2 | enforcement == 3 ~ TRUE)) %>% 
  filter(!is.na(mpa)) %>% 
  ggplot() + 
  geom_sf(data = med_shp, colour = "black", fill = "#76b8d9", alpha = .6) +
  geom_point(aes(x = lon, y = lat, col = mpa, shape = mpa), size = 5) +
  scale_color_manual(values = c("chartreuse4", "orchid3"), labels = c("Not protected", "Protected"), name = "MPA") +
  scale_shape_manual(values = c(13, 19), labels = c("Not protected", "Protected"), name = "MPA") +
  xlab("Longitude") + ylab("Latitude") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, angle = 90),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right")
ggsave("figures/maps/sampled_mpas.png", device = "png", dpi = 300, width = 10, height = 4.5)

