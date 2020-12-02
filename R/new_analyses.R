source("R/packages.R")

# Functions ---------------------------------------------------------------

### Create species matrix - for guild with environmental variables
create_spp_mat <- function(dataset, guild, covariate){
  cols <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)
  dataset %>%
    group_by_at(.vars = cols) %>%
    summarise(n = sum(sp.n)) %>% 
    spread(species, n, fill = 0) %>% 
    ungroup() %>%
    na.omit() %>% 
    mutate(loc = paste(site, trans)) %>% 
    group_by(loc) %>%
    column_to_rownames("loc") %>% 
    select(all_of(guild), all_of(covariate)) 
}

# Data --------------------------------------------------------------------

### Variables (fitting with med_clean)
env_vector <- c("temp", "depth", "prod")
anthro_vector <- c("mpa")

### Guilds
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Sparisoma.cretense")

all_guilds <- list(groupers, diplodus, herbivores)
names(all_guilds) <- c("groupers", "seabreams", "herbivores")

### MEData
med_raw <- read_rds("data/medata.Rds") %>% 
  filter(data.origin != "azz_asi") %>% # presence-absence
  ungroup()

med_clean <- med_raw %>%
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         sal = scale(sal_mean),
         prod = scale(pp_mean)) %>%
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)

guilds_data <- med_raw %>%
  filter(species %in% c(groupers, diplodus, herbivores)) %>% 
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         sal = scale(sal_mean),
         prod = scale(pp_mean),
         guild = case_when(species %in% groupers ~ "groupers",
                           species %in% diplodus ~ "seabreams",
                           species %in% herbivores ~ "herbivores")) %>%
  select(site, lon, lat, trans, species, sp.n, guild, mpa, temp, depth, prod)

### Mean vectors for continuous variables
mean_depth <- median(scale(med_raw$depth), na.rm = TRUE)
mean_prod <- median(scale(med_raw$pp_mean))
mean_temp <- median(scale(med_raw$tmean))

### Create species matrices
grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, covariate = c("mpa", "temp", "depth", "prod"))
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, covariate = c("mpa", "temp", "depth", "prod"))
herb_mat <- create_spp_mat(dataset = med_clean, guild = herbivores, covariate = c("mpa", "temp", "depth", "prod"))

# save.image(file = "data/base_data_and_matrices.RData")
# load("data/base_data_and_matrices.RData")

# Raw data view -----------------------------------------------------------

guilds_data %>% 
  filter(species %in% groupers) %>% 
  ggplot() +
  aes(x = species, y = sp.n) +
  geom_boxplot()

guilds_data %>% 
  filter(species %in% groupers) %>% 
  group_by(lon, lat, species) %>% 
  summarise(mean_abund = mean(sp.n)) %>% 
  ggplot() +
  aes(x = species, y = mean_abund) +
  geom_boxplot()

lapply(all_guilds, myfun, df = df)

guild_df <- NULL
for (i in all_guilds){
  guild_df[[i]] <- guilds_data %>%
    filter(guild == i)
}
spp_list[[5]] # Check

spp_maps <- list()
for (j in 1:length(spp_list)) {
  spp_maps[[j]] <- ggplot(data = spp_list[[j]]) +
    geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
    geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = 5, alpha = 0.6) +
    ggtitle(label = str_replace(spp_list[[j]]$species, "\\.", "\\ ")) +
    xlab("") + ylab("") + 
    theme(title = element_text(face = "italic"))
  
  ggsave(filename = paste0(j, "_raw_obs.png"), path = "figures/species_maps/", plot = last_plot(),
         width = 12, height = 8, units = "in", dpi = 300, device = "png")
  
}

# Species maps ------------------------------------------------------------

# Base map of the Mediterranean Sea
med_seas <- sf::st_read("C:/Users/shira/Documents/MSc/medata/Med_World_Seas_IHO_v3_MarineRegions/Medit_seas_IHO.shp")
species_locations <- guilds_data %>% select(lon, lat, species, sp.n, guild) %>% arrange(guild)
spp_list <- NULL
for (i in unique(species_locations$species)){
  spp_list[[i]] <- species_locations %>%
    filter(species == i)
}
spp_list[[5]] # Check

spp_maps <- list()
for (j in 1:length(spp_list)) {
  spp_maps[[j]] <- ggplot(data = spp_list[[j]]) +
    geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
    geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = 5, alpha = 0.6) +
    ggtitle(label = str_replace(spp_list[[j]]$species, "\\.", "\\ ")) +
    xlab("") + ylab("") + 
    theme(title = element_text(face = "italic"))
  
  ggsave(filename = paste0(spp_list[[j]]$species, "_raw_obs.png"), path = "figures/species_maps/", plot = last_plot(),
         width = 12, height = 8, units = "in", dpi = 300, device = "png")
  
}

spp_maps[[1]] # Check

# # Plot all together in two methods (very time consuming)
patchwork::wrap_plots(spp_maps) %>% ggsave(filename = "species_maps.png", 
                                           device = "png", path = "figures", height = 16, width = 30, units = "in")

list.files(path = "figures/species_maps") # Check all maps have been written to the directory




