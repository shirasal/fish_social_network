source("R/packages.R")

# Functions ---------------------------------------------------------------

### Create species matrix - for guild with environmental variables
create_spp_mat <- function(dataset, guild, covariate){
  cols <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)
  dataset %>%
    group_by_at(.vars = cols) %>%
    summarise(n = sum(abundance)) %>% 
    spread(species, n, fill = 0) %>% 
    ungroup() %>%
    na.omit() %>% 
    mutate(loc = paste(site, trans)) %>% 
    group_by(loc) %>%
    column_to_rownames("loc") %>% 
    select(all_of(guild), all_of(covariate)) 
}

### Summarise the relative importance of each type of covariate for each species
rel_imp_sum <- function(guild_mod){
  env_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>%  # Take the taxa model and apply the following:
                         filter(Variable %in% env_vector) %>%  # Filter by relevant covariates
                         summarise(n = sum(Rel_importance))) %>% # Summarise Rel_importance column
    unlist() %>% # Take out of the list
    enframe(name = "species", value = "env_rel_imp") %>%  # Rearrange
    mutate(species = str_sub(string = species, end = -3)) # Fix species names
  
  anthro_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                            filter(Variable %in% anthro_vector) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "anthro_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  biotic_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                            filter(!(Variable %in% env_vector | Variable %in% anthro_vector | str_detect(string = Variable, pattern = "_"))) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "biotic_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_bio_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                             filter(str_detect(string = Variable, pattern = "temp_")) %>% 
                             summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "env_bio_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  anthro_bio_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                                filter(str_detect(string = Variable, pattern = "mpa_")) %>% 
                                summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "mpa_bio_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_relimp %>%
    left_join(anthro_relimp, by = "species") %>%
    left_join(biotic_relimp, by = "species") %>%
    left_join(env_bio_relimp, by = "species") %>%
    left_join(anthro_bio_relimp, by = "species")
}


### Plot relative importance of covariates by covariate for each species, within guild:
plot_relimp <- function(rel_imp_df, col, guild_name){
  rel_imp_df %>% 
    pivot_longer(2:length(.)) %>% 
    rename(species = species, covariate = name, rel_imp = value) %>%
    mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
    mutate(facet.title = case_when(covariate == "env" ~ "Environment",
                                   covariate == "anthro" ~ "MPA",
                                   covariate == "biotic" ~ "Biotic Associations",
                                   covariate == "env_bio" ~ "Temp * Biotic",
                                   covariate == "mpa_bio" ~ "MPA * Biotic")) %>% 
    mutate(facet.title = fct_relevel(facet.title, 
                                     "Environment", "MPA", "Biotic Associations",
                                     "Temp * Biotic", "MPA * Biotic")) %>% 
    ggplot() +
    aes(x = species, y = rel_imp) +
    stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = col) +
    facet_wrap(~facet.title, nrow = 1) +
    labs(subtitle = guild_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside",
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          strip.text.x = element_text(size = 12, face = "bold"))
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

grps_col <- "#eccbae"
dip_col <- "#d29a4c"
herb_col <- "#145d82"

all_guilds <- list(groupers = groupers, seabreams = diplodus, herbivores = herbivores)

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
  group_by(lon, lat, site, trans, species, mpa, temp, depth, prod) %>% 
  summarise(abundance = sum(sp.n)) %>%
  select(site, lon, lat, trans, species, abundance, mpa, temp, depth, prod) %>% 
  ungroup()

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
  group_by(lon, lat, site, trans, species, guild, mpa, temp, depth, prod) %>% 
  summarise(abundance = sum(sp.n)) %>%
  select(site, lon, lat, trans, species, abundance, guild, mpa, temp, depth, prod) %>% 
  ungroup()

### Mean vectors for continuous variables
mean_depth <- median(scale(med_raw$depth), na.rm = TRUE)
mean_prod <- median(scale(med_raw$pp_mean))
mean_temp <- median(scale(med_raw$tmean))

### Create species matrices
grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, covariate = c("mpa", "temp", "depth", "prod"))
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, covariate = c("mpa", "temp", "depth", "prod"))
herb_mat <- create_spp_mat(dataset = med_clean, guild = herbivores, covariate = c("mpa", "temp", "depth", "prod"))

# save.image(file = "data/base_data_and_matrices.RData")
load("data/base_data_and_matrices.RData")

# Raw data view -----------------------------------------------------------

## BOXPLOTS per GUILD

grps_boxplot <- guilds_data %>% 
  filter(species %in% groupers) %>% 
  ggplot() +
  aes(x = species, y = abundance) +
  geom_boxplot(col = grps_col) +
  xlab("") + ylab("Abundance") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

dip_boxplot <- guilds_data %>% 
  filter(species %in% diplodus) %>% 
  ggplot() +
  aes(x = species, y = abundance) +
  geom_boxplot(col = dip_col) +
  xlab("") + ylab("Abundance") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

herb_boxplot <- guilds_data %>% 
  filter(species %in% herbivores) %>% 
  ggplot() +
  aes(x = species, y = abundance) +
  geom_boxplot(col = herb_col) +
  xlab("") + ylab("Abundance") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

boxplot_guilds <- list(grps_boxplot, dip_boxplot, herb_boxplot)
patchwork::wrap_plots(boxplot_guilds, ncol = 1) %>% 
  ggsave(filename = "guilds_boxplots.png", 
         device = "png", path = "figures", height = 8, width = 6, units = "in")

## BOXPLOTS per GUILD - transformed

grps_log_boxplot <- guilds_data %>% 
  filter(species %in% groupers) %>% 
  ggplot() +
  aes(x = species, y = log2(abundance)) +
  geom_boxplot(col = grps_col) +
  xlab("") + ylab("Abundance (log2)") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

dip_log_boxplot <- guilds_data %>% 
  filter(species %in% diplodus) %>% 
  ggplot() +
  aes(x = species, y = log2(abundance)) +
  geom_boxplot(col = dip_col) +
  xlab("") + ylab("Abundance (log2)") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

herb_log_boxplot <- guilds_data %>% 
  filter(species %in% herbivores) %>% 
  ggplot() +
  aes(x = species, y = log2(abundance)) +
  geom_boxplot(col = herb_col) +
  xlab("") + ylab("Abundance (log2)") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

boxplot_guilds_log <- list(grps_log_boxplot, dip_log_boxplot, herb_log_boxplot)
patchwork::wrap_plots(boxplot_guilds_log, ncol = 1) %>% 
  ggsave(filename = "guilds_boxplots_log.png", 
         device = "png", path = "figures", height = 8, width = 6, units = "in")

# boxplot_guilds <- list()
# for (j in 1:length(all_guilds)) {
#   boxplot_guilds[[i]] <- guilds_data %>% 
#     filter(species %in% all_guilds[[j]]) %>% 
#     group_by(lon, lat, species) %>% 
#     summarise(mean_abund = mean(abundance)) %>% 
#     ggplot(aes(x = species, y = mean_abund)) +
#     geom_boxplot() +
#     xlab("Species") + ylab("Abundance") + 
#     theme(axis.text.x = element_text(angle = 45))
# }


## HISTOGRAMS per SPECIES
grps_hist <- guilds_data %>% 
  filter(species %in% groupers) %>% 
  group_by(lon, lat, species) %>% 
  summarise(mean_abund = mean(abundance)) %>% 
  ggplot() +
  aes(log2(mean_abund)) +
  geom_histogram(fill = "#eccbae", binwidth = 0.2) +
  xlab("Abundance (log2)") + ylab("Frequency") +
  facet_wrap(~species)

dip_hist <- guilds_data %>% 
  filter(species %in% diplodus) %>% 
  group_by(lon, lat, species) %>% 
  summarise(mean_abund = mean(abundance)) %>% 
  ggplot() +
  aes(log2(mean_abund)) +
  geom_histogram(fill = "#d29a4c", binwidth = .2) +
  xlab("Abundance (log2)") + ylab("Frequency") +
  facet_wrap(~species)

herb_hist <- guilds_data %>% 
  filter(species %in% herbivores) %>% 
  group_by(lon, lat, species) %>% 
  summarise(mean_abund = mean(abundance)) %>% 
  ggplot() +
  aes(log2(mean_abund)) +
  geom_histogram(fill = "#145d82", binwidth = 0.3) +
  xlab("Abundance (log2)") + ylab("Frequency") +
  facet_wrap(~species)

species_histograms <- list(grps_hist, dip_hist, herb_hist)
species_histograms

# for(i in 1:length(species_histograms)){
#   ggsave(plot = species_histograms[[i]], filename = str_glue("figures/{i}.png"), device = "png")
# }


## SPECIES ABUNDANCE ~ COVARIATE per guild

guilds_data %>% 
  filter(species %in% groupers) %>% 
  group_by(species, temp, depth, prod, mpa) %>% 
  summarise(mean_abund = mean(abundance)) %>% 
  ggplot() +
  aes(x = temp, y = log2(mean_abund)) +
  geom_point(col = grps_col) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (scaled)") + ylab("Abundance (log2)") +
  facet_wrap(~species)

guilds_data %>% 
  filter(species %in% diplodus) %>% 
  group_by(species, temp, depth, prod, mpa) %>% 
  summarise(mean_abund = mean(abundance)) %>% 
  ggplot() +
  aes(x = temp, y = log2(mean_abund)) +
  geom_point(col = dip_col) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (scaled)") + ylab("Abundance (log2)") +
  facet_wrap(~species)

guilds_data %>% 
  filter(species %in% herbivores) %>% 
  group_by(species, temp, depth, prod, mpa) %>% 
  summarise(mean_abund = mean(abundance)) %>% 
  ggplot() +
  aes(x = temp, y = log2(mean_abund)) +
  geom_point(col = herb_col) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (scaled)") + ylab("Abundance (log2)") +
  facet_wrap(~species)

# Species maps ------------------------------------------------------------

# Base map of the Mediterranean Sea
med_seas <- sf::st_read("C:/Users/shira/Documents/MSc/medata/Med_World_Seas_IHO_v3_MarineRegions/Medit_seas_IHO.shp")
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
    geom_sf(data = med_seas, colour = "black", fill = "#00E5E5", alpha = 0.3) +
    geom_point(aes(x = lon, y = lat), colour = "#F7347A", size = 5, alpha = 0.6) +
    ggtitle(label = str_replace(spp_list[[j]]$species, "\\.", "\\ ")) +
    xlab("") + ylab("") + 
    theme(title = element_text(face = "italic"))
  
  # ggsave(filename = paste0(spp_list[[j]]$species, "_raw_obs.png"), path = "figures/species_maps/", plot = last_plot(),
  #        width = 12, height = 8, units = "in", dpi = 300, device = "png")
  
}

spp_maps[[1]] # Check

# # Plot all together in two methods (very time consuming)
# patchwork::wrap_plots(spp_maps) %>% ggsave(filename = "species_maps.png", 
#                                            device = "png", path = "figures", height = 16, width = 30, units = "in")

list.files(path = "figures/species_maps") # Check all maps have been written to the directory



# Models ------------------------------------------------------------------

### Poisson

grps_pois <- MRFcov(grps_mat, n_nodes = 5, family = "poisson")
dip_pois <- MRFcov(dip_mat, n_nodes = 4, family = "poisson")
herb_pois <- MRFcov(herb_mat, n_nodes = 4, family = "poisson")

## Check for interactions
lapply(grps_pois$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(dip_pois$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(herb_pois$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

## Relative importance summary
grps_pois_relimp <- rel_imp_sum(grps_pois)
dip_pois_relimp <- rel_imp_sum(dip_pois)
herb_pois_relimp <- rel_imp_sum(herb_pois)

p_relimp_grps_pois <- plot_relimp(grps_pois_relimp, grps_col, "Groupers")
# ggsave("p_relimp_grps_pois_nonspat.png", p_relimp_grps_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_pois <- plot_relimp(dip_pois_relimp, dip_col, "Diplodus")
# ggsave("p_relimp_dip_pois_nonspat.png", p_relimp_dip_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_pois <- plot_relimp(herb_pois_relimp, herb_col, "Herbivores")
# ggsave("p_relimp_herb_pois_nonspat.png", p_relimp_herb_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
egg::ggarrange(p_relimp_grps_pois, p_relimp_dip_pois, p_relimp_herb_pois) # %>% ggsave("rel_imp_pois_nonspat.png", "png", "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")


### Gaussian

std_grps_mat <- grps_mat %>% mutate(across(1:5, .fns = scale))
std_dip_mat <- dip_mat %>% mutate(across(1:5, .fns = scale))
std_herb_mat <- herb_mat %>% mutate(across(1:5, .fns = scale))

grps_gaus <- MRFcov(std_grps_mat, n_nodes = 5, family = "gaussian")
dip_gaus <- MRFcov(std_dip_mat, n_nodes = 4, family = "gaussian")
herb_gaus <- MRFcov(std_herb_mat, n_nodes = 4, family = "gaussian")

## Check for interactions
lapply(grps_gaus$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(dip_gaus$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(herb_gaus$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

## Relative importance summary
grps_gaus_relimp <- rel_imp_sum(grps_gaus)
dip_gaus_relimp <- rel_imp_sum(dip_gaus)
herb_gaus_relimp <- rel_imp_sum(herb_gaus)

p_relimp_grps_gaus <- plot_relimp(grps_gaus_relimp, grps_col, "Groupers")
# ggsave("p_relimp_grps_gaus_nonspat.png", p_relimp_grps_gaus, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_gaus <- plot_relimp(dip_gaus_relimp, dip_col, "Diplodus")
# ggsave("p_relimp_dip_gaus_nonspat.png", p_relimp_dip_gaus, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_gaus <- plot_relimp(herb_gaus_relimp, herb_col, "Herbivores")
# ggsave("p_relimp_herb_gaus_nonspat.png", p_relimp_herb_gaus, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
egg::ggarrange(p_relimp_grps_gaus, p_relimp_dip_gaus, p_relimp_herb_gaus) # %>% ggsave("rel_imp_gaus_nonspat.png", "png", "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")

grps_data %>% 
  group_by(lon, lat, site, trans, species, mpa, temp, depth, prod) %>% 
  summarise(abundance = sum(abundance))
grps_data %>% colnames()


