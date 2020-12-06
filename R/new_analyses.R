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

### MPA predictions visualisation
vis_mpa_pred_pair <- function(species_i, species_j, spp_mat, spp_mod, guild){
  # Create a vector of all other species except species_j
  all_other_species <- guild[-which(guild == species_j)]
  
  # Scenario 1: species_j is absent, other species are at their mean abundance
  j_abs <- spp_mat %>% 
    mutate(temp = median(temp),
           depth = median(depth),
           prod = median(prod),
           across(.cols = all_of(species_j), .fns = function(x) 0),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(mpa) %>% 
    sample_n(1) %>% 
    ungroup()
  # Scenario 2: species_j is at its 90th percentile abundance, other species are at their mean abundance
  j_max <- spp_mat %>% 
    mutate(temp = median(temp),
           depth = median(depth),
           prod = median(prod),
           across(.cols = all_of(species_j), .fns = function(x) quantile(x, 0.9)),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(mpa) %>% 
    sample_n(1) %>% 
    ungroup()
  
  # Create predictions
  ## For when species j is absent
  predict_abs <- predict_MRF(j_abs, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(mpa = j_abs$mpa)
  ## For when species j is absent
  predict_max <- predict_MRF(j_max, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(mpa = j_max$mpa)
  
  # Put the two scenarios together
  mpa_predict <- bind_rows(predict_abs, predict_max, .id = "scenario") %>% 
    mutate(scenario = case_when(scenario == 1 ~ "absent",
                                scenario == 2 ~ "present"))
  
  # Visualise the predictions
  ## Create a dataframe with all the predictions, sorted by scenario
  predictions_mpa <- mpa_predict %>% 
    pivot_longer(cols = all_of(2:5),
                 names_to = "species",
                 values_to = "prediction", 
                 names_repair = "minimal")
  
  ## Plot the predictions:
  predictions_mpa %>%
    filter(species == species_i) %>% 
    ggplot() +
    aes(x = mpa, y = prediction, fill = scenario) +
    stat_summary(geom = "bar", fun = "mean", position = "dodge") +
    stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.8), width = 0.2) +
    xlab("MPA") + ylab("Abundance Prediction") +
    labs(title = "Observation predictions",
         subtitle = stringr::str_replace(species_i, "\\.", "\\ "),
         fill = stringr::str_replace(species_j, "\\.", "\\ ")) +
    scale_fill_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
    theme(legend.title = element_text(face = "italic"), plot.subtitle = element_text(face = "italic"))
}

### Temperature predictions visualisation
vis_temp_pred_pair <- function(species_i, species_j, spp_mat, spp_mod, guild){
  # Create a vector of all other species except species_j
  all_other_species <- guild[-which(guild == species_j)]
  
  # Scenario 1: species_j is absent, other species are at their mean abundance
  j_abs <- spp_mat %>% 
    mutate(depth = median(depth),
           prod = median(prod),
           mpa = TRUE,
           temperature = spp_mat$temp * attr(spp_mat$temp, 'scaled:scale') + attr(spp_mat$temp, 'scaled:center'),
           across(.cols = all_of(species_j), .funs = function(x) 0),
           across(.cols = all_of(all_other_species), .fns = mean)) %>% 
    group_by(temperature = round(temperature, digits = 1)) %>% 
    sample_n(1) %>%  
    ungroup() %>% 
    mutate(temp = scale(temperature))
  # Scenario 2: species_j is at its 90th percentile abundance, other species are at their mean abundance
  j_max <- spp_mat %>% 
    mutate(depth = median(depth),
           prod = median(prod),
           mpa = TRUE,
           temperature = spp_mat$temp * attr(spp_mat$temp, 'scaled:scale') + attr(spp_mat$temp, 'scaled:center'),
           across(.cols = all_of(species_j), .funs = quantile(species_j, 0.9)),
           across(.cols = all_of(all_other_species), .fns = mean)) %>% 
    group_by(temperature = round(temperature, digits = 1)) %>% 
    sample_n(1) %>%  
    ungroup() %>% 
    mutate(temp = scale(temperature))
  
  # Create predictions
  ## For when species j is absent
  predict_abs <- predict_MRF(j_abs, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(temp = j_abs$temperature)
  ## For when species j is absent
  predict_max <- predict_MRF(j_max, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(temp = j_max$temperature)
  
  # Put the two scenarios together
  temp_predict <- bind_rows(predict_abs, predict_max, .id = "scenario") %>% 
    mutate(scenario = case_when(scenario == 1 ~ "absent",
                                scenario == 2 ~ "present"))
  
  # Visualise the predictions
  ## Create a dataframe with all the predictions, sorted by scenario
  predictions_temp <- temp_predict %>% 
    pivot_longer(cols = all_of(2:5),
                 names_to = "species",
                 values_to = "prediction", 
                 names_repair = "minimal")
  
  ## Plot the predictions:
  predictions_temp %>%
    filter(species == species_i) %>% 
    ggplot() +
    aes(x = temp, y = prediction, col = scenario) +
    geom_smooth(method = "lm", formula = y ~ x, cex = 3, alpha = 0.1) +
    xlab("Temperature (°C)") + ylab("Observation predictions") +
    labs(subtitle = stringr::str_replace(species_i, "\\.", "\\ "),
         col = stringr::str_replace(species_j, "\\.", "\\ ")) +
    scale_color_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
    theme(legend.title = element_text(face = "italic"), plot.subtitle = element_text(face = "italic"))
}

### Summarise coefficients for each species, separating positive from negative
coefs_sum <- function(guild_mod, guild){
  env_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
                         filter(Variable %in% env_vector) %>%
                         group_by(Standardised_coef > 0) %>% 
                         summarise(coefs = sum(Standardised_coef), .groups = "drop") %>% 
                         transmute(direction = `Standardised_coef > 0`, env_coef = coefs) %>%
                         mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                      .$direction == FALSE ~ "neg",
                                                      TRUE ~ as.character(.$direction)))) %>% 
    bind_rows(.id = "id")
  
  temp_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
                          filter(Variable == "temp") %>%
                          group_by(Standardised_coef > 0) %>% 
                          summarise(coefs = sum(Standardised_coef), .groups = "drop") %>% 
                          transmute(direction = `Standardised_coef > 0`, temp_coef = coefs) %>%
                          mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                       .$direction == FALSE ~ "neg",
                                                       TRUE ~ as.character(.$direction)))) %>% 
    bind_rows(.id = "id")
  
  anthro_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
                            filter(Variable %in% anthro_vector) %>%
                            group_by(Standardised_coef > 0) %>% 
                            summarise(coefs = sum(Standardised_coef), .groups = "drop") %>% 
                            transmute(direction = `Standardised_coef > 0`, mpa_coef = coefs) %>%
                            mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                         .$direction == FALSE ~ "neg",
                                                         TRUE ~ as.character(.$direction)))) %>% 
    bind_rows(.id = "id")

  
  biotic_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
                            filter(Variable %in% colnames(guild_mod$graph)) %>%
                            group_by(Standardised_coef > 0) %>% 
                            summarise(coefs = sum(Standardised_coef), .groups = "drop") %>% 
                            transmute(direction = `Standardised_coef > 0`, bio_coef = coefs) %>%
                            mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                         .$direction == FALSE ~ "neg",
                                                         TRUE ~ as.character(.$direction)))) %>% 
    bind_rows(.id = "id")
  
  env_bio_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
                             filter(str_detect(string = Variable, pattern = "temp_")) %>%
                             group_by(Standardised_coef > 0) %>% 
                             summarise(coefs = sum(Standardised_coef), .groups = "drop") %>%
                             transmute(direction = `Standardised_coef > 0`, bio_env_coef = coefs) %>%
                             mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                          .$direction == FALSE ~ "neg",
                                                          TRUE ~ as.character(.$direction)))) %>%
    bind_rows(.id = "id")
  
  anthro_bio_effect <- lapply(guild_mod$key_coefs, FUN = function(x) x %>%
                                filter(str_detect(string = Variable, pattern = "mpa_")) %>%
                                group_by(Standardised_coef > 0) %>% 
                                summarise(coefs = sum(Standardised_coef), .groups = "drop") %>%
                                transmute(direction = `Standardised_coef > 0`, bio_mpa_coef = coefs) %>%
                                mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                             .$direction == FALSE ~ "neg",
                                                             TRUE ~ as.character(.$direction)))) %>%
    bind_rows(.id = "id")
  
  
  bind_rows(env_effect, temp_effect, anthro_effect, biotic_effect, 
            env_bio_effect, anthro_bio_effect) %>% 
    arrange(direction, id) %>% 
    rename(species = id)
}

# Data --------------------------------------------------------------------

### Variables (fitting with med_clean)
env_vector <- c("temp", "depth", "prod")
anthro_vector <- c("mpa")

### Guilds
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus", "Serranus.cabrilla", 
              "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Sparisoma.cretense")

guild_colours <- list(grps = "#C54607", dip = "#145D82", herb = "#43AA8B")

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
  summarise(abundance = sum(sp.n), .groups = "drop") %>%
  select(site, lon, lat, trans, species, abundance, mpa, temp, depth, prod)

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
  summarise(abundance = sum(sp.n), .groups = "drop") %>%
  select(site, lon, lat, trans, species, abundance, guild, mpa, temp, depth, prod)

### Mean vectors for continuous variables
mean_depth <- median(scale(med_raw$depth), na.rm = TRUE)
mean_prod <- median(scale(med_raw$pp_mean))
mean_temp <- median(scale(med_raw$tmean))

### Create species matrices
grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, covariate = c("mpa", "temp", "depth", "prod"))
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, covariate = c("mpa", "temp", "depth", "prod"))
herb_mat <- create_spp_mat(dataset = med_clean, guild = herbivores, covariate = c("mpa", "temp", "depth", "prod"))

save.image(file = "data/base_data_and_matrices.RData")
# load("data/base_data_and_matrices.RData")

# Raw data view -----------------------------------------------------------

## BOXPLOTS per GUILD

grps_boxplot <- guilds_data %>% 
  filter(species %in% groupers) %>% 
  ggplot() +
  aes(x = species, y = abundance) +
  geom_boxplot(col = guild_colours$grps) +
  xlab("") + ylab("Abundance") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

dip_boxplot <- guilds_data %>% 
  filter(species %in% diplodus) %>% 
  ggplot() +
  aes(x = species, y = abundance) +
  geom_boxplot(col = guild_colours$dip) +
  xlab("") + ylab("Abundance") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

herb_boxplot <- guilds_data %>% 
  filter(species %in% herbivores) %>% 
  ggplot() +
  aes(x = species, y = abundance) +
  geom_boxplot(col = guild_colours$herb) +
  xlab("") + ylab("Abundance") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

boxplot_guilds <- list(grps_boxplot, dip_boxplot, herb_boxplot)
patchwork::wrap_plots(boxplot_guilds, ncol = 1)
ggsave(filename = "guilds_boxplots.png", device = "png", path = "figures", 
       height = 8, width = 6, units = "in")

## BOXPLOTS per GUILD - transformed

grps_log_boxplot <- guilds_data %>% 
  filter(species %in% groupers) %>% 
  ggplot() +
  aes(x = species, y = log2(abundance)) +
  geom_boxplot(col = guild_colours$grps) +
  xlab("") + ylab("Abundance (log2)") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

dip_log_boxplot <- guilds_data %>% 
  filter(species %in% diplodus) %>% 
  ggplot() +
  aes(x = species, y = log2(abundance)) +
  geom_boxplot(col = guild_colours$dip) +
  xlab("") + ylab("Abundance (log2)") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

herb_log_boxplot <- guilds_data %>% 
  filter(species %in% herbivores) %>% 
  ggplot() +
  aes(x = species, y = log2(abundance)) +
  geom_boxplot(col = guild_colours$herb) +
  xlab("") + ylab("Abundance (log2)") +
  ggtitle(names(guilds_data$guild)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

boxplot_guilds_log <- list(grps_log_boxplot, dip_log_boxplot, herb_log_boxplot)
patchwork::wrap_plots(boxplot_guilds_log, ncol = 1)
ggsave(filename = "guilds_boxplots_log.png", device = "png", path = "figures",
       height = 8, width = 6, units = "in")

## HISTOGRAMS per SPECIES
grps_hist <- guilds_data %>% 
  filter(species %in% groupers) %>% 
  ggplot() +
  aes(log2(abundance)) +
  geom_histogram(fill = guild_colours$grps, binwidth = .5) +
  xlab("Abundance (log2)") + ylab("Frequency") +
  facet_wrap(~species)

dip_hist <- guilds_data %>% 
  filter(species %in% diplodus) %>% 
  ggplot() +
  aes(log2(abundance)) +
  geom_histogram(fill = guild_colours$dip, binwidth = .5) +
  xlab("Abundance (log2)") + ylab("Frequency") +
  facet_wrap(~species)

herb_hist <- guilds_data %>% 
  filter(species %in% herbivores) %>% 
  ggplot() +
  aes(log2(abundance)) +
  geom_histogram(fill = guild_colours$herb, binwidth = .5) +
  xlab("Abundance (log2)") + ylab("Frequency") +
  facet_wrap(~species)

species_histograms <- list(grps = grps_hist, dip = dip_hist, herb = herb_hist)
species_histograms

for(i in 1:length(species_histograms)){
  ggsave(plot = species_histograms[[i]], filename = str_glue("figures/{names(species_histograms)[i]}_hist.png"), device = "png")
}


## SPECIES ABUNDANCE ~ COVARIATE per guild

grps_temp <- guilds_data %>% 
  filter(species %in% groupers) %>% 
  ggplot() +
  aes(x = temp, y = log2(abundance)) +
  geom_point(col = guild_colours$grps) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (scaled)") + ylab("Abundance (log2)") +
  facet_wrap(~species)

dip_temp <- guilds_data %>% 
  filter(species %in% diplodus) %>% 
  ggplot() +
  aes(x = temp, y = log2(abundance)) +
  geom_point(col = guild_colours$dip) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (scaled)") + ylab("Abundance (log2)") +
  facet_wrap(~species)

herb_temp <- guilds_data %>% 
  filter(species %in% herbivores) %>% 
  ggplot() +
  aes(x = temp, y = log2(abundance)) +
  geom_point(col = guild_colours$herb) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (scaled)") + ylab("Abundance (log2)") +
  facet_wrap(~species)

temp_plots <- list(grps = grps_temp, dip = dip_temp, herb = herb_temp)
for(i in 1:length(temp_plots)){
  ggsave(plot = temp_plots[[i]], filename = str_glue("figures/{names(temp_plots)[i]}_temp.png"), device = "png")
}

grps_mpa <- guilds_data %>% 
  filter(species %in% groupers) %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = mpa, y = log2(abundance)) +
  geom_boxplot(col = guild_colours$grps) +
  xlab("MPA") + ylab("Abundance (log2)") +
  facet_wrap(~species)

dip_mpa <- guilds_data %>% 
  filter(species %in% diplodus) %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = mpa, y = log2(abundance)) +
  geom_boxplot(col = guild_colours$dip) +
  xlab("MPA") + ylab("Abundance (log2)") +
  facet_wrap(~species)

herb_mpa <- guilds_data %>% 
  filter(species %in% herbivores) %>% 
  na.omit() %>% 
  ggplot() +
  aes(x = mpa, y = log2(abundance)) +
  geom_boxplot(col = guild_colours$herb) +
  xlab("MPA") + ylab("Abundance (log2)") +
  facet_wrap(~species)

mpa_plots <- list(grps = grps_mpa, dip = dip_mpa, herb = herb_mpa)
for(i in 1:length(mpa_plots)){
  ggsave(plot = mpa_plots[[i]], filename = str_glue("figures/{names(mpa_plots)[i]}_mpa.png"), device = "png")
}



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

# # Plot all together (very time consuming)
# patchwork::wrap_plots(spp_maps) 
# ggsave(filename = "species_maps.png", device = "png", path = "figures", height = 16, width = 30, units = "in")

list.files(path = "figures/species_maps") # Check all maps have been written to the directory



# Models ------------------------------------------------------------------

### Poisson

grps_pois <- MRFcov(grps_mat, n_nodes = 4, family = "poisson")
dip_pois <- MRFcov(dip_mat, n_nodes = 4, family = "poisson")
herb_pois <- MRFcov(herb_mat, n_nodes = 4, family = "poisson")

## Relative importance summary
grps_pois_relimp <- rel_imp_sum(grps_pois)
dip_pois_relimp <- rel_imp_sum(dip_pois)
herb_pois_relimp <- rel_imp_sum(herb_pois)

p_relimp_grps_pois <- plot_relimp(grps_pois_relimp, guild_colours$grps, "Groupers")
# ggsave("p_relimp_grps_pois_nonspat.png", p_relimp_grps_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_pois <- plot_relimp(dip_pois_relimp, guild_colours$dip, "Diplodus")
# ggsave("p_relimp_dip_pois_nonspat.png", p_relimp_dip_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_pois <- plot_relimp(herb_pois_relimp, guild_colours$herb, "Herbivores")
# ggsave("p_relimp_herb_pois_nonspat.png", p_relimp_herb_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
egg::ggarrange(p_relimp_grps_pois, p_relimp_dip_pois, p_relimp_herb_pois)
ggsave(filename = "rel_imp_pois_nonspat.png", device = "png", path = "figures/rel_imp/", 
         dpi = 150, height = 10, width = 10, units = "in")


### Spatial (Poisson)
### Poisson

med_coords <- med_clean %>% 
  distinct(site, trans, lat, lon) %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc") %>% 
  select(lon, lat)
grps_coords <- med_coords %>% 
  filter(rownames(med_coords) %in% rownames(grps_mat))
dip_coords <- med_coords %>% 
  filter(rownames(med_coords) %in% rownames(dip_mat))
herb_coords <- med_coords %>% 
  filter(rownames(med_coords) %in% rownames(herb_mat))

grps_pois_spat <- MRFcov_spatial(grps_mat, n_nodes = 4, family = "poisson", coords = grps_coords)
dip_pois_spat <- MRFcov_spatial(dip_mat, n_nodes = 4, family = "poisson", coords = dip_coords)
herb_pois_spat <- MRFcov_spatial(herb_mat, n_nodes = 4, family = "poisson", coords = herb_coords)

## Relative importance summary
grps_pois_spat_relimp <- rel_imp_sum(grps_pois_spat)
dip_pois_spat_relimp <- rel_imp_sum(dip_pois_spat)
herb_pois_spat_relimp <- rel_imp_sum(herb_pois_spat)

p_relimp_grps_pois_spat <- plot_relimp(grps_pois_spat_relimp, guild_colours$grps, "Groupers")
p_relimp_dip_pois_spat <- plot_relimp(dip_pois_spat_relimp, guild_colours$dip, "Diplodus")
p_relimp_herb_pois_spat <- plot_relimp(herb_pois_spat_relimp, guild_colours$herb, "Herbivores")
egg::ggarrange(p_relimp_grps_pois_spat, p_relimp_dip_pois_spat, p_relimp_herb_pois_spat)
  ggsave(filename = "rel_imp_pois_spat.png", device = "png", path = "figures/rel_imp/", 
         dpi = 150, height = 10, width = 10, units = "in")

### Gaussian ----

std_grps_mat <- grps_mat %>% mutate(across(1:4, .fns = scale))
std_dip_mat <- dip_mat %>% mutate(across(1:4, .fns = scale))
std_herb_mat <- herb_mat %>% mutate(across(1:4, .fns = scale))

grps_gaus <- MRFcov(std_grps_mat, n_nodes = 4, family = "gaussian")
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

p_relimp_grps_gaus <- plot_relimp(grps_gaus_relimp, guild_colours$grps, "Groupers")
# ggsave("p_relimp_grps_gaus_nonspat.png", p_relimp_grps_gaus, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_gaus <- plot_relimp(dip_gaus_relimp, guild_colours$dip, "Diplodus")
# ggsave("p_relimp_dip_gaus_nonspat.png", p_relimp_dip_gaus, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_gaus <- plot_relimp(herb_gaus_relimp, guild_colours$herb, "Herbivores")
# ggsave("p_relimp_herb_gaus_nonspat.png", p_relimp_herb_gaus, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
egg::ggarrange(p_relimp_grps_gaus, p_relimp_dip_gaus, p_relimp_herb_gaus) %>% 
  ggsave(filename = "rel_imp_gaus_nonspat.png", device = "png", path = "figures/rel_imp/", 
         dpi = 150, height = 10, width = 10, units = "in")

### Abundance rankings + Gaussian -----

grps_rank <- grps_mat %>% 
  mutate(across(1:4, .fns = rank))
dip_rank <- dip_mat %>% 
  mutate(across(1:4, .fns = rank))
herb_rank <- herb_mat %>% 
  mutate(across(1:4, .fns = rank))

# Check the rankings, because something here is odd
t <- grps_rank %>% 
  pivot_longer(cols = all_of(groupers), names_to = "species", values_to = "rank")
median(t$rank)

grps_rank %>% 
  pivot_longer(cols = all_of(groupers), names_to = "species", values_to = "rank") %>% 
  ggplot() +
  aes(rank) +
  geom_histogram(fill = guild_colours$grps, binwidth = 200) +
  xlab("Rank") + ylab("Frequency") +
  facet_wrap(~species)

dip_rank %>% 
  pivot_longer(cols = all_of(diplodus), names_to = "species", values_to = "rank") %>% 
  ggplot() +
  aes(rank) +
  geom_histogram(fill = guild_colours$dip, binwidth = 200) +
  xlab("Rank") + ylab("Frequency") +
  facet_wrap(~species)

herb_rank %>% 
  pivot_longer(cols = all_of(herbivores), names_to = "species", values_to = "rank") %>% 
  ggplot() +
  aes(rank) +
  geom_histogram(fill = guild_colours$herb, binwidth = 200) +
  xlab("Rank") + ylab("Frequency") +
  facet_wrap(~species)

grps_rank_mod <- MRFcov(grps_rank, n_nodes = 4, family = "gaussian")
dip_rank_mod <- MRFcov(dip_rank, n_nodes = 4, family = "gaussian")
herb_rank_mod <- MRFcov(herb_rank, n_nodes = 4, family = "gaussian")

## Check for interactions
lapply(grps_rank_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(dip_rank_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(herb_rank_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

## Relative importance summary
grps_rank_relimp <- rel_imp_sum(grps_rank_mod)
dip_rank_relimp <- rel_imp_sum(dip_rank_mod)
herb_rank_relimp <- rel_imp_sum(herb_rank_mod)

p_relimp_grps_rank <- plot_relimp(grps_rank_relimp, guild_colours$grps, "Groupers")
# ggsave("p_relimp_grps_rank_nonspat.png", p_relimp_grps_rank, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_rank <- plot_relimp(dip_rank_relimp, guild_colours$dip, "Diplodus")
# ggsave("p_relimp_dip_rank_nonspat.png", p_relimp_dip_rank, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_rank <- plot_relimp(herb_rank_relimp, guild_colours$herb, "Herbivores")
# ggsave("p_relimp_herb_rank_nonspat.png", p_relimp_herb_rank, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
egg::ggarrange(p_relimp_grps_rank, p_relimp_dip_rank, p_relimp_herb_rank) %>% 
  ggsave(filename = "rel_imp_rank_nonspat.png", device = "png", path = "figures/rel_imp/", 
         dpi = 150, height = 10, width = 10, units = "in")




# Visualisations ----------------------------------------------------------

### Poisson

## Check for interactions
lapply(grps_pois$key_coefs, function(x) x %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(dip_pois$key_coefs, function(x) x %>% 
         filter(str_detect(string = Variable, pattern = "_")))

## Temperature

# y                  ~ cov_biotic                  rel_imp           coef
# -----------------  -----------------------    -------------      -----------
# Epinephelus.costae ~ temp_Serranus.cabrilla     0.07217235        0.05212067
# Diplodus.annularis ~ temp_Diplodus.vulgaris     0.12332969        0.08714881
# Diplodus.sargus ~ temp_Diplodus.vulgaris     0.05466831         0.1373128
# Diplodus.vulgaris ~ temp_Diplodus.sargus     0.05159193        0.13018894
# Diplodus.vulgaris ~ temp_Diplodus.annularis     0.02311833        0.08714881

# Groupers
vis_temp_pred_pair("Epinephelus.costae", "Serranus.cabrilla", grps_mat, grps_pois, groupers) %>%
  ggsave(filename = "figures/predictions/final/E_costae-S_cabrilla--TEMP.png", device = "png")
# Seabream
vis_temp_pred_pair("Diplodus.annularis", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_annularis-D_vulgaris--TEMP.png", device = "png")
vis_temp_pred_pair("Diplodus.sargus", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_sargus-D_vulgaris--TEMP.png", device = "png")
vis_temp_pred_pair("Diplodus.vulgaris", "Diplodus.sargus", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_vulgaris-D_sargus--TEMP.png", device = "png")
vis_temp_pred_pair("Diplodus.vulgaris", "Diplodus.annularis", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_vulgaris-D_annularis--TEMP.png", device = "png")

## MPA

# y                  ~ cov_biotic                  rel_imp           coef
# -----------------  -----------------------    -------------      -----------
# Epinephelus.costae ~ mpa_Serranus.cabrilla     0.07737350       -0.05396605
# Epinephelus.costae ~ mpa_Epinephelus.marginatus  0.06208940       -0.04834296
# Diplodus.annularis ~ mpa_Diplodus.vulgaris     0.04784076        0.05427835
# Diplodus.puntazzo ~ mpa_Diplodus.vulgaris     0.35485998        0.15129631
# Diplodus.sargus ~ mpa_Diplodus.vulgaris     0.20124590         0.2634552
# Diplodus.vulgaris ~ mpa_Diplodus.sargus     0.21127450        0.26345518
# Diplodus.vulgaris ~ mpa_Diplodus.puntazzo     0.06967714        0.15129631

# Groupers
vis_mpa_pred_pair("Epinephelus.costae", "Serranus.cabrilla", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/final/E_costae-S_cabrilla--MPA.png", device = "png")
vis_mpa_pred_pair("Epinephelus.costae", "Epinephelus.marginatus", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/final/E_costae-E_marginatus--MPA.png", device = "png")
vis_mpa_pred_pair("Epinephelus.marginatus", "Epinephelus.costae", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/final/E_marginatus-E_costae--MPA.png", device = "png")
# Seabream
vis_mpa_pred_pair("Diplodus.annularis", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_annularis-D_vulgaris--MPA.png", device = "png")
vis_mpa_pred_pair("Diplodus.puntazzo", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_puntazzo-D_vulgaris--MPA.png", device = "png")
vis_mpa_pred_pair("Diplodus.sargus", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_sargus-D_vulgaris--MPA.png", device = "png")
vis_mpa_pred_pair("Diplodus.vulgaris", "Diplodus.sargus", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_vulgaris-D_sargus--MPA.png", device = "png")
vis_mpa_pred_pair("Diplodus.vulgaris", "Diplodus.puntazzo", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/final/D_vulgaris-D_puntazzo--MPA.png", device = "png")

# Raw data with scenarios -------------------------------------------------
lapply(grps_pois$key_coefs, function(x) x %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(dip_pois$key_coefs, function(x) x %>% 
         filter(str_detect(string = Variable, pattern = "_")))

# y                  ~ cov_biotic                  rel_imp           coef
# -----------------  -----------------------    -------------      -----------
# Epinephelus.costae ~ temp_Serranus.cabrilla     0.07217235        0.05212067
# Diplodus.annularis ~ temp_Diplodus.vulgaris     0.12332969        0.08714881
# Diplodus.sargus ~ temp_Diplodus.vulgaris     0.05466831         0.1373128
# Diplodus.vulgaris ~ temp_Diplodus.sargus     0.05159193        0.13018894
# Diplodus.vulgaris ~ temp_Diplodus.annularis     0.02311833        0.08714881

# Epinephelus.costae ~ temp_Serranus.cabrilla
grps_mat %>% 
  mutate(species_j = if_else(Serranus.cabrilla == 0, "Species j Absent", "Species j Present"),
         Temperature = grps_mat$temp * attr(grps_mat$temp, 'scaled:scale') + attr(grps_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Epinephelus.costae+0.1)) +
  geom_point(col = guild_colours$grps) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus costae", subtitle = "Serranus cabrilla") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

# All in one graph:
grps_mat %>% 
  mutate(scenario = if_else(Serranus.cabrilla == 0, "Species j Absent", "Species j Present"),
         Temperature = grps_mat$temp * attr(grps_mat$temp, 'scaled:scale') + attr(grps_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Epinephelus.costae+0.1)) +
  geom_point(aes(colour = scenario), alpha = 0.1) +
  stat_smooth(aes(colour = scenario), method = "gam", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus costae") +
  scale_color_manual(name = "Serranus cabrilla", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/E_costae-S_cabrilla--TEMP_raw.png", 
       device = "png", dpi = 150)

# Diplodus.annularis ~ temp_Diplodus.vulgaris
dip_mat %>% 
  mutate(species_j = if_else(Diplodus.vulgaris == 0, "Species j Absent", "Species j Present"),
         Temperature = dip_mat$temp * attr(dip_mat$temp, 'scaled:scale') + attr(dip_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Diplodus.annularis+0.1)) +
  geom_point(col = guild_colours$dip) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus annularis", subtitle = "Diplodus vulgaris") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

dip_mat %>% 
  mutate(scenario = if_else(Diplodus.vulgaris == 0, "Species j Absent", "Species j Present"),
         Temperature = dip_mat$temp * attr(dip_mat$temp, 'scaled:scale') + attr(dip_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Diplodus.annularis+0.1)) +
  geom_point(aes(colour = scenario), alpha = 0.1) +
  stat_smooth(aes(colour = scenario), method = "gam", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus annularis") +
  scale_color_manual(name = "Diplodus vulgaris", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_annularis-D_vulgaris--TEMP_raw.png", 
       device = "png", dpi = 150)

# Diplodus.sargus ~ temp_Diplodus.vulgaris
dip_mat %>% 
  mutate(species_j = if_else(Diplodus.vulgaris == 0, "Species j Absent", "Species j Present"),
         Temperature = dip_mat$temp * attr(dip_mat$temp, 'scaled:scale') + attr(dip_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Diplodus.sargus+0.1)) +
  geom_point(col = guild_colours$grps) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus sargus", subtitle = "Diplodus vulgaris") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

dip_mat %>% 
  mutate(scenario = if_else(Diplodus.vulgaris == 0, "Species j Absent", "Species j Present"),
         Temperature = dip_mat$temp * attr(dip_mat$temp, 'scaled:scale') + attr(dip_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Diplodus.sargus+0.1)) +
  geom_point(aes(colour = scenario), alpha = 0.1) +
  stat_smooth(aes(colour = scenario), method = "gam", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus sargus") +
  scale_color_manual(name = "Diplodus vulgaris", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_sargus-D_vulgaris--TEMP_raw.png", 
       device = "png", dpi = 150)

# Diplodus.vulgaris ~ temp_Diplodus.sargus
dip_mat %>% 
  mutate(species_j = if_else(Diplodus.sargus == 0, "Species j Absent", "Species j Present"),
         Temperature = dip_mat$temp * attr(dip_mat$temp, 'scaled:scale') + attr(dip_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Diplodus.vulgaris+0.1)) +
  geom_point(col = guild_colours$grps) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus vulgaris", subtitle = "Diplodus sargus") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

dip_mat %>% 
  mutate(scenario = if_else(Diplodus.sargus == 0, "Species j Absent", "Species j Present"),
         Temperature = dip_mat$temp * attr(dip_mat$temp, 'scaled:scale') + attr(dip_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Diplodus.vulgaris+0.1)) +
  geom_point(aes(colour = scenario), alpha = 0.1) +
  stat_smooth(aes(colour = scenario), method = "gam", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus vulgaris") +
  scale_color_manual(name = "Diplodus sargus", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_vulgaris-D_sargus--TEMP_raw.png", 
       device = "png", dpi = 150)

# Diplodus.vulgaris ~ temp_Diplodus.annularis
dip_mat %>% 
  mutate(species_j = if_else(Diplodus.annularis == 0, "Species j Absent", "Species j Present"),
         Temperature = dip_mat$temp * attr(dip_mat$temp, 'scaled:scale') + attr(dip_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Diplodus.vulgaris+0.1)) +
  geom_point(col = guild_colours$grps) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus vulgaris", subtitle = "Diplodus annularis") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

dip_mat %>% 
  mutate(scenario = if_else(Diplodus.annularis == 0, "Species j Absent", "Species j Present"),
         Temperature = dip_mat$temp * attr(dip_mat$temp, 'scaled:scale') + attr(dip_mat$temp, 'scaled:center')) %>% 
  ggplot() +
  aes(x = Temperature, y = log2(Diplodus.vulgaris+0.1)) +
  geom_point(aes(colour = scenario), alpha = 0.1) +
  stat_smooth(aes(colour = scenario), method = "gam", alpha = 0.3) +
  xlab("Temperature (°C)") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus vulgaris") +
  scale_color_manual(name = "Diplodus annularis", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_vulgaris-D_annularis--TEMP_raw.png", 
       device = "png", dpi = 150)


# y                  ~ cov_biotic                  rel_imp           coef
# -----------------  -----------------------    -------------      -----------
# Epinephelus.costae ~ mpa_Serranus.cabrilla      0.07737350      -0.05396605
# Epinephelus.costae ~ mpa_Epinephelus.marginatus 0.06208940      -0.04834296
# Epinephelus.marginatus ~ mpa_Epinephelus.costae	0.05148053	    -0.04834296
# Diplodus.annularis ~ mpa_Diplodus.vulgaris      0.04784076       0.05427835
# Diplodus.puntazzo ~ mpa_Diplodus.vulgaris       0.35485998       0.15129631
# Diplodus.sargus ~ mpa_Diplodus.vulgaris         0.20124590       0.2634552
# Diplodus.vulgaris ~ mpa_Diplodus.sargus         0.21127450       0.26345518
# Diplodus.vulgaris ~ mpa_Diplodus.puntazzo       0.06967714       0.15129631

# Epinephelus.costae ~ mpa_Serranus.cabrilla

grps_mat %>% 
  mutate(species_j = if_else(Serranus.cabrilla == 0, "Species j Absent", "Species j Present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Epinephelus.costae+0.1)) +
  geom_boxplot(col = guild_colours$grps, fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus costae", subtitle = "Serranus cabrilla") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

grps_mat %>% 
  mutate(scenario = if_else(Serranus.cabrilla == 0, "absent", "present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Epinephelus.costae+0.1)) +
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus costae") +
  scale_color_manual(name = "Serranus cabrilla", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/E_costae-S_cabrilla--MPA_raw.png", device = "png", dpi = 150)

# Epinephelus.costae ~ mpa_Epinephelus.marginatus
grps_mat %>% 
  mutate(species_j = if_else(Epinephelus.marginatus == 0, "Species j Absent", "Species j Present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Epinephelus.costae+0.1)) +
  geom_boxplot(col = guild_colours$grps, fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus costae", subtitle = "Epinephelus marginatus") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

grps_mat %>% 
  mutate(scenario = if_else(Epinephelus.marginatus == 0, "absent", "present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Epinephelus.costae+0.1)) +
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus costae") +
  scale_color_manual(name = "Epinephelus marginatus", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/E_costae-Emarginatus--MPA_raw.png", device = "png", dpi = 150)

# Epinephelus.marginatus ~ mpa_Epinephelus.costae
grps_mat %>% 
  mutate(species_j = if_else(Epinephelus.costae == 0, "Species j Absent", "Species j Present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Epinephelus.marginatus+0.1)) +
  geom_boxplot(col = guild_colours$grps, fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus marginatus", subtitle = "Epinephelus costae") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

grps_mat %>% 
  mutate(scenario = if_else(Epinephelus.costae == 0, "absent", "present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Epinephelus.marginatus+0.1)) +
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus marginatus") +
  scale_color_manual(name = "Epinephelus costae", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/Emarginatus-E_costae--MPA_raw.png", device = "png", dpi = 150)

# Diplodus.annularis ~ mpa_Diplodus.vulgaris
dip_mat %>% 
  mutate(species_j = if_else(Diplodus.vulgaris == 0, "Species j Absent", "Species j Present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.annularis+0.1)) +
  geom_boxplot(col = guild_colours$grps, fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus annularis", subtitle = "Diplodus vulgaris") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

dip_mat %>% 
  mutate(scenario = if_else(Diplodus.vulgaris == 0, "absent", "present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.annularis+0.1)) +
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus annularis") +
  scale_color_manual(name = "Diplodus vulgaris", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_annularis-D_vulgaris--MPA_raw.png", device = "png", dpi = 150)

# Diplodus.puntazzo ~ mpa_Diplodus.vulgaris
dip_mat %>% 
  mutate(scenario = if_else(Diplodus.vulgaris == 0, "absent", "present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.puntazzo+0.1)) +
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus puntazzo") +
  scale_color_manual(name = "Diplodus vulgaris", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_puntazzo-D_vulgaris--MPA_raw.png", device = "png", dpi = 150)

# Diplodus.sargus ~ mpa_Diplodus.vulgaris
dip_mat %>% 
  mutate(species_j = if_else(Diplodus.vulgaris == 0, "Species j Absent", "Species j Present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.sargus+0.1)) +
  geom_boxplot(col = guild_colours$grps, fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus sargus", subtitle = "Diplodus vulgaris") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

dip_mat %>% 
  mutate(scenario = if_else(Diplodus.vulgaris == 0, "absent", "present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.sargus+0.1)) +
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus sargus") +
  scale_color_manual(name = "Diplodus vulgaris", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_sargus-D_vulgaris--MPA_raw.png", device = "png", dpi = 150)

# Diplodus.vulgaris ~ mpa_Diplodus.sargus
dip_mat %>% 
  mutate(species_j = if_else(Diplodus.sargus == 0, "Species j Absent", "Species j Present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.vulgaris+0.1)) +
  geom_boxplot(col = guild_colours$grps, fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus vulgaris", subtitle = "Diplodus sargus") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

dip_mat %>% 
  mutate(scenario = if_else(Diplodus.sargus == 0, "absent", "present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.vulgaris+0.1)) +
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus vulgaris") +
  scale_color_manual(name = "Diplodus sargus", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_vulgaris-D_sargus--MPA_raw.png", device = "png", dpi = 150)

# Diplodus.vulgaris ~ mpa_Diplodus.puntazzo
dip_mat %>% 
  mutate(species_j = if_else(Diplodus.puntazzo == 0, "Species j Absent", "Species j Present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.vulgaris+0.1)) +
  geom_boxplot(col = guild_colours$grps, fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus vulgaris", subtitle = "Diplodus puntazzo") +
  facet_wrap(~species_j, ncol = 2) + 
  theme(plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"))

dip_mat %>% 
  mutate(scenario = if_else(Diplodus.puntazzo == 0, "absent", "present")) %>% 
  ggplot() +
  aes(x = mpa, y = log2(Diplodus.vulgaris+0.1)) +
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Diplodus vulgaris") +
  scale_color_manual(name = "Diplodus puntazzo", 
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))
ggsave(filename = "figures/predictions/final/D_vulgaris-D_puntazzo--MPA_raw.png", device = "png", dpi = 150)

# Check all files exported:
list.files("figures/predictions/final/", "TEMP.png") %>% length()
list.files("figures/predictions/final/", "TEMP_raw.png") %>% length()
list.files("figures/predictions/final/", "MPA.png") %>% length()
list.files("figures/predictions/final/", "MPA_raw.png") %>% length()


# Coefficients ------------------------------------------------------------

grps_coefs <- coefs_sum(grps_pois) %>% 
  pivot_longer(cols = 3:8) %>% 
  mutate(coefficient_type = str_remove(.$name, "_coef")) %>% 
  na.omit(value) %>%
  select(species, coefficient_type, value, direction) %>% 
  arrange(coefficient_type, species, direction) 
grps_coefs %>% 
  print(n = Inf)

dip_coefs <- coefs_sum(dip_pois) %>% 
  pivot_longer(cols = 3:8) %>% 
  mutate(coefficient_type = str_remove(.$name, "_coef")) %>% 
  na.omit(value) %>%
  select(species, coefficient_type, value, direction) %>% 
  arrange(coefficient_type, species, direction) 
dip_coefs %>% 
  print(n = Inf)


# Compare spatial / Nonspatial results ------------------------------------

grps_pois_relimp
grps_pois_spat_relimp

dip_pois_relimp
dip_pois_spat_relimp


# Overall networks --------------------------------------------------------

library(ggraph)

plot_graph <- function(guild_mod, plot_title){
  net_cols <- c(neg = '#FF3333', pos = '#3399CC')
  net <- igraph::graph.adjacency(guild_mod$graph, weighted = T, mode = "undirected")
  weights <- igraph::E(net)$weight
  deg <- igraph::degree(net, mode = "all")
  ggraph(net, layout = "circle") + 
    geom_edge_link(aes(width = weights, color = weights < 0), lineend = "round", linejoin = "round") +
    scale_edge_width(range = c(0, 3)) +
    scale_edge_color_manual(values = c(net_cols[["pos"]], net_cols[["neg"]])) +
    geom_node_point(aes(size = deg), col = "grey", alpha = 0.5) +
    geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE, 
                   point.padding = unit(0.2, "lines"), fontface = "italic") +
    theme(legend.position = "none",
          aspect.ratio = 1,
          panel.background = element_blank())
}

plot_graph(grps_pois)
ggsave("figures/networks/groupers_network.png", device = "png", 
       dpi = 300, width = 4, unit = "in")

plot_graph(dip_pois)
ggsave("figures/networks/diplodus_network.png", device = "png", 
       dpi = 300, width = 4, unit = "in")
