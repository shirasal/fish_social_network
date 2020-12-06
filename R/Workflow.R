library(magrittr)
library(tidyverse)
library(MRFcov)


# Guilds ------------------------------------------------------------------
# The following code runs everything on the diplodus (seabreams) guild, 
# but everything can be done on any other species group. The following guilds are the ones I'm using:

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa", "Sparisoma.cretense")

# Required data -----------------------------------------------------------
med_raw <- read_rds("data/medata.Rds") %>% ungroup()

med_clean <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence data
  mutate(mpa = case_when(enforcement == 1 | enforcement == 0 ~ FALSE,
                         enforcement == 2 | enforcement == 3 ~ TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         sal = scale(sal_mean),
         prod = scale(pp_mean)) %>%
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)

# Step 1: Create species matrix -------------------------------------------
# Note that the following function is set to be running on `dataset = med_clean`
create_spp_mat <- function(dataset, guild, covariate){
  cols <- c("lat", "lon", "site", "trans", "species", "temp", "depth", "prod", "mpa")
  dataset %>%
    group_by_at(.vars = cols) %>%
    summarise(n = sum(sp.n), .groups = "drop") %>%
    spread(species, n, fill = 0) %>%
    na.omit() %>% # remove NAs; see 'issues' directory for more information on ommited observations
    mutate(loc = paste(site, trans)) %>%
    group_by(loc) %>%
    column_to_rownames("loc") %>% 
    select(all_of(guild), all_of(covariate)) %>% 
    ungroup()
}

dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, covariate = c("mpa", "temp", "depth", "prod"))
head(dip_mat)

# Step 2: run models ------------------------------------------------------

dip_mod <- MRFcov(data = dip_mat, n_nodes = length(diplodus), n_covariates = 4, family = "poisson")

# Step 3: Summarise relative importance -----------------------------------

env_vector <- c("temp", "depth", "prod") # Salinity (sal) removed to avoid data loss (NAs)
anthro_vector <- c("mpa")

rel_imp_sum <- function(taxa_mod){
  env_relimp <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>%  # Take the taxa model and apply the following:
                         filter(Variable %in% env_vector) %>%  # Filter by relevant covariates
                         summarise(n = sum(Rel_importance))) %>% # Summarise Rel_importance column
    unlist() %>% # Take out of the list
    enframe(name = "species", value = "env_rel_imp") %>%  # Rearrange
    mutate(species = str_sub(string = species, end = -3)) # Fix species names
  
  anthro_relimp <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>% 
                            filter(Variable %in% anthro_vector) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "anthro_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  biotic_relimp <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>% 
                            filter(!(Variable %in% env_vector | Variable %in% anthro_vector | str_detect(string = Variable, pattern = "_"))) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "biotic_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_bio_relimp <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>% 
                             filter(str_detect(string = Variable, pattern = "temp_")) %>% 
                             summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "env_bio_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  anthro_bio_relimp <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>% 
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

dip_relimp <- rel_imp_sum(dip_mod)
dip_relimp %>% pivot_longer(2:length(.)) %>% 
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
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = "#d69c4e") +
  facet_wrap(~facet.title, nrow = 1) +
  labs(subtitle = "Seabreams") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))


# Step 4: Visualising predictions for species pairs -----------------------

# First look for species pairs that show nonstationarity
lapply(dip_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

# Then visualise the interaction effect

# -- MPA * biotic

vis_mpa_pred_pair <- function(species_i, species_j, spp_mat, spp_mod, guild, n_spp){
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
    sample_n(1)
  # Scenario 2: species_j is at its max abundance, other species are at their mean abundance
  j_max <- spp_mat %>% 
    mutate(temp = median(temp),
           depth = median(depth),
           prod = median(prod),
           across(.cols = all_of(species_j), .fns = max),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(mpa) %>% 
    sample_n(1)
  
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
    pivot_longer(cols = all_of(2:(1+n_spp)),
                 names_to = "species",
                 values_to = "prediction", 
                 names_repair = "minimal")
  
  ## Plot the predictions:
  predictions_mpa %>%
    filter(species == species_i) %>% 
    ggplot() +
    aes(x = mpa, y = prediction, fill = scenario) +
    stat_summary(geom = "bar", fun = "mean", position = "dodge") +
    # stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.8), width = 0.2) +
    xlab("MPA") + ylab("Prediction") +
    labs(title = "Observation predictions",
         subtitle = stringr::str_replace(species_i, "\\.", "\\ "),
         fill = stringr::str_replace(species_j, "\\.", "\\ ")) +
    scale_fill_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
    theme(legend.title = element_text(face = "italic"), plot.subtitle = element_text(face = "italic"))
}

# -- Temperature * Biotic

vis_temp_pred_pair <- function(species_i, species_j, spp_mat, spp_mod, guild, n_spp){
  # Create a vector of all other species except species_j
  all_other_species <- guild[-which(guild == species_j)]
  
  # Scenario 1: species_j is absent, other species are at their mean abundance
  j_abs <- spp_mat %>% 
    mutate(depth = median(depth),
           prod = median(prod),
           mpa = TRUE,
           across(.cols = all_of(species_j), .fns = function(x) 0),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(temp = round(temp, digits = 1)) %>% 
    sample_n(1)
  # Scenario 2: species_j is at its max abundance, other species are at their mean abundance
  j_max <- spp_mat %>% 
    mutate(depth = median(depth),
           prod = median(prod),
           mpa = TRUE,
           across(.cols = all_of(species_j), .fns = max),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(temp = round(temp, digits = 1)) %>% 
    sample_n(1)
  
  # Create predictions
  ## For when species j is absent
  predict_abs <- predict_MRF(j_abs, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(temp = j_abs$temp)
  ## For when species j is absent
  predict_max <- predict_MRF(j_max, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(temp = j_max$temp)
  
  # Put the two scenarios together
  temp_predict <- bind_rows(predict_abs, predict_max, .id = "scenario") %>% 
    mutate(scenario = case_when(scenario == 1 ~ "absent",
                                scenario == 2 ~ "present"))
  
  # Visualise the predictions
  ## Create a dataframe with all the predictions, sorted by scenario
  
  predictions_temp <- temp_predict %>% 
    pivot_longer(cols = all_of(2:(1+n_spp)),
                 names_to = "species",
                 values_to = "prediction", 
                 names_repair = "minimal")
  
  ## Plot the predictions:
  predictions_temp %>%
    filter(species == species_i) %>% 
    ggplot() +
    aes(x = temp, y = prediction, col = scenario) +
    geom_smooth(method = "lm", formula = y ~ x, cex = 3, alpha = 0.1) +
    xlab("Temperature (scaled)") + ylab("Prediction") +
    labs(title = "Observation predictions",
         subtitle = stringr::str_replace(species_i, "\\.", "\\ "),
         col = stringr::str_replace(species_j, "\\.", "\\ ")) +
    scale_color_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
    theme(legend.title = element_text(face = "italic"), plot.subtitle = element_text(face = "italic"))
}

### Diplodus.sargus ~ Diplodus.vulgaris * mpa
vis_mpa_pred_pair(species_i = "Diplodus.sargus",
                  species_j = "Diplodus.vulgaris", 
                  spp_mat = dip_mat, spp_mod = dip_mod, guild = diplodus, n_spp = 5)
