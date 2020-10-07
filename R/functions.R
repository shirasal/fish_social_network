
# Func 1: Create species matrix for a specific taxa with all environmental variables
create_spp_mat <- function(dataset, taxa, covariate){
  cols <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)
  dataset %>%
    group_by_at(.vars = cols) %>% # group for summarise
    summarise(n = sum(sp.n)) %>% # sum sp.n for each grouped variable
    spread(species, n, fill = 0) %>% # convert to species matrix
    ungroup() %>%
    na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
    mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which shpuld be unique
    group_by(loc) %>%
    column_to_rownames("loc") %>% # create row names by location
    select(all_of(taxa), all_of(covariate)) # keep the species and covariates columns
}

##################

# Func 2: Count the number of associations per species in taxa
assoc_count <- function(taxa_mod){
  sapply(taxa_mod$key_coefs, FUN = count) %>% # returns a list
    unlist() %>%
    enframe(name = "species", value = "associations") %>% 
    mutate(species = str_sub(string = species, end = -3)) # the count function returns species names with a suffix, this line removes the suffix
}

##################

# Func 3: Count the number of associations per species in taxa, by covariate type
covar_count <- function(taxa_mod){
  env_effect <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>% filter(Variable %in% env_vector) %>%
                         count()) %>% 
    unlist() %>%
    enframe(name = "species", value = "env_assoc") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  anthro_effect <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>% filter(Variable %in% anthro_vector) %>%
                            count()) %>% 
    unlist() %>%
    enframe(name = "species", value = "anthro_assoc") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  biotic_effect <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>%
                            filter(!(Variable %in% env_vector | Variable %in% anthro_vector | str_detect(string = Variable, pattern = "_"))) %>%
                            count()) %>%
    unlist() %>%
    enframe(name = "species", value = "biotic_assoc") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_bio_effect <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>% 
                             filter(str_detect(string = Variable, pattern = "temp_")) %>% 
                             count()) %>% 
    unlist() %>%
    enframe(name = "species", value = "env_bio_assoc") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  anthro_bio_effect <- sapply(taxa_mod$key_coefs, FUN = function(x) x %>% 
                                filter(str_detect(string = Variable, pattern = "mpa_")) %>% 
                                count()) %>% 
    unlist() %>%
    enframe(name = "species", value = "mpa_bio_assoc") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_effect %>%
    left_join(anthro_effect, by = "species") %>%
    left_join(biotic_effect, by = "species") %>%
    left_join(env_bio_effect, by = "species") %>%
    left_join(anthro_bio_effect, by = "species")
}

##################

# Func 4: Summarise the relative importance of each type of covariate for each species
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

##################

# Func 5: Count the all POSITIVE/NEGATIVE association coefficients, per taxa

std_coefs <- function(taxa_mod){
  env_effect <- lapply(taxa_mod$key_coefs, FUN = function(x) x %>%
                         filter(Variable %in% env_vector) %>%
                         group_by(Standardised_coef > 0) %>% 
                         summarise(env_coefs = sum(Standardised_coef)) %>% 
                         transmute(direction = `Standardised_coef > 0`, env_coefficient = env_coefs) %>%
                         mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                      .$direction == FALSE ~ "neg",
                                                      TRUE ~ as.character(.$direction)))) %>% 
    bind_rows(.id = "id")
  
  anthro_effect <- lapply(taxa_mod$key_coefs, FUN = function(x) x %>%
                            filter(Variable %in% anthro_vector) %>%
                            group_by(Standardised_coef > 0) %>% 
                            summarise(env_coefs = sum(Standardised_coef)) %>% 
                            transmute(direction = `Standardised_coef > 0`, anthro_coefficient = env_coefs) %>%
                            mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                         .$direction == FALSE ~ "neg",
                                                         TRUE ~ as.character(.$direction)))) %>% 
    bind_rows(.id = "id")
  
  biotic_effect <- lapply(taxa_mod$key_coefs, FUN = function(x) x %>%
                            filter(!(Variable %in% env_vector | Variable %in% anthro_vector | str_detect(string = Variable, pattern = "_"))) %>%
                            group_by(Standardised_coef > 0) %>% 
                            summarise(env_coefs = sum(Standardised_coef)) %>% 
                            transmute(direction = `Standardised_coef > 0`, bio_coefficient = env_coefs) %>%
                            mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                         .$direction == FALSE ~ "neg",
                                                         TRUE ~ as.character(.$direction)))) %>% 
    bind_rows(.id = "id")
  
  env_bio_effect <- lapply(taxa_mod$key_coefs, FUN = function(x) x %>%
                             filter(str_detect(string = Variable, pattern = "temp_")) %>%
                             group_by(Standardised_coef > 0) %>% 
                             summarise(env_coefs = sum(Standardised_coef)) %>%
                             transmute(direction = `Standardised_coef > 0`, bio_env_coefficient = env_coefs) %>%
                             mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                          .$direction == FALSE ~ "neg",
                                                          TRUE ~ as.character(.$direction)))) %>%
    bind_rows(.id = "id")
  
  anthro_bio_effect <- lapply(taxa_mod$key_coefs, FUN = function(x) x %>%
                                filter(str_detect(string = Variable, pattern = "mpa_")) %>%
                                group_by(Standardised_coef > 0) %>% 
                                summarise(env_coefs = sum(Standardised_coef)) %>%
                                transmute(direction = `Standardised_coef > 0`, bio_anth_coefficient = env_coefs) %>%
                                mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                             .$direction == FALSE ~ "neg",
                                                             TRUE ~ as.character(.$direction)))) %>%
    bind_rows(.id = "id")
  
  
  env_effect %>%
    left_join(anthro_effect, by = c("id", "direction")) %>%
    left_join(biotic_effect, by = c("id", "direction")) %>% 
    left_join(env_bio_effect, by = c("id", "direction")) %>%
    left_join(anthro_bio_effect, by = c("id", "direction")) %>% 
    arrange(direction)
}

##################

# Func 6: Create coordinates dataframe for spatial model (accounting for spatial autocorrelation)
# Arguments examples:
## species_mat = grps_mat
create_coords_df <- function(species_mat){
  med_clean %>% 
    distinct(site, trans, lat, lon) %>% 
    mutate(loc = paste(site, trans)) %>% 
    column_to_rownames("loc") %>% 
    select(lon, lat) %>% 
    filter(rownames(.) %in% rownames(species_mat))
}

##################

# Func 7: plot relative importance graph for a specific species group 
# Arguments examples:
## species_relimp = grps_relimp, fill_colour = "#eccbae", group_name = "Groupers"

plot_rel_imp <- function(species_relimp, fill_colour, group_name){
  species_relimp %>% pivot_longer(2:length(.)) %>% # Create a tibble of all species
    rename(species = species, covariate = name, rel_imp = value) %>%
    mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
    ggplot() +
    aes(x = species, y = rel_imp) +
    stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = fill_colour) +
    facet_wrap(~covariate, nrow = 2) +
    labs(title = "Relative importance of factors in the model", subtitle = group_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside")
}

##################

# Func 8: create a list of dataframes for prediction
# Arguments examples:
## species_of_interest = "Epinephelus.costae", species_group = groupers

create_pres_abs_df <- function(species_of_interest, species_group){
  absent <- med_clean %>%
    group_by_at(.vars = c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)) %>%
    summarise(n = sum(sp.n)) %>%
    spread(species, n, fill = 0) %>%
    ungroup() %>%
    na.omit() %>%
    mutate(across(.cols = all_of(species_group[-which(species_group==species_of_interest)]),
                  .fns = function(x) 0)) %>% 
    mutate(loc = paste(site, trans)) %>%
    column_to_rownames("loc") %>%
    select(all_of(species_group), all_of(env_vector), all_of(anthro_vector))
  present <- med_clean %>%
    group_by_at(.vars = c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)) %>%
    summarise(n = sum(sp.n)) %>%
    spread(species, n, fill = 0) %>%
    ungroup() %>%
    na.omit() %>%
    mutate(across(.cols = all_of(species_group[-which(species_group==species_of_interest)]),
                  .fns = function(x) 1)) %>% 
    mutate(loc = paste(site, trans)) %>%
    column_to_rownames("loc") %>%
    select(all_of(species_group), all_of(env_vector), all_of(anthro_vector))
  return(list(absent = absent, present = present))
  if (absent == present) 
    warning("Present and Absent dataframes are equal")
}

##################

# Func 9: create a list of dataframes for prediction
# Arguments examples:
## list_of_dfs = output from FUNC 8, spp_coords = grps_coords, species_group = groupers

model_predictions <- function(list_of_dfs, spp_coords, species_group){
  abs_mod <- MRFcov::MRFcov_spatial(list_of_dfs$absent, n_nodes = length(species_group), n_covariates = 4, coords = spp_coords, family = "poisson")
  pres_mod <- MRFcov::MRFcov_spatial(list_of_dfs$present, n_nodes = length(species_group), n_covariates = 4, coords = spp_coords, family = "poisson")
  abs_pred <- MRFcov::predict_MRF(data = list_of_dfs$absent, MRF_mod = abs_mod) %>%
    `colnames<-`(species_group) %>% 
    as.data.frame() %>% 
    rownames_to_column("site") %>% 
    pivot_longer(2:length(.),
                 names_to = "species",
                 values_to = "pred_abs") %>% 
    mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
    left_join(locs, by = "loc") %>% 
    select(loc, tmean, mpa, 2:3)
  pres_pred <- MRFcov::predict_MRF(data = list_of_dfs$present, MRF_mod = pres_mod) %>%
    `colnames<-`(species_group) %>% 
    as.data.frame() %>% 
    rownames_to_column("site") %>% 
    pivot_longer(2:length(.),
                 names_to = "species",
                 values_to = "pred_pres") %>% 
    mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
    left_join(locs, by = "loc") %>% 
    select(loc, tmean, mpa, 2:3)
  
  abs_pred %>% left_join(pres_pred) %>% 
    pivot_longer(cols = pred_abs:pred_pres,
                 names_to = "model",
                 values_to = "prediction")
  
}

##################

# Func 10: plot the predictions
# Arguments examples:
## predictions_long_df = output from FUNC 9, species_of_interest = "Epinephelus.costae"

plot_predictions <- function(predictions_long_df, species_of_interest){
  predictions_long_df %>%
    filter(species == species_of_interest) %>% 
    ggplot() +
    aes(x = temp, y = prediction, color = model) +
    geom_smooth(method = "lm", formula = y ~ x, cex = 3, alpha = 0.1) +
    xlab("Temperature (scaled)") +
    ylab("Predicted observations") +
    labs(subtitle = stringr::str_replace(species_of_interest, "\\.", "\\ "),
         colour = 'All other species') +
    scale_colour_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9"))
}

# or plot for MPA (bar plot)
plot_bar_predictions <- function(predictions_long_df, species_of_interest){
  predictions_long_df %>%
    filter(species == species_of_interest) %>% 
    ggplot() +
    aes(x = mpa, y = prediction, fill = model) +
    stat_summary(geom = "bar", fun = "mean", position = "dodge") +
    # stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.8), width = 0.2) +
    xlab("MPA") + ylab("Predicted observations") +
    labs(title = "Observation predictions",
         subtitle = stringr::str_replace(species_of_interest, "\\.", "\\ "),
         colour = 'All other species') +
    scale_fill_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9"))
}
