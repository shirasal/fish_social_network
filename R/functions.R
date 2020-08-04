
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

#--------------------------------------------------------------------------------------------

# Func 2: Count the number of associations per species in taxa
assoc_count <- function(taxa_mod){
  sapply(taxa_mod$key_coefs, FUN = count) %>% # returns a list
    unlist() %>%
    enframe(name = "species", value = "associations") %>% 
    mutate(species = str_sub(string = species, end = -3)) # the count function returns species names with a suffix, this line removes the suffix
}

#------------------------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------------

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
                             transmute(direction = `Standardised_coef > 0`, int_env_coefficient = env_coefs) %>%
                             mutate(direction = case_when(.$direction == TRUE ~ "pos",
                                                          .$direction == FALSE ~ "neg",
                                                          TRUE ~ as.character(.$direction)))) %>%
    bind_rows(.id = "id")
  
  anthro_bio_effect <- lapply(taxa_mod$key_coefs, FUN = function(x) x %>%
                                filter(str_detect(string = Variable, pattern = "mpa_")) %>%
                                group_by(Standardised_coef > 0) %>% 
                                summarise(env_coefs = sum(Standardised_coef)) %>%
                                transmute(direction = `Standardised_coef > 0`, int_anth_coefficient = env_coefs) %>%
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

