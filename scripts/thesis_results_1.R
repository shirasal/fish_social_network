
source("scripts/pckgs_preps.R")

# Add medata --------------------------------------------------------------

med_raw <- read_rds("data/medata.Rds")
# TODO fix me
med_raw %<>% filter(!(site %in% c("assecret2210191mlsc_a", "assecret2210191mlsc_b", "assecret2210191mlsc_c")))

med_clean <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         country = as.factor(country),
         temp = scale(tmean),
         depth = scale(depth),
         # enforce = as.factor(enforcement),
         sal = scale(sal_mean),
         prod = scale(pp_mean)) %>%
  select(site, lon, lat, trans, species, sp.n, country, mpa, temp, depth, sal, prod)
  

med_clean %>% colnames

# Functions ---------------------------------------------------------------

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

#*** *** ***#

# Func 2: Count the number of associations per species in taxa
assoc_count <- function(taxa){
  sapply(taxa$key_coefs, FUN = count) %>% # returns a list
    unlist() %>%
    as_tibble(rownames = "species") %>% 
    mutate(species = str_sub(string = species, end = -3)) %>% # the count function returns species names with a suffix, this line removes the suffix
    select(species, associations = value) # rename variables
}

#*** *** ***#

# Func 3: Count the number of associations per species in taxa, by covariate type
covar_count <- function(taxa){
  env_effect <- sapply(taxa$key_coefs, FUN = function(x) x %>% filter(Variable %in% env_vector) %>%
                         count()) %>% 
    unlist() %>%
    as_tibble(rownames = "species") %>% 
    mutate(species = str_sub(string = species, end = -3)) %>%
    select(species, env_assoc = value)
  
  anthro_effect <- sapply(taxa$key_coefs, FUN = function(x) x %>% filter(Variable %in% anthro_vector) %>%
                            count()) %>% 
    unlist() %>%
    as_tibble(rownames = "species") %>% 
    mutate(species = str_sub(string = species, end = -3)) %>%
    select(species, anthro_assoc = value)
  
  biotic_effect <- sapply(taxa$key_coefs, FUN = function(x) x %>%
                            filter(!(Variable %in% env_vector | Variable %in% anthro_vector | str_detect(string = Variable, pattern = "_"))) %>%
                            count()) %>%
    unlist() %>%
    as_tibble(rownames = "species") %>% 
    mutate(species = str_sub(string = species, end = -3)) %>%
    select(species, biotic_assoc = value)
  
  inter_effect <- sapply(taxa$key_coefs, FUN = function(x) x %>%
                           filter(str_detect(string = Variable, pattern = "_")) %>%
                           count()) %>%
    unlist() %>%
    as_tibble(rownames = "species") %>% 
    mutate(species = str_sub(string = species, end = -3)) %>%
    select(species, inter_assoc = value)
  
  env_effect %>%
    left_join(anthro_effect, by = "species") %>%
    left_join(biotic_effect, by = "species") %>%
    left_join(inter_effect, by = "species")
}

#*** *** ***#

# Func 4: Summarise the relative importance of each type of covariate for each species
rel_imp_sum <- function(taxa){
  env_relimp <- sapply(taxa$key_coefs, FUN = function(x) x %>%  # Take the taxa model and apply the following:
                         filter(Variable %in% env_vector) %>%  # Filter by relevant covariates
                         summarise(n = sum(Rel_importance))) %>% # Summarise Rel_importance column
    unlist() %>% # Take out of the list
    enframe(name = "species", value = "env_rel_imp") %>%  # Rearrange
    mutate(species = str_sub(string = species, end = -3)) # Fix species names
  
  anthro_relimp <- sapply(taxa$key_coefs, FUN = function(x) x %>% 
                            filter(Variable %in% anthro_vector) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "anthro_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  biotic_relimp <- sapply(taxa$key_coefs, FUN = function(x) x %>% 
                            filter(!(Variable %in% env_vector | Variable %in% anthro_vector | str_detect(string = Variable, pattern = "_"))) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "biotic_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  inter_relimp <- sapply(taxa$key_coefs, FUN = function(x) x %>% 
                           filter(str_detect(string = Variable, pattern = "_")) %>%
                           summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "inter_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  
  env_relimp %>%
    left_join(anthro_relimp, by = "species") %>%
    left_join(biotic_relimp, by = "species") %>%
    left_join(inter_relimp, by = "species")
}


# Create species matrix for each taxa -------------------------------------

# Create species matrix to run the model on (using FUNC 1)
# This matrix should include all species from the taxa I'm interested in
# and the covariates I'd like to include in the model (these are pre-determined in FUNC 1)

## GROUPERS
grps_mat <- create_spp_mat(dataset = med_clean, taxa = groupers, covariate = c("temp", "depth", "mpa"))
grps_mat %>% View()

## SEABREAM (Diplodus species)
dip_mat <- create_spp_mat(dataset = med_clean, taxa = diplodus, covariate = c("temp", "depth", "mpa"))
dip_mat %>% View()

## HERBIVORES
herb_mat <- create_spp_mat(dataset = med_clean, taxa = herbivores, covariate = c("temp", "depth", "mpa"))
herb_mat %>% View()


############## List of figures (dead line 15/7 ####################################

# Table 1: relative importance for each species:
# how many associations and how many of them are an effect of covariate?
# 
# Figure 1: Co-occurrence with/without covariates
# 
# Figure 2: Relative importance aggregation per species: env, spp, interaction (spp*cov)-stationarity
# try diffetrent combinations (env*spp, mpa*spp, cov*spp)
# for each taxa (3 bar graphs)
#
# Write all results and think of a take home message.
# Is species response the same for varying covariates?

##################################################################################

# Figure 1. Co-occurrence of species with and without covariates ----------

# GROUPERS
grps_mod <- MRFcov(data = grps_mat, n_nodes = length(groupers), n_covariates = 3, family = "gaussian")
grpsHM_cov <- plotMRF_hm(grps_mod, main = "with covariates")

grps_mod$indirect_coefs$temp
grps_mod$indirect_coefs$depth
grps_mod$indirect_coefs$mpa

grps_mod_nocov <- MRFcov(data = grps_mat, n_nodes = length(groupers), n_covariates = 3, family = "gaussian")
grpsHM_nocov <- plotMRF_hm(grps_mod_nocov, main = "without covariates")

gridExtra::grid.arrange(grpsHM_cov, grpsHM_nocov, nrow = 1, top = "Groupers co-occurrence")


# SEABREAM
dip_mod <- MRFcov(data = dip_mat, n_nodes = length(diplodus), n_covariates = 3, family = "gaussian")
dipHM_cov <- plotMRF_hm(dip_mod, main = "with covariates")

dip_mod$indirect_coefs$temp
dip_mod$indirect_coefs$depth
dip_mod$indirect_coefs$mpa

dip_mod_nocov <- MRFcov(data = dip_mat, n_nodes = length(diplodus), n_covariates = 3, family = "gaussian")
dipHM_nocov <- plotMRF_hm(dip_mod_nocov, main = "without covariates")

gridExtra::grid.arrange(dipHM_cov, dipHM_nocov, nrow = 1, top = "Seabream co-occurrence")


# HERBIVORES
herb_mod <- MRFcov(data = herb_mat, n_nodes = length(herbivores), n_covariates = 3, family = "gaussian")
herbHM_cov <- plotMRF_hm(herb_mod, main = "with covariates")

herb_mod$indirect_coefs$temp
herb_mod$indirect_coefs$depth
herb_mod$indirect_coefs$mpa

herb_mod_nocov <- MRFcov(data = herb_mat, n_nodes = length(herbivores), n_covariates = 3, family = "gaussian")
herbHM_nocov <- plotMRF_hm(herb_mod_nocov, main = "without covariates")

gridExtra::grid.arrange(herbHM_cov, herbHM_nocov, nrow = 1, top = "Herbivores co-occurrence")


# Table 1. Relative importance per species --------------------------------
# How many associations a species has? -> for each species, count coefficients != 0

grps_assoc <- assoc_count(grps_mod)
dip_assoc <- assoc_count(dip_mod)
herb_assoc <- assoc_count(herb_mod)
# TODO Calling `as_tibble()` on a vector is discouraged, because the behavior is likely to change in the future. Use `tibble::enframe(name = NULL)` instead.

all_assoc <- list(groupers = grps_assoc,
                  seabream = dip_assoc,
                  herbivores = herb_assoc)

# How many of these associations are a result of env./mpa? -> for each coef, determine biotic/abiotic
grps_cov_assoc <- covar_count(grps_mod)
dip_cov_assoc <- covar_count(dip_mod)
herb_cov_assoc <- covar_count(herb_mod)

all_cov_assoc <- list(groupers = grps_cov_assoc,
                      seabrean = dip_cov_assoc,
                      herbivores = herb_cov_assoc)

# Figure 2. Relative importance per taxa ----------------------------------

grps_relimp <- rel_imp_sum(grps_mod)
dip_relimp <- rel_imp_sum(dip_mod)
herb_relimp <- rel_imp_sum(herb_mod)

all_relimp <- list(groupers = grps_relimp,
                   seabream = dip_relimp,
                   herbivores = herb_relimp)

# Table of mean relative importance of covariates per taxa:
relimp_table <- sapply(X = all_relimp, FUN = function(x) x[,-1] %>% colMeans())

# Figure 2:
all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:6) %>% # Create a tibble of all taxa
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = taxa)+
  stat_summary(geom = "bar", fun = mean, position = "dodge")+
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge")

