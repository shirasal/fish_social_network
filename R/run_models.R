source("R/packages.R")
load("data/all_objects.RData")
source("R/functions.R")

# Run all models with and without covariates

# Groupers ----------------------------------------------------------------

grps_mod <- MRFcov(data = grps_mat, n_nodes = length(groupers), n_covariates = 5, family = "gaussian")
grps_mod_nocov <- MRFcov(data = grps_mat[1:length(groupers)], n_nodes = length(groupers), family = "gaussian")

# Seabream ----------------------------------------------------------------

dip_mod <- MRFcov(data = dip_mat, n_nodes = length(diplodus), n_covariates = 5, family = "gaussian")
dip_mod_nocov <- MRFcov(data = dip_mat[1:length(diplodus)], n_nodes = length(diplodus), family = "gaussian")

# Herbivores --------------------------------------------------------------

herb_mod <- MRFcov(data = herb_mat, n_nodes = length(herbivores), n_covariates = 5, family = "gaussian")
herb_mod_nocov <- MRFcov(data = herb_mat[1:length(herbivores)], n_nodes = length(herbivores), family = "gaussian")

# Take a look at the output of the models:
grps_mod
grps_mod$graph # Only association parameters of species
grps_mod$intercepts
grps_mod$direct_coefs # association parameters of species and covariates
grps_mod$direct_coefs[1,] %>% View

