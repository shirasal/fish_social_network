# source("R/functions.R")
# source("R/packages.R")
# load("data/all_objects.RData")
# source("R/run_models_spatial.R")

# Groupers ----------------------------------------------------------------

grps_mat_pa <- grps_mat
grps_mat_pa[,1:5] <- ifelse(grps_mat_pa[,1:5] > 0, 1, 0)

grps_mod_pa <- MRFcov_spatial(data = grps_mat_pa, n_nodes = length(groupers), n_covariates = 4, 
                              coords = grps_coords, family = "binomial")

# Seabream ----------------------------------------------------------------

dip_mat_pa <- dip_mat
dip_mat_pa[,1:5] <- ifelse(dip_mat_pa[,1:5] > 0, 1, 0)

dip_mod_pa <- MRFcov_spatial(data = dip_mat_pa, n_nodes = length(diplodus), n_covariates = 4, 
                             coords = dip_coords, family = "binomial")

# Herbivores --------------------------------------------------------------

herb_mat_pa <- herb_mat
herb_mat_pa[,1:5] <- ifelse(herb_mat_pa[,1:5] > 0, 1, 0)

herb_mod_pa <- MRFcov_spatial(data = herb_mat_pa, n_nodes = length(herbivores), n_covariates = 4, 
                              coords = herb_coords, family = "binomial")
