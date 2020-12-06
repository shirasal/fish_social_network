# source("R/run_models.R")
# source("R/run_models_spatial.R")

rm(list = c("grps_mod_nocov", "dip_mod_nocov", "herb_mod_nocov", 
            "grpsHM_nocov", "dipHM_nocov", "herbHM_nocov"))

grps_temp <- create_spp_mat(med_clean, groupers, "temp")
grps_mod_temp <- MRFcov_spatial(grps_temp, n_nodes = 5, n_covariates = 1, coords = grps_coords,
                                family = "poisson", n_cores = 8)
plotMRF_hm(grps_mod_temp)
plotMRF_hm(grps_spat)

dip_temp <- create_spp_mat(med_clean, diplodus, "temp")
dip_mod_temp <- MRFcov_spatial(dip_temp, n_nodes = 5, n_covariates = 1, coords = dip_coords,
                                family = "poisson", n_cores = 8)
plotMRF_hm(dip_mod_temp)
plotMRF_hm(dip_spat)

herb_temp <- create_spp_mat(med_clean, herbivores, "temp")
herb_mod_temp <- MRFcov_spatial(herb_temp, n_nodes = 5, n_covariates = 1, coords = herb_coords,
                                family = "poisson", n_cores = 8)
plotMRF_hm(herb_mod_temp)
plotMRF_hm(herb_spat)
