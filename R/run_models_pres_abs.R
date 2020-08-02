
# load("data/all_objects.RData")
source("R/run_models.R") # Similar, only on presence-absence data

# Groupers ----------------------------------------------------------------

grps_mat_pa <- grps_mat
grps_mat_pa[,1:5] <- ifelse(grps_mat_pa[,1:5] > 0, 1, 0)

grps_mod_pa <- MRFcov(data = grps_mat_pa, n_nodes = length(groupers), n_covariates = 5, family = "binomial")
grps_mod_pa_nocov <- MRFcov(data = grps_mat_pa[1:length(groupers)], n_nodes = length(groupers), family = "binomial")
# Leave-one-out cv used for the following low-occurrence (rare) nodes:
# Epinephelus.costae Mycteroperca.rubra ...
grpsHM_cov_pa <- plotMRF_hm(grps_mod_pa, main = "with covariates")
grpsHM_nocov_pa <- plotMRF_hm(grps_mod_pa_nocov, main = "without covariates")
gridExtra::grid.arrange(grpsHM_cov_pa, grpsHM_nocov_pa, nrow = 1, top = "Groupers co-occurrence")

# Seabream ----------------------------------------------------------------

dip_mat_pa <- dip_mat
dip_mat_pa[,1:5] <- ifelse(dip_mat_pa[,1:5] > 0, 1, 0)

dip_mod_pa <- MRFcov(data = dip_mat_pa, n_nodes = length(diplodus), n_covariates = 5, family = "binomial")
dip_mod_pa_nocov <- MRFcov(data = dip_mat_pa[1:length(diplodus)], n_nodes = length(diplodus), family = "binomial")

dipHM_cov_pa <- plotMRF_hm(dip_mod_pa, main = "with covariates")
dipHM_nocov_pa <- plotMRF_hm(dip_mod_pa_nocov, main = "without covariates")
gridExtra::grid.arrange(dipHM_cov_pa, dipHM_nocov_pa, nrow = 1, top = "Seabream co-occurrence")

# Herbivores --------------------------------------------------------------

herb_mat_pa <- herb_mat
herb_mat_pa[,1:5] <- ifelse(herb_mat_pa[,1:5] > 0, 1, 0)

herb_mod_pa <- MRFcov(data = herb_mat_pa, n_nodes = length(herbivores), n_covariates = 5, family = "binomial")
herb_mod_pa_nocov <- MRFcov(data = herb_mat_pa[1:length(herbivores)], n_nodes = length(groupers), family = "binomial")

herbHM_cov_pa <- plotMRF_hm(herb_mod_pa, main = "with covariates")
herbHM_nocov_pa <- plotMRF_hm(herb_mod_pa_nocov, main = "without covariates")
gridExtra::grid.arrange(herbHM_cov_pa, herbHM_nocov_pa, nrow = 1, top = "Herbivores co-occurrence")
