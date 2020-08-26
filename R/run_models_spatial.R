source("R/packages.R")
load("data/all_objects.RData")
source("R/functions.R")
# source("R/run_models.R")

med_coords <- med_clean %>% 
  distinct(site, trans, lat, lon) %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc") %>% 
  select(lon, lat)


grps_coords <- med_coords %>% 
  filter(rownames(med_coords) %in% rownames(grps_mat))
nrow(grps_mat) == nrow(grps_coords)
grps_spat <- MRFcov_spatial(grps_mat, n_nodes = 5, n_covariates = 4, coords = grps_coords,
                            family = "poisson", n_cores = 8)
# grpsHM_pois <- plotMRF_hm(grps_spat, main = "poisson")
# grpsHM_gaus <- plotMRF_hm(grps_mod, main = "gaussian (unscaled)")
# # When scaled we loose a lot of the associations, only association left is positive E.marg-E.cost
# gridExtra::grid.arrange(grpsHM_gaus, grpsHM_pois, nrow = 1, top = "Groupers co-occurrence")

dip_coords <- med_coords %>% 
  filter(rownames(med_coords) %in% rownames(dip_mat))
nrow(dip_mat) == nrow(dip_coords)
dip_spat <- MRFcov_spatial(dip_mat, n_nodes = 5, n_covariates = 4, coords = dip_coords,
                           family = "poisson", n_cores = 8)
# dipHM_pois <- plotMRF_hm(dip_spat, main = "poisson")
# dipHM_gaus <- plotMRF_hm(dip_mod, main = "gaussian (unscaled)")
# gridExtra::grid.arrange(dipHM_gaus, dipHM_pois, nrow = 1, top = "Seabream co-occurrence")

herb_coords <- med_coords %>% 
  filter(rownames(med_coords) %in% rownames(herb_mat))
nrow(herb_mat) == nrow(herb_coords)
herb_spat <- MRFcov_spatial(herb_mat, n_nodes = 5, n_covariates = 4, coords = herb_coords,
                            family = "poisson", n_cores = 8)
# herbHM_pois <- plotMRF_hm(herb_spat, main = "poisson")
# herbHM_gaus <- plotMRF_hm(herb_mod, main = "gaussian (unscaled)")
# gridExtra::grid.arrange(herbHM_gaus, herbHM_pois, nrow = 1, top = "Herbivores co-occurrence")
