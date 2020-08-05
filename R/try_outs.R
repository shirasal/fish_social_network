med_raw %>% glimpse()

med_raw %>% 
  ggplot() +
  aes(x = lon, y = tmean) + 
  geom_point(col = "darkred")

med_clean %>% 
  ggplot() + 
  aes(x = lon, y = temp) +
  geom_point(col = "darkblue")

test_grps_pois <- MRFcov(grps_mat, n_nodes = 5, n_covariates = 5, family = "poisson")
test_grps_gaus <- MRFcov(grps_mat, n_nodes = 5, n_covariates = 5, family = "gaussian")

plotMRF_hm(test_grps_gaus, main = "Gaussian")
plotMRF_hm(test_grps_pois, main = "Poisson")


grps_mat_pa <- grps_mat
grps_mat_pa[,1:5] <- ifelse(grps_mat_pa[,1:5] > 0, 1, 0)
grps_mod_pa <- MRFcov(data = grps_mat_pa, n_nodes = length(groupers), n_covariates = 5, family = "binomial")
plotMRF_hm(grps_mod_pa, main = "Binomial")
