library(tidyverse)
library(MRFcov)
load("data/all_objects.RData")
source("R/functions.R")

# Create matrix of UNSCALED data and run model with family = 'poisson'
med_data <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE)) %>% 
  rename(temp = tmean, depth = depth, prod = pp_mean) %>% 
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)
spp_mat <- create_spp_mat(dataset = med_data, taxa = groupers, covariate = c(as.array(env_vector), as.array(anthro_vector)))
pois <- MRFcov(spp_mat, n_nodes = 5, n_covariates = 4, family = "poisson")

#--------------------------------------------

# Create matrix of UNSCALED observations data with standaradised covariates
# Run model with raw data and family 'poisson' with scaled covariates
med_cov_scaled <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE)) %>% 
  mutate(temp = tmean,
         depth = depth,
         prod = pp_mean) %>% 
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)
spp_mat_covstd <- create_spp_mat(dataset = med_cov_scaled, taxa = groupers,
                                 covariate = c(as.array(env_vector), as.array(anthro_vector))) %>% 
  mutate_at(.vars = c("temp", "depth", "prod"), .funs = scale)
pois_covstd <- MRFcov(spp_mat_covstd, n_nodes = 5, n_covariates = 4, family = "poisson")

#--------------------------------------------

# Create matrix of UNSCALED observations data with transformed covariates (nonparanormal transformation)
nonparanormal <- function(x){log2(x + 0.1)}

med_npn <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE)) %>% 
  mutate(temp = tmean,
         depth = depth,
         prod = pp_mean) %>% 
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)
spp_mat_covstd <- create_spp_mat(dataset = med_npn, taxa = groupers,
                                 covariate = c(as.array(env_vector), as.array(anthro_vector))) %>% 
  mutate_at(.vars = c("temp", "depth", "prod"), .funs = nonparanormal)
pois_npn <- MRFcov(spp_mat_covstd, n_nodes = 5, n_covariates = 4, family = "poisson")

p_pois <- plotMRF_hm(pois, main = "Poisson (no transformations)")
p_npn <- plotMRF_hm(pois_npn, main = "Poisson with transformed covariates")
p_poiscov <- plotMRF_hm(pois_covstd, main = "Poisson with standardised covariates")

gridExtra::grid.arrange(p_npn, p_pois, p_poiscov, nrow = 1)

rel_imp_sum(pois)
rel_imp_sum(pois_covstd)
rel_imp_sum(pois_npn)

# Poisson vs. Gaussian on transformed -------------------------------------

# Create matrix of scaled covariates and abundances to run with 'gaussian':
med_scaled <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         prod = scale(pp_mean),
         sp.n = scale(sp.n)) %>%
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)
spp_mat_scaled <- create_spp_mat(dataset = med_scaled, taxa = groupers, covariate = c(as.array(env_vector), as.array(anthro_vector)))

# Run model on standardised data
gaus <- MRFcov(spp_mat_scaled, n_nodes = 5, n_covariates = 4, family = "gaussian")

# Create matrix of UNSCALED data
med_data <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE)) %>% 
  rename(temp = tmean, depth = depth, prod = pp_mean) %>% 
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)
spp_mat <- create_spp_mat(dataset = med_data, taxa = groupers, covariate = c(as.array(env_vector), as.array(anthro_vector)))

# Run model with raw data and family 'poisson'
pois <- MRFcov(spp_mat, n_nodes = 5, n_covariates = 4, family = "poisson")

p_gaus <- plotMRF_hm(gaus, main = "Gaussian with standardised data")
p_pois <- plotMRF_hm(pois, main = "Poisson")

gridExtra::grid.arrange(p_gaus, p_pois, nrow = 1)


