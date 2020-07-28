
source("scripts/packages.R")
load("data/all_objects.RData")
source("scripts/functions.R")

med_clean %>% colnames
med_clean %>% glimpse()


# Check auto-correlations -------------------------------------------------

# Temperature - Salinity

med_clean %>% ggplot() + 
  aes(x = temp, y = sal) + 
  geom_point()

# Salinity NAs
med_raw %>% dplyr::filter(!complete.cases(sal_mean))
med_raw %>% dplyr::filter(!complete.cases(sal_mean)) %>% group_by(data.origin) %>% summarise(n = n())
med_raw %>% dplyr::filter(!complete.cases(sal_mean) & data.origin == "Belmaker") %>% distinct(site)
med_raw %>% dplyr::filter(!complete.cases(sal_mean) & data.origin == "Sala - PEW") %>% distinct(site)

# Temperature - Invasive species


# Figure 1. Co-occurrence of species with and without covariates ----------

# GROUPERS
grps_mod <- MRFcov(data = grps_mat, n_nodes = length(groupers), n_covariates = 6, family = "gaussian")
grpsHM_cov <- plotMRF_hm(grps_mod, main = "with covariates")

grps_mod_nocov <- MRFcov(data = grps_mat, n_nodes = length(groupers), n_covariates = 6, family = "gaussian")
grpsHM_nocov <- plotMRF_hm(grps_mod_nocov, main = "without covariates")

gridExtra::grid.arrange(grpsHM_cov, grpsHM_nocov, nrow = 1, top = "Groupers co-occurrence")


# SEABREAM
dip_mod <- MRFcov(data = dip_mat, n_nodes = length(diplodus), n_covariates = 6, family = "gaussian")
dipHM_cov <- plotMRF_hm(dip_mod, main = "with covariates")

dip_mod_nocov <- MRFcov(data = dip_mat, n_nodes = length(diplodus), n_covariates = 6, family = "gaussian")
dipHM_nocov <- plotMRF_hm(dip_mod_nocov, main = "without covariates")

gridExtra::grid.arrange(dipHM_cov, dipHM_nocov, nrow = 1, top = "Seabream co-occurrence")


# HERBIVORES
herb_mod <- MRFcov(data = herb_mat, n_nodes = length(herbivores), n_covariates = 6, family = "gaussian")
herbHM_cov <- plotMRF_hm(herb_mod, main = "with covariates")

herb_mod_nocov <- MRFcov(data = herb_mat, n_nodes = length(herbivores), n_covariates = 6, family = "gaussian")
herbHM_nocov <- plotMRF_hm(herb_mod_nocov, main = "without covariates")

gridExtra::grid.arrange(herbHM_cov, herbHM_nocov, nrow = 1, top = "Herbivores co-occurrence")


# Table 1. Relative importance per species --------------------------------
# How many associations a species has? -> for each species, count coefficients != 0

grps_assoc <- assoc_count(grps_mod)
dip_assoc <- assoc_count(dip_mod)
herb_assoc <- assoc_count(herb_mod)

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
all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>% # Create a tibble of all taxa
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = taxa)+
  stat_summary(geom = "bar", fun = mean, position = "dodge")+
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge")

# Does the anthro/biotic/env include also interactions? If so - need to separate them.

