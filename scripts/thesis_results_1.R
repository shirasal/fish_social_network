
source("R/packages.R")
load("data/all_objects.RData")
source("R/functions.R")
source("R/run_models.R")
library(wesanderson)

med_clean %<>% select(-sal) # removing salinity to avoid data loss because of NAs until completed.
med_clean %>% glimpse()

# Check auto-correlations -------------------------------------------------

# # Temperature - Salinity
# 
# med_clean %>% ggplot() + 
#   aes(x = temp, y = sal) + 
#   geom_point()

# Temperature - Invasive species
# TODO add column of native(T/F) to data

# Figure 1. Co-occurrence of species with and without covariates ----------
# Compare between co-occurrence with and without covariates:
# GROUPERS
grpsHM_cov <- plotMRF_hm(grps_mod, main = "with covariates")
grpsHM_nocov <- plotMRF_hm(grps_mod_nocov, main = "without covariates")
gridExtra::grid.arrange(grpsHM_cov, grpsHM_nocov, nrow = 1, top = "Groupers co-occurrence")

# SEABREAM
dipHM_cov <- plotMRF_hm(dip_mod, main = "with covariates")
dipHM_nocov <- plotMRF_hm(dip_mod_nocov, main = "without covariates")
gridExtra::grid.arrange(dipHM_cov, dipHM_nocov, nrow = 1, top = "Seabream co-occurrence")

# HERBIVORES
herbHM_cov <- plotMRF_hm(herb_mod, main = "with covariates")
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
# How much is does a predictor affect the data
grps_relimp <- rel_imp_sum(grps_mod)
dip_relimp <- rel_imp_sum(dip_mod)
herb_relimp <- rel_imp_sum(herb_mod)

all_relimp <- list(groupers = grps_relimp,
                   seabream = dip_relimp,
                   herbivores = herb_relimp)

# Table of mean relative importance of covariates per taxa:
relimp_table <- sapply(X = all_relimp, FUN = function(x) x[,-1] %>% colMeans())

# Figure: how much are predictors important for all fish?
all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>% # Create a tibble of all taxa
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = taxa)+
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge")
## A different look (facetted)
all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>% # Create a tibble of all taxa
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = taxa)+
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  facet_wrap(~taxa, nrow = 3) +
  theme(legend.position = "none")

# Rel_imp figures for species:
# Groupers
grps_rel_imp_bp <- all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>%
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  filter(taxa == "groupers") %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = species) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) + 
  theme_classic() + 
  scale_fill_manual(values = wes_palette(n = 5, name = "Moonrise3"))

# Diplodus
dip_rel_imp_bp <- all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>%
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  filter(taxa == "seabream") %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = species) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) + 
  theme_classic() + 
  scale_fill_manual(values = wes_palette(n = 5, name = "FantasticFox1"))

# Herbivores
herb_rel_imp_bp <- all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>%
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  filter(taxa == "herbivores") %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = species) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) + 
  theme_classic() + 
  scale_fill_manual(values = wes_palette(n = 5, name = "Rushmore1"))

gridExtra::grid.arrange(grps_rel_imp_bp, dip_rel_imp_bp, herb_rel_imp_bp)

# Exchange covaraite with species (species as x and cov as fill)
all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>%
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  filter(taxa == "groupers") %>% 
  ggplot() +
  aes(x = species, y = rel_imp, fill = covariate) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) + 
  theme_classic() + 
  scale_fill_manual(values = wes_palette(n = 5, name = "Zissou1"))

all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>%
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  filter(taxa == "seabream") %>% 
  ggplot() +
  aes(x = species, y = rel_imp, fill = covariate) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) + 
  theme_classic() + 
  scale_fill_manual(values = wes_palette(n = 5, name = "Zissou1"))

all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>%
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  filter(taxa == "herbivores") %>% 
  ggplot() +
  aes(x = species, y = rel_imp, fill = covariate) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) + 
  theme_classic() + 
  scale_fill_manual(values = wes_palette(n = 5, name = "Zissou1"))

# rel_imp*effect_size for a better index of the importance (when summing up)


# Fig. 3: Associations per MPA --------------------------------------------

# Working on that in 'run_models.R'

############################################################################

grps_dir_assoc <- direction_assoc(taxa = grps_mod)
dip_dir_assoc <- direction_assoc(taxa = dip_mod)
herb_dir_assoc <- direction_assoc(taxa = herb_mod)