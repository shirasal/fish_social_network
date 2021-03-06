
source("R/packages.R")
load("data/all_objects.RData")
source("R/functions.R")
source("R/run_models.R")
library(wesanderson)

# Check auto-correlations -------------------------------------------------

# # Temperature - Salinity
# 
# med_clean %>% ggplot() + 
#   aes(x = temp, y = sal) + 
#   geom_point()

# Temperature - Invasive species
# TODO add column of native(T/F) to data


# Table 1: Number of associations per species -----------------------------

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
                      seabream = dip_cov_assoc,
                      herbivores = herb_cov_assoc)

covariates_effect <- all_cov_assoc %>%
  bind_rows(.id = "taxa") %>%
  pivot_longer(3:length(.)) %>%
  rename(taxa = taxa, species = species, covariate = name, cov_assoc = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_assoc")) %>%
  left_join(all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>%
              rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
              mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")))

# Rel_imp figures for species per taxa:
covariates_effect %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = species) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) +
  facet_wrap(~taxa, nrow = 1)

# Rel_imp * (1/cov_assoc) figure for species per taxa:
covariates_effect %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp*(1/(cov_assoc)), fill = species) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) +
  facet_wrap(~taxa, nrow = 1)

# Exchange covaraite with species (species as x and cov as fill)
covariates_effect %>% 
  ggplot() +
  aes(x = species, y = rel_imp, fill = covariate) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) +
  scale_fill_manual(values = wes_palette(n = 5, name = "Zissou1"))

covariates_effect %>% 
  filter(taxa == "seabream") %>% 
  ggplot() +
  aes(x = species, y = rel_imp*cov_assoc, fill = covariate) +
  stat_summary(geom = "bar", fun = mean, position = "stack", alpha = 0.7) +
  scale_fill_manual(values = wes_palette(n = 5, name = "Zissou1")) +
  labs("")

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



