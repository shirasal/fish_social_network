source("R/packages.R")
load("data/all_objects.RData")
source("R/functions.R")

# Run all models with and without covariates

# Groupers ----------------------------------------------------------------

grps_mod <- MRFcov(data = grps_mat, n_nodes = length(groupers), n_covariates = 5, family = "gaussian")
grps_mod_nocov <- MRFcov(data = grps_mat, n_nodes = length(groupers), n_covariates = 5, family = "gaussian")

# Seabream ----------------------------------------------------------------

dip_mod <- MRFcov(data = dip_mat, n_nodes = length(diplodus), n_covariates = 5, family = "gaussian")
dip_mod_nocov <- MRFcov(data = dip_mat, n_nodes = length(diplodus), n_covariates = 5, family = "gaussian")

# Herbivores --------------------------------------------------------------

herb_mod <- MRFcov(data = herb_mat, n_nodes = length(herbivores), n_covariates = 5, family = "gaussian")
herb_mod_nocov <- MRFcov(data = herb_mat, n_nodes = length(herbivores), n_covariates = 5, family = "gaussian")

# Take a look at the output of the models:
grps_mod
grps_mod$graph # Only association parameters of species
grps_mod$intercepts
grps_mod$direct_coefs # association parameters of species and covariates
grps_mod$direct_coefs[1,] %>% View
# How to plot this model? or how to aggregate the 

# Create a dataframe of locations and temperatures
locs <- med_clean %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, temp) %>% unique()

# Run predictions on the model
grps_predict <- MRFcov::predict_MRF(data = grps_mat, MRF_mod = grps_mod) %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  select(loc, 2:6) %>% 
  left_join(locs, by = "loc")

# How to plot? TODO
grps_predict %>% 
  ggplot() +
  aes(x = temp, y = Epinephelus.costae)
  geom_point() +
  stat_smooth(method = "lm")

grps_mod$key_coefs # from direct_coefs
