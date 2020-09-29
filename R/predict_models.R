source("R/run_models_spatial.R")
library(paletteer)

# Run predictions on the model

# ----------------------------------Temperature-------------------------- #

# Create a dataframe of locations and temperatures
locs <- med_raw %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, tmean) %>% unique()


# Groupers ----------------------------------------------------------------

# Create coordinates dataframe for spatial analysis
grps_coords <- create_coords_df(grps_mat)

# Create relative important graph to see which species are affected by other species
grps_relimp <- rel_imp_sum(grps_spat)
plot_rel_imp(species_relimp = grps_relimp, fill_colour = "#eccbae", group_name = "Groupers")
### The species we're interested in are: E. costae, E. marginatus, M. rubra

###### Epinephelus costae #####
ec_mats <- create_pres_abs_df(species_of_interest = "Epinephelus.costae", species_group = groupers)
ec_preds <- model_predictions(list_of_dfs = ec_mats, spp_coords = grps_coords, species_group = groupers)
plot_predictions(predictions_long_df = ec_preds, species_of_interest = "Epinephelus.costae")

###### Epinephelus marginatus #####
em_mats <- create_pres_abs_df(species_of_interest = "Epinephelus.marginatus", species_group = groupers)
em_preds <- model_predictions(list_of_dfs = em_mats, spp_coords = grps_coords, species_group = groupers)
plot_predictions(predictions_long_df = em_preds, species_of_interest = "Epinephelus.marginatus")

###### Mycteroperca rubra #####
mr_mats <- create_pres_abs_df(species_of_interest = "Mycteroperca.rubra", species_group = groupers)
mr_preds <- model_predictions(list_of_dfs = mr_mats, spp_coords = grps_coords, species_group = groupers)
plot_predictions(predictions_long_df = mr_preds, species_of_interest = "Mycteroperca.rubra")

# Seabream ----------------------------------------------------------------

dip_coords <- create_coords_df(dip_mat)

# Create relative important graph to see which species are affected by other species
dip_relimp <- rel_imp_sum(dip_spat)
plot_rel_imp(species_relimp = dip_relimp, fill_colour = "#d29a4c", group_name = "Seabreams")
### All the species are of interest

###### Epinephelus costae #####
ec_mats <- create_pres_abs_df(species_of_interest = "Epinephelus.costae", species_group = groupers)
ec_preds <- model_predictions(list_of_dfs = ec_mats, spp_coords = grps_coords, species_group = groupers)
plot_predictions(predictions_long_df = ec_preds, species_of_interest = "Epinephelus.costae")

###### Epinephelus marginatus #####
em_mats <- create_pres_abs_df(species_of_interest = "Epinephelus.marginatus", species_group = groupers)
em_preds <- model_predictions(list_of_dfs = em_mats, spp_coords = grps_coords, species_group = groupers)
plot_predictions(predictions_long_df = em_preds, species_of_interest = "Epinephelus.marginatus")

###### Epinephelus marginatus #####
mr_mats <- create_pres_abs_df(species_of_interest = "Mycteroperca.rubra", species_group = groupers)
mr_preds <- model_predictions(list_of_dfs = mr_mats, spp_coords = grps_coords, species_group = groupers)
plot_predictions(predictions_long_df = mr_preds, species_of_interest = "Mycteroperca.rubra")


# Herbivores --------------------------------------------------------------



# ----------------------------------MPA-------------------------- #

# Create a dataframe of locations and MPA
locs_mpas <- med_clean %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, mpa)

# Groupers ----------------------------------------------------------------

grps_mpa_predict <- MRFcov::predict_MRF(data = grps_mat, MRF_mod = grps_spat) %>%
  `colnames<-`(groupers) %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  pivot_longer(2:length(.),
               names_to = "species",
               values_to = "predict_obs") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  left_join(locs_mpas, by = "loc") %>% 
  select(loc, mpa, 2:5)


# Plot probability of occurrence of species as a function of MPA:
grps_mpa_predict %>%
  ggplot() +
  aes(x = mpa, y = log(predict_obs), fill = species) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_paletteer_d("ggsci::uniform_startrek") +
  xlab("In/Out MPA") +
  ylab("Predicted observations, log scaled") +
  labs(title = "Observation predictions", subtitle = "Groupers")



# Seabream ----------------------------------------------------------------

dip_mpa_predict <- MRFcov::predict_MRF(data = dip_mat, MRF_mod = dip_spat) %>%
  `colnames<-`(diplodus) %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  pivot_longer(2:length(.),
               names_to = "species",
               values_to = "predict_obs") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  left_join(locs_mpas, by = "loc") %>% 
  select(loc, mpa, 2:5)


# Plot probability of occurrence of species as a function of MPA:
dip_mpa_predict %>%
  ggplot() +
  aes(x = mpa, y = log(predict_obs), fill = species) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_paletteer_d("ggsci::uniform_startrek") +
  xlab("In/Out MPA") +
  ylab("Predicted observations, log_scaled") +
  labs(title = "Observation predictions", subtitle = "Seabreams")



# Herbivores --------------------------------------------------------------

herb_mpa_predict <- MRFcov::predict_MRF(data = herb_mat, MRF_mod = herb_spat) %>%
  `colnames<-`(herbivores) %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  pivot_longer(2:length(.),
               names_to = "species",
               values_to = "predict_obs") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  left_join(locs_mpas, by = "loc") %>% 
  select(loc, mpa, 2:5)


# Plot probability of occurrence of species as a function of MPA:
herb_mpa_predict %>%
  ggplot() +
  aes(x = mpa, y = log(predict_obs), fill = species) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_paletteer_d("ggsci::uniform_startrek") +
  xlab("In/Out MPA") +
  ylab("Predicted observations, log scaled") +
  labs(title = "Observation predictions", subtitle = "Herbivores")

