source("R/run_models_spatial.R")

# Run predictions on the model

# ----------------------------------Temperature-------------------------- #

# Create a dataframe of locations and temperatures
locs <- med_raw %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, tmean) %>% unique()


# Groupers ----------------------------------------------------------------

# Create coordinates dataframe for spatial analysis
grps_coords <- create_coords_df(grps_mat)

# Create relative importance graph to see which species are affected by other species
grps_relimp <- rel_imp_sum(grps_spat)
plot_rel_imp(species_relimp = grps_relimp, fill_colour = "#eccbae", group_name = "Groupers")
### The species we're interested in are: E. costae, E. marginatus, M. rubra

###### Epinephelus costae #####
ec_mats <- create_pres_abs_df(species_of_interest = "Epinephelus.costae", species_group = groupers)
ec_preds <- model_predictions(list_of_dfs = ec_mats, spp_coords = grps_coords, species_group = groupers)
ec_pred_plot <- plot_predictions(predictions_long_df = ec_preds, species_of_interest = "Epinephelus.costae")
ggsave(ec_pred_plot, filename = "figures/predictions/e_costae_temp.png", device = "png", dpi = 150)

###### Epinephelus marginatus #####
em_mats <- create_pres_abs_df(species_of_interest = "Epinephelus.marginatus", species_group = groupers)
em_preds <- model_predictions(list_of_dfs = em_mats, spp_coords = grps_coords, species_group = groupers)
em_pred_plot <- plot_predictions(predictions_long_df = em_preds, species_of_interest = "Epinephelus.marginatus")
ggsave(em_pred_plot, filename = "figures/predictions/e_marginatus_temp.png", device = "png", dpi = 150)

###### Mycteroperca rubra #####
mr_mats <- create_pres_abs_df(species_of_interest = "Mycteroperca.rubra", species_group = groupers)
mr_preds <- model_predictions(list_of_dfs = mr_mats, spp_coords = grps_coords, species_group = groupers)
mr_pred_plot <- plot_predictions(predictions_long_df = mr_preds, species_of_interest = "Mycteroperca.rubra")
ggsave(mr_pred_plot, filename = "figures/predictions/m_rubra_temp.png", device = "png", dpi = 150)

# Seabream ----------------------------------------------------------------

dip_coords <- create_coords_df(dip_mat)
dip_relimp <- rel_imp_sum(dip_spat)
plot_rel_imp(species_relimp = dip_relimp, fill_colour = "#d29a4c", group_name = "Seabreams")
### All the species are of interest

###### Diplodus annularis #####
da_mats <- create_pres_abs_df(species_of_interest = diplodus[1], species_group = diplodus)
da_preds <- model_predictions(list_of_dfs = da_mats, spp_coords = dip_coords, species_group = diplodus)
da_pred_plot <- plot_predictions(predictions_long_df = da_preds, species_of_interest = diplodus[1])
ggsave(da_pred_plot, filename = "figures/predictions/d_annularis_temp.png", device = "png", dpi = 150)

###### Diplodus puntazzo #####
dp_mats <- create_pres_abs_df(species_of_interest = diplodus[2], species_group = diplodus)
dp_preds <- model_predictions(list_of_dfs = dp_mats, spp_coords = dip_coords, species_group = diplodus)
dp_pred_plot <- plot_predictions(predictions_long_df = dp_preds, species_of_interest = diplodus[2])
ggsave(dp_pred_plot, filename = "figures/predictions/d_puntazzo_temp.png", device = "png", dpi = 150)

###### Diplodus sargus #####
ds_mats <- create_pres_abs_df(species_of_interest = diplodus[3], species_group = diplodus)
ds_preds <- model_predictions(list_of_dfs = ds_mats, spp_coords = dip_coords, species_group = diplodus)
ds_pred_plot <- plot_predictions(predictions_long_df = ds_preds, species_of_interest = diplodus[3])
ggsave(ds_pred_plot, filename = "figures/predictions/d_sargus_temp.png", device = "png", dpi = 150)

###### Diplodus vulgaris #####
dv_mats <- create_pres_abs_df(species_of_interest = diplodus[4], species_group = diplodus)
dv_preds <- model_predictions(list_of_dfs = dv_mats, spp_coords = dip_coords, species_group = diplodus)
dv_pred_plot <- plot_predictions(predictions_long_df = dv_preds, species_of_interest = diplodus[4])
ggsave(dv_pred_plot, filename = "figures/predictions/d_vulgaris_temp.png", device = "png", dpi = 150)

###### Diplodus vulgaris #####
dc_mats <- create_pres_abs_df(species_of_interest = diplodus[5], species_group = diplodus)
dc_preds <- model_predictions(list_of_dfs = dc_mats, spp_coords = dip_coords, species_group = diplodus)
dc_pred_plot <- plot_predictions(predictions_long_df = dc_preds, species_of_interest = diplodus[5])
ggsave(dc_pred_plot, filename = "figures/predictions/d_cervinus_temp.png", device = "png", dpi = 150)

# Herbivores --------------------------------------------------------------

herb_coords <- create_coords_df(herb_mat)
herb_relimp <- rel_imp_sum(herb_spat)
plot_rel_imp(species_relimp = herb_relimp, fill_colour = "#145d82", group_name = "Herbivores")
###

###### Diplodus vulgaris #####
dc_mats <- create_pres_abs_df(species_of_interest = diplodus[5], species_group = diplodus)
dc_preds <- model_predictions(list_of_dfs = dc_mats, spp_coords = dip_coords, species_group = diplodus)
plot_predictions(predictions_long_df = dc_preds, species_of_interest = diplodus[5])

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

