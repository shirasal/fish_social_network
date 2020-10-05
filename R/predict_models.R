# source("R/run_models_spatial.R")

# Run predictions on the model

# ----------------------------------Temperature-------------------------- #

# Create a dataframe of locations and temperatures
locs <- med_raw %>% 
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE), loc = paste0(site, "_", trans)) %>% 
  select(loc, tmean, mpa) %>% unique()


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
# ggsave(ec_pred_plot, filename = "figures/predictions/e_costae_temp.png", device = "png", dpi = 300)
ec_mpa_plot <- plot_bar_predictions(predictions_long_df = ec_preds, species_of_interest = "Epinephelus.costae")
# ggsave(ec_mpa_plot, filename = "figures/predictions/e_costae_mpa.png", device = "png", dpi = 300)

###### Epinephelus marginatus #####
em_mats <- create_pres_abs_df(species_of_interest = "Epinephelus.marginatus", species_group = groupers)
em_preds <- model_predictions(list_of_dfs = em_mats, spp_coords = grps_coords, species_group = groupers)
em_pred_plot <- plot_predictions(predictions_long_df = em_preds, species_of_interest = "Epinephelus.marginatus")
# ggsave(em_pred_plot, filename = "figures/predictions/e_marginatus_temp.png", device = "png", dpi = 300)
em_mpa_plot <- plot_bar_predictions(predictions_long_df = em_preds, species_of_interest = "Epinephelus.marginatus")
# ggsave(em_mpa_plot, filename = "figures/predictions/e_marginatus_mpa.png", device = "png", dpi = 300)

###### Mycteroperca rubra #####
mr_mats <- create_pres_abs_df(species_of_interest = "Mycteroperca.rubra", species_group = groupers)
mr_preds <- model_predictions(list_of_dfs = mr_mats, spp_coords = grps_coords, species_group = groupers)
mr_pred_plot <- plot_predictions(predictions_long_df = mr_preds, species_of_interest = "Mycteroperca.rubra")
# ggsave(mr_pred_plot, filename = "figures/predictions/m_rubra_temp.png", device = "png", dpi = 300)
mr_mpa_plot <- plot_bar_predictions(predictions_long_df = mr_preds, species_of_interest = "Mycteroperca.rubra")
# ggsave(mr_mpa_plot, filename = "figures/predictions/m_rubra_mpa.png", device = "png", dpi = 300)

# Seabream ----------------------------------------------------------------

dip_coords <- create_coords_df(dip_mat)
dip_relimp <- rel_imp_sum(dip_spat)
plot_rel_imp(species_relimp = dip_relimp, fill_colour = "#d29a4c", group_name = "Seabreams")
### All the species are of interest

###### Diplodus annularis #####
da_mats <- create_pres_abs_df(species_of_interest = diplodus[1], species_group = diplodus)
da_preds <- model_predictions(list_of_dfs = da_mats, spp_coords = dip_coords, species_group = diplodus)
da_pred_plot <- plot_predictions(predictions_long_df = da_preds, species_of_interest = diplodus[1])
# ggsave(da_pred_plot, filename = "figures/predictions/d_annularis_temp.png", device = "png", dpi = 300)
da_mpa_plot <- plot_bar_predictions(predictions_long_df = da_preds, species_of_interest = diplodus[1])
# ggsave(da_mpa_plot, filename = "figures/predictions/d_annularis_mpa.png", device = "png", dpi = 300)

###### Diplodus puntazzo #####
dp_mats <- create_pres_abs_df(species_of_interest = diplodus[2], species_group = diplodus)
dp_preds <- model_predictions(list_of_dfs = dp_mats, spp_coords = dip_coords, species_group = diplodus)
dp_pred_plot <- plot_predictions(predictions_long_df = dp_preds, species_of_interest = diplodus[2])
# ggsave(dp_pred_plot, filename = "figures/predictions/d_puntazzo_temp.png", device = "png", dpi = 300)
dp_mpa_plot <- plot_bar_predictions(predictions_long_df = dp_preds, species_of_interest = diplodus[2])
# ggsave(dp_mpa_plot, filename = "figures/predictions/d_puntazzo_mpa.png", device = "png", dpi = 300)

###### Diplodus sargus #####
ds_mats <- create_pres_abs_df(species_of_interest = diplodus[3], species_group = diplodus)
ds_preds <- model_predictions(list_of_dfs = ds_mats, spp_coords = dip_coords, species_group = diplodus)
ds_pred_plot <- plot_predictions(predictions_long_df = ds_preds, species_of_interest = diplodus[3])
# ggsave(ds_pred_plot, filename = "figures/predictions/d_sargus_temp.png", device = "png", dpi = 300)
ds_mpa_plot <- plot_bar_predictions(predictions_long_df = ds_preds, species_of_interest = diplodus[3])
# ggsave(ds_mpa_plot, filename = "figures/predictions/d_sargus_mpa.png", device = "png", dpi = 300)

###### Diplodus vulgaris #####
dv_mats <- create_pres_abs_df(species_of_interest = diplodus[4], species_group = diplodus)
dv_preds <- model_predictions(list_of_dfs = dv_mats, spp_coords = dip_coords, species_group = diplodus)
dv_pred_plot <- plot_predictions(predictions_long_df = dv_preds, species_of_interest = diplodus[4])
# ggsave(dv_pred_plot, filename = "figures/predictions/d_vulgaris_temp.png", device = "png", dpi = 300)
dv_mpa_plot <- plot_bar_predictions(predictions_long_df = dv_preds, species_of_interest = diplodus[4])
# ggsave(dv_mpa_plot, filename = "figures/predictions/d_vulgaris_mpa.png", device = "png", dpi = 300)

###### Diplodus cervinus #####
dc_mats <- create_pres_abs_df(species_of_interest = diplodus[5], species_group = diplodus)
dc_preds <- model_predictions(list_of_dfs = dc_mats, spp_coords = dip_coords, species_group = diplodus)
dc_pred_plot <- plot_predictions(predictions_long_df = dc_preds, species_of_interest = diplodus[5])
# ggsave(dc_pred_plot, filename = "figures/predictions/d_cervinus_temp.png", device = "png", dpi = 300)
dc_mpa_plot <- plot_bar_predictions(predictions_long_df = dc_preds, species_of_interest = diplodus[5])
# ggsave(dc_mpa_plot, filename = "figures/predictions/d_cervinus_mpa.png", device = "png", dpi = 300)

# Herbivores --------------------------------------------------------------

herb_coords <- create_coords_df(herb_mat)
herb_relimp <- rel_imp_sum(herb_spat)
plot_rel_imp(species_relimp = herb_relimp, fill_colour = "#145d82", group_name = "Herbivores")
### All species are of interest

###### Siganus rivulatus #####
riv_mats <- create_pres_abs_df(species_of_interest = "Siganus.rivulatus", species_group = herbivores)
riv_preds <- model_predictions(list_of_dfs = riv_mats, spp_coords = herb_coords, species_group = herbivores)
riv_pred_plot <- plot_predictions(predictions_long_df = riv_preds, species_of_interest = "Siganus.rivulatus")
# ggsave(riv_pred_plot, filename = "figures/predictions/s_rivulatus_temp.png", device = "png", dpi = 300)
riv_mpa_plot <- plot_bar_predictions(predictions_long_df = riv_preds, species_of_interest = "Siganus.rivulatus")
# ggsave(riv_mpa_plot, filename = "figures/predictions/s_rivulatus_mpa.png", device = "png", dpi = 300)

###### Siganus luridus #####
lurid_mats <- create_pres_abs_df(species_of_interest = "Siganus.luridus", species_group = herbivores)
lurid_preds <- model_predictions(list_of_dfs = lurid_mats, spp_coords = herb_coords, species_group = herbivores)
lurid_pred_plot <- plot_predictions(predictions_long_df = lurid_preds, species_of_interest = "Siganus.luridus")
# ggsave(lurid_pred_plot, filename = "figures/predictions/s_luridus_temp.png", device = "png", dpi = 300)
lurid_mpa_plot <- plot_bar_predictions(predictions_long_df = lurid_preds, species_of_interest = "Siganus.luridus")
# ggsave(lurid_mpa_plot, filename = "figures/predictions/s_luridus_mpa.png", device = "png", dpi = 300)

###### Sarpa salpa #####
salpa_mats <- create_pres_abs_df(species_of_interest = "Sarpa.salpa", species_group = herbivores)
salpa_preds <- model_predictions(list_of_dfs = salpa_mats, spp_coords = herb_coords, species_group = herbivores)
salpa_pred_plot <- plot_predictions(predictions_long_df = salpa_preds, species_of_interest = "Sarpa.salpa")
# ggsave(salpa_pred_plot, filename = "figures/predictions/s_salpa_temp.png", device = "png", dpi = 300)
salpa_mpa_plot <- plot_bar_predictions(predictions_long_df = salpa_preds, species_of_interest = "Sarpa.salpa")
# ggsave(salpa_mpa_plot, filename = "figures/predictions/s_salpa_mpa.png", device = "png", dpi = 300)

###### Sparisoma cretense #####
cret_mats <- create_pres_abs_df(species_of_interest = "Sparisoma.cretense", species_group = herbivores)
cret_preds <- model_predictions(list_of_dfs = cret_mats, spp_coords = herb_coords, species_group = herbivores)
cret_pred_plot <- plot_predictions(predictions_long_df = cret_preds, species_of_interest = "Sparisoma.cretense")
# ggsave(cret_pred_plot, filename = "figures/predictions/s_cretense_temp.png", device = "png", dpi = 300)
cret_mpa_plot <- plot_bar_predictions(predictions_long_df = cret_preds, species_of_interest = "Sparisoma.cretense")
# ggsave(cret_mpa_plot, filename = "figures/predictions/s_cretense_mpa.png", device = "png", dpi = 300)
