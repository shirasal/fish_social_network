# source("R/models.R")
# file.edit("Prediction_visualisations.Rmd")


# Seabreams | Temperature -------------------------------------------------

#----
# Diplodus.annularis ~ Diplodus.vulgaris x Temperature
vis_temp_pred_pair("Diplodus.annularis", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_annularis-D_vulgaris--TEMP.png", device = "png", width = 9, height = 6, units = "in")


# Seabreams | MPA ---------------------------------------------------------

#----
# Diplodus.vulgaris ~ Diplodus.puntazzo x MPA
vis_mpa_pred_pair("Diplodus.vulgaris", "Diplodus.puntazzo", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_puntazzo--MPA.png", device = "png", width = 9, height = 6, unit = "in")

#----
# Diplodus.sargus ~ Diplodus.vulgaris x MPA
vis_mpa_pred_pair("Diplodus.sargus", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_sargus-D_vulgaris--MPA.png", device = "png", width = 9, height = 6, units = "in")

#----
# Diplodus.vulgaris ~ Diplodus.sargus x MPA
vis_mpa_pred_pair("Diplodus.vulgaris", "Diplodus.sargus", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_sargus--MPA.png", device = "png", width = 9, height = 6, units = "in")

