# source("R/models.R")
# file.edit("Prediction_visualisations.Rmd")


# Groupers | Temperature --------------------------------------------------

#----
# Epinephelus.costae ~ Serranus.cabrilla x Temperature
# RI = 0.08 ; Coef = 0.06
vis_temp_pred_pair("Epinephelus.costae", "Serranus.cabrilla", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/E_costae-S_cabrilla--TEMP.png", device = "png")

vis_raw_temp(species_i = "Epinephelus.costae", species_j = "Serranus.cabrilla", spp_mat = grps_mat) %>% 
  ggsave(filename = "figures/predictions/E_costae-S_cabrilla--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Serranus.cabrilla ~ Epinephelus.costae x Temperature
# RI = 0.01 ; Coef = 0.06
vis_temp_pred_pair("Serranus.cabrilla", "Epinephelus.costae", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/S_cabrilla-E_costae--TEMP.png", device = "png")

#----
# Epinephelus.costae ~ Serranus.cabrilla x MPA
# RI = 0.07; Coef = -0.05
vis_mpa_pred_pair("Epinephelus.costae", "Serranus.cabrilla", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/E_costae-S_cabrilla--TEMP.png", device = "png")

vis_raw_mpa(spp_mat = grps_mat, "Epinephelus.costae", "Serranus.cabrilla") %>% 
  ggsave(filename = "figures/predictions/E_costae-S_cabrilla--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)



# Groupers | MPA ----------------------------------------------------------

#----
# Epinephelus.costae ~ Epinephelus.marginatus x MPA
# RI = 0.07 ; # Coef = -0.05
vis_mpa_pred_pair("Epinephelus.costae", "Epinephelus.marginatus", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/E_marginatus-E_costae--MPA.png", device = "png")

vis_raw_mpa(spp_mat = grps_mat, "Epinephelus.costae", "Epinephelus.marginatus") %>% 
  ggsave(filename = "figures/predictions/E_marginatus-E_costae--MPA_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Epinephelus.marginatus ~ Epinephelus.costae x MPA
# RI = 0.06 ; Coef = -0.05
vis_mpa_pred_pair("Epinephelus.costae", "Epinephelus.marginatus", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/E_costae-E_marginatus--MPA.png", device = "png")

vis_raw_mpa(spp_mat = grps_mat, "Epinephelus.costae", "Epinephelus.costae") %>% 
  ggsave(filename = "figures/predictions/E_costae-E_marginatus--MPA_raw.png", device = "png", scale = c(2,1), dpi = 150)




# Seabreams | Temperature -------------------------------------------------

#----
# Diplodus.annularis ~ Diplodus.vulgaris x Temperature
# RI = 0.1 ; Coef = 0.08
vis_temp_pred_pair("Diplodus.annularis", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_annularis-D_vulgaris--TEMP.png", device = "png")

vis_raw_temp(species_i = "Diplodus.annularis", species_j = "Diplodus.vulgaris", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_annularis-D_vulgaris--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.vulgaris ~ Diplodus.annularis x Temperature
# RI = 0.02 | Coef = 0.08
vis_temp_pred_pair("Diplodus.vulgaris", "Diplodus.annularis", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_annularis--TEMP.png", device = "png")

vis_raw_temp(species_i = "Diplodus.vulgaris", species_j = "Diplodus.annularis", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_annularis--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.puntazzo ~ Diplodus.vulgaris x Temperature
# RI = 0.01 | Coef = -0.03
vis_temp_pred_pair("Diplodus.puntazzo", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_puntazzo-D_vulgaris--TEMP.png", device = "png")

vis_raw_temp(species_i = "Diplodus.puntazzo", species_j = "Diplodus.vulgaris", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_puntazzo-D_vulgaris--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.sargus ~ Diplodus.vulgaris x Temperature
# RI = 0.05 | Coef = 0.13
vis_temp_pred_pair("Diplodus.sargus", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_sargus-D_vulgaris--TEMP.png", device = "png")

vis_raw_temp(species_i = "Diplodus.sargus", species_j = "Diplodus.vulgaris", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_sargus-D_vulgaris--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.vulgaris ~ Diplodus.sargus x Temperature
# RI = 0.05 | Coef = 0.13
vis_temp_pred_pair("Diplodus.vulgaris", "Diplodus.sargus", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_sargus--TEMP.png", device = "png")

vis_raw_temp(species_i = "Diplodus.vulgaris", species_j = "Diplodus.sargus", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_sargus--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)



# Seabreams | MPA ---------------------------------------------------------

#----
# Diplodus.annularis ~ Diplodus.vulgaris x MPA
#   RI = 0.05 | Coef = 0.06
vis_mpa_pred_pair("Diplodus.annularis", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_annularis-D_vulgaris--TEMP.png", device = "png")

vis_raw_mpa(species_i = "Diplodus.annularis", species_j = "Diplodus.vulgaris", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_annularis-D_vulgaris--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.puntazzo ~ Diplodus.vulgaris x MPA
# RI = 0.34 | Coef = 0.15
vis_mpa_pred_pair("Diplodus.puntazzo", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_puntazzo-D_vulgaris--TEMP.png", device = "png")

vis_raw_mpa(species_i = "Diplodus.puntazzo", species_j = "Diplodus.vulgaris", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_puntazzo-D_vulgaris--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.sargus ~ Diplodus.vulgaris x MPA
# RI = 0.2 | Coef = 0.26
vis_mpa_pred_pair("Diplodus.sargus", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_sargus-D_vulgaris--TEMP.png", device = "png")

vis_raw_mpa(species_i = "Diplodus.sargus", species_j = "Diplodus.vulgaris", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_sargus-D_vulgaris--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.vulgaris ~ Diplodus.sargus x MPA
# RI = 0.21 | Coef = 0.26
vis_mpa_pred_pair("Diplodus.vulgaris", "Diplodus.sargus", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_sargus--TEMP.png", device = "png")

vis_raw_mpa(species_i = "Diplodus.vulgaris", species_j = "Diplodus.sargus", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_sargus--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.vulgaris ~ Diplodus.puntazzo x MPA
# RI = 0.07 | Coef = 0.15
vis_mpa_pred_pair("Diplodus.vulgaris", "Diplodus.puntazzo", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_puntazzo--TEMP.png", device = "png")

vis_raw_mpa(species_i = "Diplodus.vulgaris", species_j = "Diplodus.puntazzo", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_puntazzo--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)

#----
# Diplodus.vulgaris ~ Diplodus.annularis x MPA
# RI = 0.01 | Coef = 0.06
vis_mpa_pred_pair("Diplodus.vulgaris", "Diplodus.annularis", dip_mat, dip_pois, diplodus) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_annularis--TEMP.png", device = "png")

vis_raw_mpa(species_i = "Diplodus.vulgaris", species_j = "Diplodus.annularis", spp_mat = dip_mat) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_annularis--TEMP_raw.png", device = "png", scale = c(2,1), dpi = 150)




