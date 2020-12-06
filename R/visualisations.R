# source("R/models.R")
# file.edit("Prediction_visualisations.Rmd")

# Groupers

## Temperature

#----
# Epinephelus.costae ~ Serranus.cabrilla x Temperature
# RI = 0.08 ; Coef = 0.06
vis_temp_pred_pair("Epinephelus.costae", "Serranus.cabrilla", grps_mat, grps_pois, groupers) %>% 
  ggsave(filename = "figures/predictions/E_costae-S_cabrilla--TEMP.png", device = "png")

vis_raw_temp(species_i = "Epinephelus.costae", species_j = "Serranus.cabrilla", spp_mat = grps_mat) %>% 
  ggsave(filename = "figures/predictions/E_costae-S_cabrilla--TEMP_raw.png", device = "png")

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
  ggsave(filename = "figures/predictions/E_costae-S_cabrilla--TEMP_raw.png", device = "png")

## MPA
vis_mpa_pred_pair("Epinephelus.costae", "Serranus.cabrilla", grps_mat, grps_pois, groupers)
ggsave(filename = "figures/predictions/E_costae-S_cabrilla--MPA.png", device = "png")

grps_mat %>% 
  mutate(scenario = if_else(Serranus.cabrilla == 0, "absent", "present")) %>% # SPECIES J
  ggplot() +
  aes(x = mpa, y = log2(Epinephelus.costae+0.1)) + # SPECIES I
  geom_boxplot(aes(colour = scenario), fill = "ghostwhite") +
  xlab("MPA") + ylab("Abundance (nonparanormal)") +
  ggtitle("Epinephelus costae") + # SPECIES I
  scale_color_manual(name = "Serranus cabrilla",  # SPECIES J
                     labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
  theme(plot.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold.italic"))

# Seabream
vis_temp_pred_pair("Diplodus.annularis", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus) 
ggsave(filename = "figures/predictions/final/D_annularis-D_vulgaris--TEMP.png", device = "png")
vis_temp_pred_pair("Diplodus.sargus", "Diplodus.vulgaris", dip_mat, dip_pois, diplodus)
ggsave(filename = "figures/predictions/final/D_sargus-D_vulgaris--TEMP.png", device = "png")
vis_temp_pred_pair("Diplodus.vulgaris", "Diplodus.sargus", dip_mat, dip_pois, diplodus)
ggsave(filename = "figures/predictions/final/D_vulgaris-D_sargus--TEMP.png", device = "png")
vis_temp_pred_pair("Diplodus.vulgaris", "Diplodus.annularis", dip_mat, dip_pois, diplodus)
ggsave(filename = "figures/predictions/final/D_vulgaris-D_annularis--TEMP.png", device = "png")

## MPA

