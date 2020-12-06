source("R/packages.R")
load("data/all_objects.RData")
source("R/run_models.R")
source("R/functions.R")
source("R/rel_imp.R")

# Functions ---------------------------------------------------------------

## MPA

vis_mpa_pred_pair <- function(species_i, species_j, spp_mat, spp_mod, guild, n_spp){
  # Create a vector of all other species except species_j
  all_other_species <- guild[-which(guild == species_j)]
  
  # Scenario 1: species_j is absent, other species are at their mean abundance
  j_abs <- spp_mat %>% 
    mutate(temp = median(temp),
           depth = median(depth),
           prod = median(prod),
           across(.cols = all_of(species_j), .fns = function(x) 0),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(mpa) %>% 
    sample_n(1)
  # Scenario 2: species_j is at its max abundance, other species are at their mean abundance
  j_max <- spp_mat %>% 
    mutate(temp = median(temp),
           depth = median(depth),
           prod = median(prod),
           across(.cols = all_of(species_j), .fns = max),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(mpa) %>% 
    sample_n(1)
  
  # Create predictions
  ## For when species j is absent
  predict_abs <- predict_MRF(j_abs, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(mpa = j_abs$mpa)
  ## For when species j is absent
  predict_max <- predict_MRF(j_max, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(mpa = j_max$mpa)
  
  # Put the two scenarios together
  mpa_predict <- bind_rows(predict_abs, predict_max, .id = "scenario") %>% 
    mutate(scenario = case_when(scenario == 1 ~ "absent",
                                scenario == 2 ~ "present"))
  
  # Visualise the predictions
  ## Create a dataframe with all the predictions, sorted by scenario
  
  predictions_mpa <- mpa_predict %>% 
    pivot_longer(cols = all_of(2:(1+n_spp)),
                 names_to = "species",
                 values_to = "prediction", 
                 names_repair = "minimal")
  
  ## Plot the predictions:
  predictions_mpa %>%
    filter(species == species_i) %>% 
    ggplot() +
    aes(x = mpa, y = prediction, fill = scenario) +
    stat_summary(geom = "bar", fun = "mean", position = "dodge") +
    # stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.8), width = 0.2) +
    xlab("MPA") + ylab("Prediction") +
    labs(title = "Observation predictions",
         subtitle = stringr::str_replace(species_i, "\\.", "\\ "),
         fill = stringr::str_replace(species_j, "\\.", "\\ ")) +
    scale_fill_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
    theme(legend.title = element_text(face = "italic"), plot.subtitle = element_text(face = "italic"))
}

## Temperature

vis_temp_pred_pair <- function(species_i, species_j, spp_mat, spp_mod, guild, n_spp){
  # Create a vector of all other species except species_j
  all_other_species <- guild[-which(guild == species_j)]
  
  # Scenario 1: species_j is absent, other species are at their mean abundance
  j_abs <- spp_mat %>% 
    mutate(depth = median(depth),
           prod = median(prod),
           mpa = TRUE,
           across(.cols = all_of(species_j), .fns = function(x) 0),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(temp = round(temp, digits = 1)) %>% 
    sample_n(1)
  # Scenario 2: species_j is at its max abundance, other species are at their mean abundance
  j_max <- spp_mat %>% 
    mutate(depth = median(depth),
           prod = median(prod),
           mpa = TRUE,
           across(.cols = all_of(species_j), .fns = max),
           across(all_of(all_other_species), .fns = mean)) %>% 
    group_by(temp = round(temp, digits = 1)) %>% 
    sample_n(1)
  
  # Create predictions
  ## For when species j is absent
  predict_abs <- predict_MRF(j_abs, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(temp = j_abs$temp)
  ## For when species j is absent
  predict_max <- predict_MRF(j_max, spp_mod) %>% 
    `colnames<-`(guild) %>% 
    as_data_frame() %>% 
    mutate(temp = j_max$temp)
  
  # Put the two scenarios together
  temp_predict <- bind_rows(predict_abs, predict_max, .id = "scenario") %>% 
    mutate(scenario = case_when(scenario == 1 ~ "absent",
                                scenario == 2 ~ "present"))
  
  # Visualise the predictions
  ## Create a dataframe with all the predictions, sorted by scenario
  
  predictions_temp <- temp_predict %>% 
    pivot_longer(cols = all_of(2:(1+n_spp)),
                 names_to = "species",
                 values_to = "prediction", 
                 names_repair = "minimal")
  
  ## Plot the predictions:
  predictions_temp %>%
    filter(species == species_i) %>% 
    ggplot() +
    aes(x = temp, y = prediction, col = scenario) +
    geom_smooth(method = "lm", formula = y ~ x, cex = 3, alpha = 0.1) +
    xlab("Temperature (scaled)") + ylab("Prediction") +
    labs(title = "Observation predictions",
         subtitle = stringr::str_replace(species_i, "\\.", "\\ "),
         col = stringr::str_replace(species_j, "\\.", "\\ ")) +
    scale_color_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9")) +
    theme(legend.title = element_text(face = "italic"), plot.subtitle = element_text(face = "italic"))
}




# Check which species pairs to visualise ----------------------------------
# * in brackets = relative importance values; in square brackets = coefficient *
lapply(grps_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))
# Epinephelus.costae ~ temp_Serranus.cabrilla (0.1075085) [0.06423452]
# Epinephelus.marginatus ~ temp_Mycteroperca.rubra (0.1426722) [+0.08344348]
# Mycteroperca.rubra ~ emp_Epinephelus.marginatus (0.1134032) [+0.08344348]

lapply(dip_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))
# Diplodus.annularis ~ temp_Diplodus.vulgaris (0.1179551) [0.08855133]
# Diplodus.puntazzo ~ mpa_Diplodus.vulgaris (0.3427963) [0.1612258]
# Diplodus.sargus ~ mpa_Diplodus.vulgaris (0.1305238) [0.2598522]
# Diplodus.vulgaris ~ mpa_Diplodus.sargus (0.1936802) [0.2598522]

lapply(herb_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))
# Siganus.rivulatus ~ temp_Sparisoma.cretense (0.2190088) [-0.3494146]
# Siganus.rivulatus ~ temp_Siganus.luridus (0.1003014) [-0.2364633]
# Siganus.luridus ~ mpa_Sparisoma.cretense (0.1296816) [0.2721658]
# Sparisoma.cretense ~ temp_Siganus.rivulatus (0.2392358) [-0.3494146]
# Sparisoma.cretense ~ mpa_Siganus.luridus (0.1451481) [0.2721658]

############################    Predictions    ############################

# Epinephelus.marginatus ~ temp_Mycteroperca.rubra (0.1426722) [+0 --------
vis_temp_pred_pair(species_i = "Epinephelus.marginatus", species_j = "Mycteroperca.rubra", 
                  spp_mat = grps_mat, spp_mod = grps_mod, guild = groupers, n_spp = 5) %>% 
  ggsave(filename = "figures/predictions/E_marginatus-M_rubra--TEMP.png", device = "png")

# Mycteroperca.rubra ~ emp_Epinephelus.marginatus (0.1134032) [+0. --------
vis_temp_pred_pair(species_i = "Mycteroperca.rubra", species_j = "Epinephelus.marginatus", 
                  spp_mat = grps_mat, spp_mod = grps_mod, guild = groupers, n_spp = 5) %>% 
  ggsave(filename = "figures/predictions/M_rubra--E_marginatus--TEMP.png", device = "png")



# Diplodus.annularis ~ temp_Diplodus.vulgaris (0.1179551) [0.08855 --------
vis_temp_pred_pair("Diplodus.annularis", "Diplodus.vulgaris", 
                   dip_mat, dip_mod, diplodus, 5) %>% 
  ggsave(filename = "figures/predictions/D_annularis-D_vulgaris--TEMP.png", device = "png")

# Diplodus.puntazzo ~ mpa_Diplodus.vulgaris (0.3427963) [0.1612258] -------
vis_mpa_pred_pair(species_i = "Diplodus.puntazzo", species_j = "Diplodus.vulgaris", 
                  spp_mat = dip_mat, spp_mod = dip_mod, guild = diplodus, n_spp = 5) %>% 
  ggsave(filename = "figures/predictions/D_puntazzo-D_vulgaris--MPA.png", device = "png")

# Diplodus.sargus ~ mpa_Diplodus.vulgaris (0.1305238) [0.2598522] ---------
vis_mpa_pred_pair("Diplodus.sargus", "Diplodus.vulgaris", dip_mat, dip_mod, diplodus, 5) %>% 
  ggsave(filename = "figures/predictions/D_sargus-D_vulgaris--MPA.png", device = "png")

# Diplodus.vulgaris ~ mpa_Diplodus.sargus (0.1936802) [0.2598522] ---------
vis_mpa_pred_pair("Diplodus.vulgaris", "Diplodus.sargus", dip_mat, dip_mod, diplodus, 5) %>% 
  ggsave(filename = "figures/predictions/D_vulgaris-D_sargus--MPA.png", device = "png")



# Siganus.rivulatus ~ temp_Sparisoma.cretense (0.2190088) [-0.3494 --------
vis_temp_pred_pair("Siganus.rivulatus", "Sparisoma.cretense", 
                   herb_mat, herb_mod, herbivores, 4) %>% 
  ggsave(filename = "figures/predictions/S_rivulatus-S_cretense--TEMP.png", device = "png")

# Siganus.rivulatus ~ temp_Siganus.luridus (0.1003014) [-0.2364633] -------
vis_temp_pred_pair(species_i = "Siganus.rivulatus", species_j = "Siganus.luridus", 
                   spp_mat = herb_mat, spp_mod = herb_mod, guild = herbivores, n_spp = 4) %>% 
  ggsave(filename = "figures/predictions/S_rivulatus-S_luridus--TEMP.png", device = "png")

# Siganus.luridus ~ mpa_Sparisoma.cretense (0.1296816) [0.2721658] --------
vis_mpa_pred_pair(species_i = "Siganus.luridus", species_j = "Sparisoma.cretense", 
                   spp_mat = herb_mat, spp_mod = herb_mod, guild = herbivores, n_spp = 4) %>% 
  ggsave(filename = "figures/predictions/S_luridus-S_cretense--MPA.png", device = "png")

# Sparisoma.cretense ~ temp_Siganus.rivulatus (0.2392358) [-0.3494 --------
vis_temp_pred_pair("Sparisoma.cretense", "Siganus.rivulatus",
                   spp_mat = herb_mat, spp_mod = herb_mod, guild = herbivores, n_spp = 4) %>% 
  ggsave(filename = "figures/predictions/S_cretense-S_rivulatus--TEMP.png", device = "png")

# Sparisoma.cretense ~ mpa_Siganus.luridus (0.1451481) [0.2721658] --------
vis_mpa_pred_pair(species_i = "Sparisoma.cretense", species_j = "Siganus.luridus", 
                  spp_mat = herb_mat, spp_mod = herb_mod, guild = herbivores, n_spp = 4) %>% 
  ggsave(filename = "figures/predictions/S_cretense-S_luridus--MPA.png", device = "png")



