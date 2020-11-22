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

lapply(grps_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))
# Epinephelus.costae ~ mpa_Epinephelus.marginatus (0.1147587) [-]
# Epinephelus.marginatus ~ mpa_Epinephelus.costae (0.1597966) [-]

lapply(dip_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))
# Diplodus.annularis ~ temp_Diplodus.sargus (0.4188775) [+]
# Diplodus.annularis ~ temp_Diplodus.vulgaris (0.1550212) [+]

lapply(herb_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))
# Siganus.luridus ~ temp_Siganus.rivulatus (0.3665292) [-]


############################    Predictions    ############################

# Epinephelus.costae ~ mpa_Epinephelus.marginatus (0.1147587) [-] ---------
vis_mpa_pred_pair(species_i = "Epinephelus.costae", species_j = "Epinephelus.marginatus", 
                  spp_mat = grps_mat, spp_mod = grps_mod, guild = groupers, n_spp = 5) %>% 
  ggsave(filename = "figures/predictions/E_costae--E_marginatus--MPA.png", device = "png")


# Epinephelus.marginatus ~ mpa_Epinephelus.costae (0.1597966) [-] ---------
vis_mpa_pred_pair(species_i = "Epinephelus.marginatus", species_j = "Epinephelus.costae", 
                  spp_mat = grps_mat, spp_mod = grps_mod, guild = groupers, n_spp = 5) %>% 
  ggsave(filename = "figures/predictions/E_marginatus--E_costae--MPA.png", device = "png")


# Diplodus.annularis ~ temp_Diplodus.sargus (0.4188775) [+] ---------------
vis_temp_pred_pair(species_i = "Diplodus.annularis", species_j = "Diplodus.sargus", 
                   spp_mat = dip_mat, spp_mod = dip_mod, guild = diplodus, n_spp = 5) %>% 
  ggsave(filename = "figures/predictions/D_annularis--D_sargus--TEMP.png", device = "png")


# Diplodus.annularis ~ temp_Diplodus.vulgaris (0.1550212) [+] -------------
vis_temp_pred_pair(species_i = "Diplodus.annularis", species_j = "Diplodus.vulgaris", 
                   spp_mat = dip_mat, spp_mod = dip_mod, guild = diplodus, n_spp = 5) %>% 
  ggsave(filename = "figures/predictions/D_annularis--D_vulgaris--TEMP.png", device = "png")


# Siganus.luridus ~ temp_Siganus.rivulatus (0.3665292) [-] ----------------
vis_temp_pred_pair(species_i = "Siganus.luridus", species_j = "Siganus.rivulatus", 
                   spp_mat = herb_mat, spp_mod = herb_mod, guild = herbivores, n_spp = 4) %>% 
  ggsave(filename = "figures/predictions/S_luridus--S_rivulatus--TEMP.png", device = "png")

