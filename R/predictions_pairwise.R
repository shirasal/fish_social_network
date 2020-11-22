source("R/packages.R")
load("data/all_objects.RData")
source("R/run_models.R")
source("R/functions.R")


# Functions ---------------------------------------------------------------

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



# Check which species pairs to visualise ----------------------------------

lapply(grps_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

lapply(dip_mod$key_coefs, function(x) x %>% 
  filter(Rel_importance > 0.1) %>% 
  filter(str_detect(string = Variable, pattern = "_")))
# Diplodus.annularis + temp_Diplodus.vulgaris (0.28701685)
# Diplodus.annularis + temp_Diplodus.sargus (0.15717596)
# Diplodus.annularis + mpa_Diplodus.vulgaris (0.04576242)
# Diplodus.puntazzo + mpa_Diplodus.vulgaris (0.82966304)
# Diplodus.puntazzo + mpa_Diplodus.vulgaris (0.829663)

lapply(herb_mod$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))
# Siganus.luridus + temp_Siganus.rivulatus


# Diplodus.annularis + temp_Diplodus.vulgaris -----------------------------

# Diplodus.annularis prediction
da_abs <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.vulgaris" = 0,
         across(which(diplodus != "Diplodus.vulgaris"), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

da_max <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.vulgaris" = max(Diplodus.vulgaris),
         across(which(diplodus != "Diplodus.vulgaris"), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

da_mpa_pred_mat <- bind_rows(da_abs, da_max)


da_mpa_predict <- predict_MRF(da_mpa_pred_mat, dip_mod) %>% 
  `colnames<-`(diplodus) %>% 
  as_data_frame() %>% 
  mutate(mpa = da_mpa_pred_mat$mpa)

# Plot
abs_pred <- da_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- da_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_pres")

da_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
da_predictions_mpa %>%
  filter(species == diplodus[[2]]) %>% 
  ggplot() +
  aes(x = mpa, y = prediction, fill = model) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge") +
  # stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.8), width = 0.2) +
  xlab("MPA") + ylab("Prediction") +
  labs(title = "Observation predictions",
       subtitle = stringr::str_replace(diplodus[[2]], "\\.", "\\ "),
       colour = diplodus[[4]]) +
  scale_fill_manual(labels = c('Absent','Present'), values = c("#031D44", "#FF99C9"))
# ggsave(filename = str_glue("figures/predictions/Ecostae_Emargin_mpa_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")


# E. marginatus prediction
emargin_abs <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Epinephelus.costae" = 0,
         across(all_of(groupers[3:5]), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

emargin_max <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Epinephelus.costae" = max(Epinephelus.costae),
         across(all_of(groupers[3:5]), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

emargin_mpa_pred_mat <- bind_rows(emargin_abs, emargin_max)


emargin_mpa_predict <- predict_MRF(emargin_mpa_pred_mat, grps_mod) %>% 
  `colnames<-`(groupers) %>% 
  as_data_frame() %>% 
  mutate(mpa = emargin_mpa_pred_mat$mpa)

# Plot
abs_pred <- emargin_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- emargin_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_pres")

emargin_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_bar_predictions(emargin_predictions_mpa, groupers[[2]])
# ggsave(filename = str_glue("figures/predictions/Emargin_Ecostae_mpa_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")



# D. puntazzo + D. Vulgaris + MPA -----------------------------------------

# D. puntazzo prediction
dpuntazzo_abs <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.vulgaris" = 0,
         across(all_of(c(c(diplodus[1], diplodus[3], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dpuntazzo_max <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.vulgaris" = max(Diplodus.vulgaris),
         across(all_of(c(c(diplodus[1], diplodus[3], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dpuntazzo_mpa_pred_mat <- bind_rows(dpuntazzo_abs, dpuntazzo_max)

dpuntazzo_mpa_predict <- predict_MRF(dpuntazzo_mpa_pred_mat, dip_mod) %>% 
  `colnames<-`(diplodus) %>% 
  as_data_frame() %>% 
  mutate(mpa = dpuntazzo_mpa_pred_mat$mpa)

# Plot
abs_pred <- dpuntazzo_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- dpuntazzo_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_pres")

dpuntazzo_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_bar_predictions(dpuntazzo_predictions_mpa, diplodus[[2]])
# ggsave(filename = "figures/predictions/Dpuntazzo_Dvulgaris_mpa_nonspat.png", plot = last_plot(), dpi = 300, device = "png")


# D. vulgaris prediction
dvulgaris_abs <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.puntazzo" = 0,
         across(all_of(c(c(diplodus[1], diplodus[3], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dvulgaris_max <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.puntazzo" = max(Diplodus.puntazzo),
         across(all_of(c(c(diplodus[1], diplodus[3], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dvulgaris_mpa_pred_mat <- bind_rows(dvulgaris_abs, dvulgaris_max)

dvulgaris_mpa_predict <- predict_MRF(dvulgaris_mpa_pred_mat, dip_mod) %>% 
  `colnames<-`(diplodus) %>% 
  as_data_frame() %>% 
  mutate(mpa = dvulgaris_mpa_pred_mat$mpa)

# Plot
abs_pred <- dvulgaris_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_abs")

pres_pred <- dvulgaris_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_pres")

dvulgaris_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_bar_predictions(dvulgaris_predictions_mpa, diplodus[[4]])
# ggsave(filename = "figures/predictions/Dvulgaris_Dpuntazzo_mpa_nonspat.png", plot = last_plot(), dpi = 300, device = "png")



# D. sargus + D. vulgaris + MPA -------------------------------------------

# D. sargus preditions
dsargus_abs <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.vulgaris" = 0,
         across(all_of(c(c(diplodus[1], diplodus[2], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dsargus_max <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.vulgaris" = max(Diplodus.vulgaris),
         across(all_of(c(c(diplodus[1], diplodus[2], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dsargus_mpa_pred_mat <- bind_rows(dsargus_abs, dsargus_max)

dsargus_mpa_predict <- predict_MRF(dsargus_mpa_pred_mat, dip_mod) %>% 
  `colnames<-`(diplodus) %>% 
  as_data_frame() %>% 
  mutate(mpa = dsargus_mpa_pred_mat$mpa)

# Plot
abs_pred <- dsargus_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- dsargus_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_pres")

dsargus_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_bar_predictions(dsargus_predictions_mpa, diplodus[[3]])
# ggsave(filename = "figures/predictions/Dsargus_Dvulgaris_mpa_nonspat.png", plot = last_plot(), dpi = 300, device = "png")


# D. vulgaris prediction
dvulgaris_ds_abs <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.sargus" = 0,
         across(all_of(c(c(diplodus[1], diplodus[2], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dvulgaris_ds_max <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.sargus" = max(Diplodus.puntazzo),
         across(all_of(c(c(diplodus[1], diplodus[2], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dvulgaris_ds_mpa_pred_mat <- bind_rows(dvulgaris_ds_abs, dvulgaris_max)

dvulgaris_ds_mpa_predict <- predict_MRF(dvulgaris_ds_mpa_pred_mat, dip_mod) %>% 
  `colnames<-`(diplodus) %>% 
  as_data_frame() %>% 
  mutate(mpa = dvulgaris_ds_mpa_pred_mat$mpa)

# Plot
abs_pred <- dvulgaris_ds_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_abs")

pres_pred <- dvulgaris_ds_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_pres")

dvulgaris_ds_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_bar_predictions(dvulgaris_ds_predictions_mpa, diplodus[[4]])
# ggsave(filename = "figures/predictions/Dvulgaris_Dsargus_mpa_nonspat.png", plot = last_plot(), dpi = 300, device = "png")



# S. rivulatus + S. luridus -----------------------------------------------

# S rivulatus predition
sriv_temp_abs <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.luridus = 0,
         across(all_of(herbivores[3:4]), .funs = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

sriv_temp_max <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.luridus = max(Siganus.luridus),
         across(all_of(herbivores[3:4]), .fns = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

sriv_temp_pred_mat <- bind_rows(sriv_temp_abs, sriv_temp_max)


sriv_temp_predict <- predict_MRF(sriv_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = sriv_temp_pred_mat$temp)

# Plot
abs_pred <- sriv_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- sriv_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_pres")

sriv_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_predictions(sriv_predictions, herbivores[[1]])
# ggsave(filename = "figures/predictions/Srivulatus_Sluridus_temp_nonspat.png", plot = last_plot(), dpi = 300, device = "png")

# S rivulatus predition
sriv_temp_abs <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.luridus = 0,
         across(all_of(herbivores[3:4]), .funs = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

sriv_temp_max <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.luridus = max(Siganus.luridus),
         across(all_of(herbivores[3:4]), .fns = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

sriv_temp_pred_mat <- bind_rows(sriv_temp_abs, sriv_temp_max)


sriv_temp_predict <- predict_MRF(sriv_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = sriv_temp_pred_mat$temp)

# Plot
abs_pred <- sriv_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- sriv_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_pres")

sriv_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_predictions(sriv_predictions, herbivores[[1]])
# ggsave(filename = "figures/predictions/Srivulatus_Sluridus_temp_nonspat.png", plot = last_plot(), dpi = 300, device = "png")

# S rivulatus predition
sriv_temp_abs <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.luridus = 0,
         across(all_of(herbivores[3:4]), .funs = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

sriv_temp_max <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.luridus = max(Siganus.luridus),
         across(all_of(herbivores[3:4]), .fns = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

sriv_temp_pred_mat <- bind_rows(sriv_temp_abs, sriv_temp_max)


sriv_temp_predict <- predict_MRF(sriv_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = sriv_temp_pred_mat$temp)

# Plot
abs_pred <- sriv_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- sriv_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_pres")

sriv_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_predictions(sriv_predictions, herbivores[[1]])
# ggsave(filename = "figures/predictions/Srivulatus_Sluridus_temp_nonspat.png", plot = last_plot(), dpi = 300, device = "png")


# S luridus prediction
slurid_temp_abs <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.rivulatus = 0,
         across(all_of(herbivores[3:4]), .funs = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

slurid_temp_max <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.rivulatus = max(Siganus.rivulatus),
         across(all_of(herbivores[3:4]), .fns = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)


sriv_temp_pred_mat <- bind_rows(sriv_temp_abs, sriv_temp_max)


sriv_temp_predict <- predict_MRF(sriv_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = sriv_temp_pred_mat$temp)

# Plot
slurid_temp_pred_mat <- bind_rows(slurid_temp_abs, slurid_temp_max)


slurid_temp_predict <- predict_MRF(slurid_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = slurid_temp_pred_mat$temp)

# Plot
abs_pred <- slurid_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_abs")

pres_pred <- slurid_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_pres")


slurid_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:

plot_predictions(sriv_predictions, herbivores[[1]])
# ggsave(filename = "figures/predictions/Srivulatus_Sluridus_temp_nonspat.png", plot = last_plot(), dpi = 300, device = "png")

# S luridus prediction
slurid_temp_abs <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.rivulatus = 0,
         across(all_of(herbivores[3:4]), .funs = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

slurid_temp_max <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.rivulatus = max(Siganus.rivulatus),
         across(all_of(herbivores[3:4]), .fns = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

slurid_temp_pred_mat <- bind_rows(slurid_temp_abs, slurid_temp_max)


slurid_temp_predict <- predict_MRF(slurid_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = slurid_temp_pred_mat$temp)

# Plot
abs_pred <- slurid_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- slurid_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_pres")

slurid_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_predictions(slurid_predictions, herbivores[[2]])
# ggsave(filename = "figures/predictions/Sluridus_Srivulatus_temp_nonspat.png", plot = last_plot(), dpi = 300, device = "png")



# E. marginatus + M. rubra + Temp -----------------------------------------

# E. marginatus prediction
em_mr_temp_abs <- grps_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Mycteroperca.rubra = 0,
         across(all_of(c(c(groupers[1], groupers[4:5]))), .funs = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

em_mr_temp_max <- grps_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Mycteroperca.rubra = max(Mycteroperca.rubra),
         across(all_of(c(c(groupers[1], groupers[4:5]))), .fns = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

em_mr_temp_pred_mat <- bind_rows(em_mr_temp_abs, em_mr_temp_max)


em_mr_temp_predict <- predict_MRF(em_mr_temp_pred_mat, grps_mod) %>% 
  `colnames<-`(groupers) %>% 
  as_data_frame() %>% 
  mutate(temp = em_mr_temp_pred_mat$temp)

# Plot
abs_pred <- em_mr_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- em_mr_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_pres")

em_mr_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_predictions(em_mr_predictions, groupers[[2]])
# ggsave(filename = "figures/predictions/Emargin_Mrubra_temp_nonspat.png", plot = last_plot(), dpi = 300, device = "png")

# M rubra predcition
mr_em_temp_abs <- grps_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Epinephelus.marginatus = 0,
         across(all_of(c(c(groupers[1], groupers[4:5]))), .funs = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

mr_em_temp_max <- grps_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Epinephelus.marginatus = max(Epinephelus.marginatus),
         across(all_of(c(c(groupers[1], groupers[4:5]))), .fns = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

mr_em_temp_pred_mat <- bind_rows(mr_em_temp_abs, mr_em_temp_max)


mr_em_temp_predict <- predict_MRF(mr_em_temp_pred_mat, grps_mod) %>% 
  `colnames<-`(groupers) %>% 
  as_data_frame() %>% 
  mutate(temp = mr_em_temp_pred_mat$temp)

# Plot
abs_pred <- mr_em_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- mr_em_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_pres")

mr_em_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_predictions(mr_em_predictions, groupers[[3]])
# ggsave(filename = "figures/predictions/Mrubra_Emargin_temp_nonspat.png", plot = last_plot(), dpi = 300, device = "png")
