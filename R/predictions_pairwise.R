source("R/packages.R")
load("data/all_objects.RData")
source("R/run_models.R")
source("R/functions.R")


# E. costae + E. marginatus + MPA -----------------------------------------

# E. costae prediction
ecostae_abs <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Epinephelus.marginatus" = 0,
         across(all_of(groupers[3:5]), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

ecostae_max <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Epinephelus.marginatus" = max(Epinephelus.marginatus),
         across(all_of(groupers[3:5]), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)


ecostae_mpa_pred_mat <- bind_rows(ecostae_abs, ecostae_max)


ecostae_mpa_predict <- predict_MRF(ecostae_mpa_pred_mat, grps_mod) %>% 
  `colnames<-`(groupers) %>% 
  as_data_frame() %>% 
  mutate(mpa = ecostae_mpa_pred_mat$mpa)

# Plot
abs_pred <- ecostae_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- ecostae_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_pres")

ecostae_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_bar_predictions(ecostae_predictions_mpa, groupers[[1]])
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
# ggsave(filename = str_glue("figures/predictions/Dpuntazzo_Dvulgaris_mpa_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")


# D. vulgaris prediction
dvulgaris_abs <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Diplodus.puntazzo" = 0,
         across(all_of(c(c(diplodus[1], diplodus[3], diplodus[5]))), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

dpuntazzo_max <- dip_mat %>% 
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
# ggsave(filename = str_glue("figures/predictions/Dvulgaris_Dpuntazzo_mpa_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")


# D. sargus + D. vulgaris + MPA -------------------------------------------

# E. costae prediction
ecostae_abs <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Epinephelus.marginatus" = 0,
         across(all_of(groupers[3:5]), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)

ecostae_max <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         "Epinephelus.marginatus" = max(Epinephelus.marginatus),
         across(all_of(groupers[3:5]), .funs = mean)) %>% 
  group_by(mpa) %>% 
  sample_n(10)


ecostae_mpa_pred_mat <- bind_rows(ecostae_abs, ecostae_max)


ecostae_mpa_predict <- predict_MRF(ecostae_mpa_pred_mat, grps_mod) %>% 
  `colnames<-`(groupers) %>% 
  as_data_frame() %>% 
  mutate(mpa = ecostae_mpa_pred_mat$mpa)

# Plot
abs_pred <- ecostae_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- ecostae_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_pres")

ecostae_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
plot_bar_predictions(ecostae_predictions_mpa, groupers[[1]])
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



# S. rivulatus + S. luridus -----------------------------------------------

herb_temp_abs <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(herbivores), function(x) 0)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

herb_temp_max <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(herbivores), .fns = max)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

herb_temp_pred_mat <- bind_rows(herb_temp_abs, herb_temp_max)


herb_temp_predict <- predict_MRF(herb_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = herb_temp_pred_mat$temp)

# Plot
abs_pred <- herb_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- herb_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_pres")

herb_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
herb_temp_plots <- list()
for(i in herbivores) { # for each species in this array
  herb_temp_plots[[i]] <- plot_predictions(herb_predictions, i)
  # ggsave(filename = str_glue("figures/predictions/{i}_temp_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")
}

