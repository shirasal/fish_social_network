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
<<<<<<< HEAD
# 
# 
# # D. sargus + D. vulgaris + MPA -------------------------------------------

=======


# D. sargus + D. vulgaris + MPA -------------------------------------------

>>>>>>> 64a69a7e3288418c6ee7a5b463c2021f08d99b60
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
<<<<<<< HEAD
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.luridus = 0,
=======
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
>>>>>>> 64a69a7e3288418c6ee7a5b463c2021f08d99b60
         across(all_of(herbivores[3:4]), .funs = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

<<<<<<< HEAD
sriv_temp_max <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.luridus = max(Siganus.luridus),
=======
slurid_temp_max <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         Siganus.rivulatus = max(Siganus.rivulatus),
>>>>>>> 64a69a7e3288418c6ee7a5b463c2021f08d99b60
         across(all_of(herbivores[3:4]), .fns = mean)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

<<<<<<< HEAD
sriv_temp_pred_mat <- bind_rows(sriv_temp_abs, sriv_temp_max)


sriv_temp_predict <- predict_MRF(sriv_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = sriv_temp_pred_mat$temp)

# Plot
abs_pred <- sriv_temp_predict %>% 
=======
slurid_temp_pred_mat <- bind_rows(slurid_temp_abs, slurid_temp_max)


slurid_temp_predict <- predict_MRF(slurid_temp_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(temp = slurid_temp_pred_mat$temp)

# Plot
abs_pred <- slurid_temp_predict %>% 
>>>>>>> 64a69a7e3288418c6ee7a5b463c2021f08d99b60
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_abs")
<<<<<<< HEAD
pres_pred <- sriv_temp_predict %>% 
=======
pres_pred <- slurid_temp_predict %>% 
>>>>>>> 64a69a7e3288418c6ee7a5b463c2021f08d99b60
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_pres")

<<<<<<< HEAD
sriv_predictions <- abs_pred %>% left_join(pres_pred) %>% 
=======
slurid_predictions <- abs_pred %>% left_join(pres_pred) %>% 
>>>>>>> 64a69a7e3288418c6ee7a5b463c2021f08d99b60
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
<<<<<<< HEAD
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
=======
plot_predictions(slurid_predictions, herbivores[[2]])
# ggsave(filename = "figures/predictions/Sluridus_Srivulatus_temp_nonspat.png", plot = last_plot(), dpi = 300, device = "png")
>>>>>>> 64a69a7e3288418c6ee7a5b463c2021f08d99b60

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
