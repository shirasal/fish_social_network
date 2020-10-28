# source("R/packages.R")
# load("data/all_objects.RData")
source("R/run_models.R")
# file.edit("R/rel_imp.R")
load("figures/predictions/legend_predictions.rdata")

# Groupers ----------------------------------------------------------------

##### TEMP

grps_temp_abs <- grps_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(groupers), function(x) 0)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

grps_temp_max <- grps_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(groupers), .fns = max)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

grps_temp_pred_mat <- bind_rows(grps_temp_abs, grps_temp_max)

grps_temp_predict <- predict_MRF(grps_temp_pred_mat, grps_mod) %>% 
  `colnames<-`(groupers) %>% 
  as_data_frame() %>% 
  mutate(temp = grps_temp_pred_mat$temp)

# Plot
abs_pred <- grps_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- grps_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_pres")

grps_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
grps_temp_plots <- list()
for(i in groupers) { # for each species in this array
  grps_temp_plots[[i]] <- plot_predictions(grps_predictions, i)
  ggsave(filename = str_glue("figures/predictions/{i}_temp_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")
}

grps_temp_plots[[4]]


grps_temp_grid_plots <- list()
for(i in groupers) { # for each species in this array
  grps_temp_grid_plots[[i]] <- plot_temp_preds_grid(grps_predictions, i)
}

ggpubr::ggarrange(plotlist = grps_temp_grid_plots) %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Observation predictions", face = "bold", size = 14),
                          bottom = ggpubr::text_grob("Temperature (scaled)", size = 10),
                          left = ggpubr::text_grob("Prediction", size = 10, rot = 90)) %>%
  ggsave(filename = "figures/predictions/grps_temp_nonspat.png", 
         dpi = 300, device = "png", width = 20, height = 10, units = "cm")

##### MPA

grps_mpa_abs <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(groupers), function(x) 0)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

grps_mpa_max <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(groupers), .fns = max)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

grps_mpa_pred_mat <- bind_rows(grps_mpa_abs, grps_mpa_max)


grps_temp_predict <- predict_MRF(grps_mpa_pred_mat, grps_mod) %>% 
  `colnames<-`(groupers) %>% 
  as_data_frame() %>% 
  mutate(mpa = grps_mpa_pred_mat$mpa)

# Plot
abs_pred <- grps_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- grps_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(groupers),
               names_to = "species",
               values_to = "pred_pres")

grps_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
grps_mpa_plots <- list()
for(i in groupers) { # for each species in this array
  grps_mpa_plots[[i]] <- plot_bar_predictions(grps_predictions_mpa, i)
  ggsave(filename = str_glue("figures/predictions/{i}_mpa_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")
}

grps_mpa_grid_plots <- list()
for(i in groupers) { # for each species in this array
  grps_mpa_grid_plots[[i]] <- plot_mpa_preds_grid(grps_predictions_mpa, i)
}

ggpubr::ggarrange(plotlist = grps_mpa_grid_plots) %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Observation predictions", face = "bold", size = 14),
                          bottom = ggpubr::text_grob("MPA", size = 10),
                          left = ggpubr::text_grob("Prediction", size = 10, rot = 90)) %>%
  ggsave(filename = "figures/predictions/grps_mpa_nonspat.png", 
         dpi = 300, device = "png", width = 20, height = 10, units = "cm")


# Diplodus ----------------------------------------------------------------

##### TEMP

dip_temp_abs <- dip_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(diplodus), function(x) 0)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

dip_temp_max <- dip_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(diplodus), .fns = max)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

dip_temp_pred_mat <- bind_rows(dip_temp_abs, dip_temp_max)


dip_temp_predict <- predict_MRF(dip_temp_pred_mat, dip_mod) %>% 
  `colnames<-`(diplodus) %>% 
  as_data_frame() %>% 
  mutate(temp = dip_temp_pred_mat$temp)

# Plot
abs_pred <- dip_temp_predict %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- dip_temp_predict %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_pres")

dip_predictions <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
dip_temp_plots <- list()
for(i in diplodus) { # for each species in this array
  dip_temp_plots[[i]] <- plot_predictions(dip_predictions, i)
  ggsave(filename = str_glue("figures/predictions/{i}_temp_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")
}

dip_temp_grid_plots <- list()
for(i in diplodus) { # for each species in this array
  dip_temp_grid_plots[[i]] <- plot_temp_preds_grid(dip_predictions, i)
}

ggpubr::ggarrange(plotlist = dip_temp_grid_plots) %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Observation predictions", face = "bold", size = 14),
                          bottom = ggpubr::text_grob("Temperature (scaled)", size = 10),
                          left = ggpubr::text_grob("Prediction", size = 10, rot = 90)) %>%
  ggsave(filename = "figures/predictions/dip_temp_nonspat.png", 
         dpi = 300, device = "png", width = 20, height = 10, units = "cm")

##### MPA

dip_mpa_abs <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(diplodus), function(x) 0)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

dip_mpa_max <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(diplodus), .fns = max)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

dip_mpa_pred_mat <- bind_rows(dip_mpa_abs, dip_mpa_max)


dip_temp_predict <- predict_MRF(dip_mpa_pred_mat, dip_mod) %>% 
  `colnames<-`(diplodus) %>% 
  as_data_frame() %>% 
  mutate(mpa = dip_mpa_pred_mat$mpa)

# Plot
abs_pred <- dip_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- dip_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(diplodus),
               names_to = "species",
               values_to = "pred_pres")

dip_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
dip_mpa_plots <- list()
for(i in diplodus) { # for each species in this array
  dip_mpa_plots[[i]] <- plot_bar_predictions(dip_predictions_mpa, i)
  ggsave(filename = str_glue("figures/predictions/{i}_mpa_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")
}


dip_mpa_grid_plots <- list()
for(i in diplodus) { # for each species in this array
  dip_mpa_grid_plots[[i]] <- plot_mpa_preds_grid(dip_predictions_mpa, i)
}

ggpubr::ggarrange(plotlist = dip_mpa_grid_plots) %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Observation predictions", face = "bold", size = 14),
                          bottom = ggpubr::text_grob("MPA", size = 10),
                          left = ggpubr::text_grob("Prediction", size = 10, rot = 90)) %>%
  ggsave(filename = "figures/predictions/dip_mpa_nonspat.png", 
         dpi = 300, device = "png", width = 20, height = 10, units = "cm")

# Herbivores --------------------------------------------------------------

##### TEMP

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
  ggsave(filename = str_glue("figures/predictions/{i}_temp_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")
}

herb_temp_grid_plots <- list()
for(i in herbivores) { # for each species in this array
  herb_temp_grid_plots[[i]] <- plot_temp_preds_grid(herb_predictions, i)
}

ggpubr::ggarrange(plotlist = herb_temp_grid_plots, 
                  common.legend = TRUE, legend.grob = leg, legend = "right") %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Observation predictions", face = "bold", size = 14),
                          bottom = ggpubr::text_grob("Temperature (scaled)", size = 10),
                          left = ggpubr::text_grob("Predicted observations", size = 10, rot = 90)) %>%
  ggsave(filename = "figures/predictions/herb_temp_nonspat.png", 
         dpi = 300, device = "png", width = 20, height = 10, units = "cm")

##### MPA

herb_mpa_abs <- herb_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(herbivores), function(x) 0)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

herb_mpa_max <- herb_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(herbivores), .fns = max)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

herb_mpa_pred_mat <- bind_rows(herb_mpa_abs, herb_mpa_max)


herb_temp_predict <- predict_MRF(herb_mpa_pred_mat, herb_mod) %>% 
  `colnames<-`(herbivores) %>% 
  as_data_frame() %>% 
  mutate(mpa = herb_mpa_pred_mat$mpa)

# Plot
abs_pred <- herb_mpa_pred_mat %>% 
  dplyr::slice_head(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_abs")
pres_pred <- herb_mpa_pred_mat %>% 
  dplyr::slice_tail(prop = 0.5) %>% 
  pivot_longer(all_of(herbivores),
               names_to = "species",
               values_to = "pred_pres")

herb_predictions_mpa <- abs_pred %>% left_join(pres_pred) %>% 
  pivot_longer(cols = pred_abs:pred_pres,
               names_to = "model",
               values_to = "prediction")

# Plot the predictions:
herb_mpa_plots <- list()
for(i in herbivores) { # for each species in this array
  herb_mpa_plots[[i]] <- plot_bar_predictions(herb_predictions_mpa, i)
  ggsave(filename = str_glue("figures/predictions/{i}_mpa_nonspat.png"), plot = last_plot(), dpi = 300, device = "png")
}


herb_mpa_grid_plots <- list()
for(i in herbivores) { # for each species in this array
  herb_mpa_grid_plots[[i]] <- plot_mpa_preds_grid(herb_predictions_mpa, i)
}

ggpubr::ggarrange(plotlist = herb_mpa_grid_plots, 
                  common.legend = TRUE, legend.grob = leg, legend = "right") %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Observation predictions", face = "bold", size = 14),
                          bottom = ggpubr::text_grob("MPA", size = 10),
                          left = ggpubr::text_grob("Predicted observations", size = 10, rot = 90)) %>%
  ggsave(filename = "figures/predictions/herb_mpa_nonspat.png", 
         dpi = 300, device = "png", width = 20, height = 10, units = "cm")
