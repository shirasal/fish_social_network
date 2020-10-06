# source("R/packages.R")
# load("data/all_objects.RData")
# source("R/run_models_spatial.R")


# Groupers ----------------------------------------------------------------

##### TEMP

grps_abs_temp <- grps_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(groupers), function(x) 0)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

grps_max_temp <- grps_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(groupers), .fns = max)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

grps_temp_pred_mat <- bind_rows(grps_abs_temp, grps_max_temp)
rm(list = c("grps_abs_temp", "grps_max_temp"))

grps_temp_predict <- predict_MRF(grps_temp_pred_mat, grps_spat) %>% 
  `colnames<-`(groupers) %>% 
  `row.names<-`(grps_temp_pred_mat$temp)

##### MPA

grps_abs_mpa <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(groupers), function(x) 0)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

grps_max_mpa <- grps_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(groupers), .fns = max)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

grps_mpa_pred_mat <- bind_rows(grps_abs_mpa, grps_max_mpa)
rm(list = c("grps_abs_mpa", "grps_max_mpa"))

grps_mpa_predict <- predict_MRF(grps_mpa_pred_mat, grps_spat) %>% 
  `colnames<-`(groupers) %>% 
  `row.names<-`(grps_mpa_pred_mat$mpa)



# Diplodus ----------------------------------------------------------------

##### TEMP

dip_abs_temp <- dip_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(diplodus), function(x) 0)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

dip_max_temp <- dip_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(diplodus), .fns = max)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

dip_temp_pred_mat <- bind_rows(dip_abs_temp, dip_max_temp)
rm(list = c("dip_abs_temp", "dip_max_temp"))

dip_temp_predict <- predict_MRF(dip_temp_pred_mat, dip_spat) %>% 
  `colnames<-`(diplodus) %>% 
  `row.names<-`(dip_temp_pred_mat$temp)


##### MPA

dip_abs_mpa <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(diplodus), function(x) 0)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

dip_max_mpa <- dip_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(diplodus), .fns = max)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

dip_mpa_pred_mat <- bind_rows(dip_abs_mpa, dip_max_mpa)
rm(list = c("dip_abs_mpa", "dip_max_mpa"))

dip_mpa_predict <- predict_MRF(dip_mpa_pred_mat, dip_spat) %>% 
  `colnames<-`(diplodus) %>% 
  `row.names<-`(dip_mpa_pred_mat$mpa)

# Herbivores --------------------------------------------------------------

##### TEMP

herb_abs_temp <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(herbivores), function(x) 0)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

herb_max_temp <- herb_mat %>% 
  mutate(depth = median(depth),
         prod = median(prod),
         mpa = TRUE,
         across(all_of(herbivores), .fns = max)) %>% 
  group_by(temp = round(temp, digits = 1)) %>% 
  sample_n(1)

herb_temp_pred_mat <- bind_rows(herb_abs_temp, herb_max_temp)
rm(list = c("herb_abs_temp", "herb_max_temp"))

herb_temp_predict <- predict_MRF(herb_temp_pred_mat, herb_spat) %>% 
  `colnames<-`(herbivores) %>% 
  `row.names<-`(herb_temp_pred_mat$temp)


##### MPA

herb_abs_mpa <- herb_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(herbivores), function(x) 0)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

herb_max_mpa <- herb_mat %>% 
  mutate(temp = median(temp),
         depth = median(depth),
         prod = median(prod),
         across(all_of(herbivores), .fns = max)) %>% 
  group_by(mpa) %>% 
  sample_n(1)

herb_mpa_pred_mat <- bind_rows(herb_abs_mpa, herb_max_mpa)
rm(list = c("herb_abs_mpa", "herb_max_mpa"))

herb_mpa_predict <- predict_MRF(herb_mpa_pred_mat, herb_spat) %>% 
  `colnames<-`(herbivores) %>% 
  `row.names<-`(herb_mpa_pred_mat$mpa)


# Results -----------------------------------------------------------------

grps_temp_predict
grps_mpa_predict
grps_temp_abs <- grps_temp_predict %>% as_data_frame() %>% slice_head(prop = 0.5)
grps_temp_pre <- grps_temp_predict %>% as_data_frame() %>% slice_tail(prop = 0.5)

