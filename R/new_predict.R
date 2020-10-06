
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



grps_temp_predict <- predict_MRF(grps_temp_pred_mat, grps_spat) %>% 
  `colnames<-`(groupers) %>% 
  `row.names<-`(grps_temp_pred_mat$temp)




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

grps_mpa_predict <- predict_MRF(grps_abs_mpa, grps_spat) %>% 
  `colnames<-`(groupers) %>% 
  `row.names<-`(unique(grps_mpa_pred_mat$mpa))





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



dip_temp_predict <- predict_MRF(dip_temp_pred_mat, dip_spat) %>% 
  `colnames<-`(diplodus) %>% 
  `row.names<-`(dip_temp_pred_mat$temp)




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

dip_mpa_predict <- predict_MRF(dip_abs_mpa, dip_spat) %>% 
  `colnames<-`(diplodus) %>% 
  `row.names<-`(unique(dip_mpa_pred_mat$mpa))

