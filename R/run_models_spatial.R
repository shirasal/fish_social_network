med_coords <- med_clean %>% 
  distinct(site, trans, lat, lon) %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc") %>% 
  select(lon, lat)


grps_coords <- med_coords %>% 
  filter(rownames(med_coords) %in% rownames(grps_mat))

nrow(grps_mat) == nrow(grps_coords)

grps_spat <- MRFcov_spatial(grps_mat, n_nodes = 5, n_covariates = 4, family = "gaussian", coords = grps_coords)
