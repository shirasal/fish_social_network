source("R/run_models.R")

med_data <- med_clean %>% 
  mutate(abund_std = scale(sp.n))

cols <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)
grps_std <- med_data %>% 
  group_by_at(.vars = cols) %>% # group for summarise
  summarise(n = sum(abund_std)) %>% # sum sp.n for each grouped variable
  spread(species, n, fill = 0) %>% # convert to species matrix
  ungroup() %>% 
  na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
  mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which should be unique
  group_by(loc) %>% 
  column_to_rownames("loc") %>% # create row names by location
  select(all_of(groupers), mpa, temp, depth, prod)

grps_mod_std <- MRFcov(grps_std, n_nodes = 5, family = "gaussian")
lapply(grps_mod_std$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

dip_std <- med_data %>% 
  group_by_at(.vars = cols) %>% # group for summarise
  summarise(n = sum(abund_std)) %>% # sum sp.n for each grouped variable
  spread(species, n, fill = 0) %>% # convert to species matrix
  ungroup() %>% 
  na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
  mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which should be unique
  group_by(loc) %>% 
  column_to_rownames("loc") %>% # create row names by location
  select(all_of(diplodus), mpa, temp, depth, prod)

dip_mod_std <- MRFcov(dip_std, n_nodes = 5, family = "gaussian")
lapply(dip_mod_std$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

vis_temp_pred_pair(species_i = "Diplodus.annularis", species_j = "Diplodus.vulgaris", 
                  spp_mat = dip_std, spp_mod = dip_mod_std, guild = diplodus, n_spp = 5)# %>% 
  # ggsave(filename = "figures/predictions/D_annularis-D_vulgaris--TEMP_SCALED.png", device = "png")

herb_std <- med_data %>% 
  group_by_at(.vars = cols) %>% # group for summarise
  summarise(n = sum(abund_std)) %>% # sum sp.n for each grouped variable
  spread(species, n, fill = 0) %>% # convert to species matrix
  ungroup() %>% 
  na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
  mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which should be unique
  group_by(loc) %>% 
  column_to_rownames("loc") %>% # create row names by location
  select(all_of(herbivores), mpa, temp, depth, prod)

herb_mod_std <- MRFcov(herb_std, n_nodes = 4, family = "gaussian")
lapply(herb_mod_std$key_coefs, function(x) x %>% 
         filter(Rel_importance > 0.1) %>% 
         filter(str_detect(string = Variable, pattern = "_")))

vis_mpa_pred_pair(species_i = "Sparisoma.cretense", species_j = "Siganus.luridus", 
                  spp_mat = herb_std, spp_mod = herb_mod_std, guild = herbivores, n_spp = 4)

vis_temp_pred_pair("Sparisoma.cretense", "Sarpa.salpa", herb_std, herb_mod_std, herbivores, 4)
