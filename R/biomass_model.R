
med_biomass <- med_raw %>% 
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         sal = scale(sal_mean),
         prod = scale(pp_mean),
         biomass = a*sp.length^b)

biomass_mat <- med_biomass %>%
  group_by_at(.vars = c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)) %>% # group for summarise
  summarise(mean_biomass = mean(biomass)) %>% # mean biomass for each grouped variable 
  spread(species, mean_biomass, fill = 0) %>% # convert to species matrix
  ungroup() %>% 
  na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
  mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which shpuld be unique
  group_by(loc) %>% 
  column_to_rownames("loc") %>% # create row names by location
  select(all_of(groupers), all_of(diplodus), all_of(herbivores), all_of(env_vector), all_of(anthro_vector)) %>%  # keep the species and covariates columns
  ungroup()

# biomass_coords <- biomass_mat %>% rownames_to_column() %>% 

grps_biomat <- biomass_mat %>% 
  select(all_of(groupers), all_of(env_vector), all_of(anthro_vector))

grps_bioms_mod <- MRFcov_spatial(grps_biomat, prep_covariates = TRUE, n_nodes = 5, n_covariates = 4, family = "gaussian", coords = grps_coords)

