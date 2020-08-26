
med_biomass <- med_raw %>% 
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         sal = scale(sal_mean),
         prod = scale(pp_mean),
         biomass = a*sp.length^b)

biomass_mat <- med_biomass %>%
  group_by_at(.vars = c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)) %>% # group for summarise
  summarise() %>% # sum biomass for each grouped variable <--------------- needs additional args
  spread(species, n, fill = 0) %>% # convert to species matrix
  ungroup() %>% 
  na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
  mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which shpuld be unique
  group_by(loc) %>% 
  column_to_rownames("loc") %>% # create row names by location
  select(all_of(groupers, diplodus, herbivores), all_of(env_vector, anthro_vector)) # keep the species and covariates columns

