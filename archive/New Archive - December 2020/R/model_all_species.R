source("R/packages.R")
load("data/all_objects.RData")
source("R/functions.R")

med_coords <- med_clean %>% 
  distinct(site, trans, lat, lon) %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc") %>% 
  select(lon, lat)

covariates <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)

all_spp_mat <- med_clean %>%
  group_by_at(.vars = covariates) %>%
  summarise(n = sum(sp.n)) %>%
  filter(n > 10) %>% 
  spread(species, n, fill = 0) %>%
  ungroup() %>%
  na.omit() %>%
  mutate(loc = paste(site, trans)) %>%
  group_by(loc) %>%
  column_to_rownames("loc") %>% 
  select(contains("."), all_of(env_vector), all_of(anthro_vector)) %>% 
  ungroup()

med_mod <- MRFcov_spatial(all_spp_mat, n_nodes = 50, n_covariates = 4, family = "poisson", n_cores = 4, coords = med_coords)

med_mod_nonspat <- MRFcov(all_spp_mat, n_nodes = 50, n_covariates = 4, family = "poisson", n_cores = 4)
