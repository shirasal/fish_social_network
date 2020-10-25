library(tidyverse)
library(MRFcov)
load("data/all_objects.RData")
med_raw <- read_rds("data/medata.Rds") %>% ungroup()

med_biomass <- med_raw %>% 
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         prod = scale(pp_mean),
         biomass = a*sp.length^b)

create_biomass_mat <- function(dataset, taxa, covariate){
  cols <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)
  dataset %>% 
    group_by_at(.vars = cols) %>% # group for summarise
    summarise(n = mean(biomass)) %>% # sum sp.n for each grouped variable
    spread(species, n, fill = 0) %>% # convert to species matrix
    ungroup() %>% 
    na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
    mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which should be unique
    group_by(loc) %>% 
    column_to_rownames("loc") %>% # create row names by location
    select(all_of(taxa), all_of(covariate)) # keep the species and covariates columns
}


med_bioms_coords <- med_biomass %>% 
  distinct(site, trans, lat, lon) %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc") %>% 
  select(lon, lat)


grps_biomat <- create_biomass_mat(dataset = med_biomass,
                           taxa = groupers,
                           covariate = c("mpa", "temp", "depth", "prod"))
grps_coords <- med_bioms_coords %>% 
  filter(rownames(med_bioms_coords) %in% rownames(grps_biomat))
nrow(grps_biomat) == nrow(grps_coords)

grps_bioms_mod <- MRFcov_spatial(grps_biomat, prep_covariates = TRUE, n_nodes = 5,
                                 n_covariates = 4, family = "gaussian", coords = grps_coords)

plotMRF_hm(grps_bioms_mod, main = "Groupers associations (biomass model)")

## SEABREAM (Diplodus species)
dip_biomat <- create_biomass_mat(dataset = med_biomass, taxa = diplodus, covariate = c("mpa", "temp", "depth", "prod"))
dip_coords <- med_bioms_coords %>% 
  filter(rownames(med_bioms_coords) %in% rownames(dip_biomat))
nrow(dip_biomat) == nrow(dip_coords)

dip_bioms_mod <- MRFcov_spatial(dip_biomat, prep_covariates = TRUE, n_nodes = 5,
                                 n_covariates = 4, family = "gaussian", coords = dip_coords)

plotMRF_hm(dip_bioms_mod, main = "Seabream associations (biomass model)")

## HERBIVORES
herb_biomat <- create_biomass_mat(dataset = med_biomass,
                                  taxa = herbivores,
                                  covariate = c("mpa", "temp", "depth", "prod"))
herb_coords <- med_bioms_coords %>% 
  filter(rownames(med_bioms_coords) %in% rownames(herb_biomat))
nrow(herb_biomat) == nrow(herb_coords)

herb_bioms_mod <- MRFcov_spatial(herb_biomat, prep_covariates = TRUE, n_nodes = 4,
                                 n_covariates = 4, family = "gaussian", coords = herb_coords)

plotMRF_hm(herb_bioms_mod, main = "Herbivores associations (biomass model)")



