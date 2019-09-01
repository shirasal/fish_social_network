# Required packcages:
library(tidyverse)
library(MRFcov)

med_raw <- read_csv("med_raw.csv")
med_raw$depth <- as.numeric(med_raw$depth) # fix class issue (instead of logic, as is has been parsed)

##### ==== CREATE SPECIES MATRIX from SUBSETTED DATA ==== #####
## Create metadata tibbles:
## 1. Temperature:
med_temp_mean <- med_raw %>% 
  distinct(site, lon, lat, tmean)

## 2. Salinity:
med_sal_mean <- med_raw %>% 
  distinct(site, lon, lat, sal_mean)

## 3. Primary production:
med_pp_mean <- med_raw %>% 
  distinct(site, lon, lat, pp_mean)

## 4. All together:
med_meta_env <- left_join(x = med_temp_mean, y = med_sal_mean, by = c("site", "lon", "lat")) %>% 
  left_join(., med_pp_mean, by = c("site", "lon", "lat"))

## sub1 = species matrix where the count of fish > 10
med_mat_sub1 <- med_raw %>%
  group_by(site, lon, lat, species) %>%
  filter(sp.n > 10) %>% 
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
head(med_mat_sub1)

## Add environmental (meta) data
full_med_mat_sub1 <- left_join(med_mat_sub1, med_meta_env, by = c("site", "lon", "lat"))
View(full_med_mat_sub1)

# Preps for MRFcov analysis: 1. remove NAs
full_med_mat_sub1 <- full_med_mat_sub1 %>% filter(complete.cases(.))

# Preps for MRFcov analysis: 2. Sites to rownames
full_med_mat_sub1 <- as.data.frame(full_med_mat_sub1)
rownames(full_med_mat_sub1) <- make.unique(full_med_mat_sub1$site, sep = "_")
full_med_mat_sub1 <-  full_med_mat_sub1 %>% select(-c("site", "lon", "lat"))
View(full_med_mat_sub1)

## Convert the abundance matrix to presence-absence matrix
# pres_abs_mat_sub1 <- full_med_mat_sub1 %>% 
# pres_abs_mat_sub1[17:ncol(pres_abs_mat_sub1)] <- ifelse(pres_abs_mat_sub1[17:ncol(pres_abs_mat_sub1)] > 0, 1, 0)
# View(pres_abs_mat_sub1)

MRF_sub1 <- MRFcov(data = full_med_mat_sub1, n_nodes = 30, n_covariates = 3, family = "gaussian")
MRF_sub1_predict <- predict_MRF(data = full_med_mat_sub1, MRF_mod = MRF_sub1)
MRF_sub1_predict