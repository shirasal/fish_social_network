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
full_med_mat_sub1 <- na.omit(full_med_mat_sub1)
unique(is.na(full_med_mat_sub1)) # check there are no NAs

# Preps for MRFcov analysis: 2. Sites to rownames
full_med_mat_sub1 <- as.data.frame(full_med_mat_sub1) # required for changing row names
rownames(full_med_mat_sub1) <- make.unique(full_med_mat_sub1$site, sep = "_")
full_med_mat_sub1 <-  full_med_mat_sub1 %>% select(-c("site", "lon", "lat"))
View(full_med_mat_sub1)

# Convert the abundance matrix to presence-absence matrix
pres_abs_mat_sub1 <- full_med_mat_sub1
pres_abs_mat_sub1[1:54] <- ifelse(pres_abs_mat_sub1[1:54] > 0, 1, 0)
View(pres_abs_mat_sub1)


MRF_sub1 <- MRFcov(data = full_med_mat_sub1, n_nodes = 54, n_covariates = 3, family = "gaussian")
MRF_PA_sub1 <- MRFcov(data = pres_abs_mat_sub1, n_nodes = 54, n_covariates = 3, family = "binomial")
plotMRF_hm(MRF_sub1)
# cv_MRF_diag(data = full_med_mat_sub1, n_nodes = 54, n_covariates = 3, family = "gaussian")

##############################################################################
## Subset 2: by season
# Summer:
med_mat_sub2_su <- med_raw %>%
  filter(season == "Summer") %>% 
  group_by(site, lon, lat, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
full_med_mat_sub2_su <- left_join(med_mat_sub2_su, med_meta_env, by = c("site", "lon", "lat")) %>%
  na.omit(full_med_mat_sub2_su)
full_med_mat_sub2_su <- as.data.frame(full_med_mat_sub2_su) # required for changing row names
rownames(full_med_mat_sub2_su) <- make.unique(full_med_mat_sub2_su$site, sep = "_")
full_med_mat_sub2_su <-  full_med_mat_sub2_su %>% select(-c("site", "lon", "lat"))
pres_abs_mat_sub2_su <- full_med_mat_sub2_su
pres_abs_mat_sub2_su[1:54] <- ifelse(pres_abs_mat_sub2_su[1:54] > 0, 1, 0)
MRF_sub2_su <- MRFcov(data = full_med_mat_sub2_su, n_nodes = 54, n_covariates = 3,
                      family = "gaussian")
MRF_PA_sub2_su <- MRFcov(data = pres_abs_mat_sub2_su, n_nodes = 54, n_covariates = 3,
                         family = "binomial")

# Autumn:
med_mat_sub2_au <- med_raw %>%
  filter(season == "Autumn") %>% 
  group_by(site, lon, lat, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
full_med_mat_sub2_au <- left_join(med_mat_sub2_au, med_meta_env, by = c("site", "lon", "lat")) %>%
  na.omit(full_med_mat_sub2_au)
full_med_mat_sub2_au <- as.data.frame(full_med_mat_sub2_au) # required for changing row names
rownames(full_med_mat_sub2_au) <- make.unique(full_med_mat_sub2_au$site, sep = "_")
full_med_mat_sub2_au <-  full_med_mat_sub2_au %>% select(-c("site", "lon", "lat"))
pres_abs_mat_sub2_au <- full_med_mat_sub2_au
pres_abs_mat_sub2_au[1:54] <- ifelse(pres_abs_mat_sub2_au[1:54] > 0, 1, 0)
MRF_sub2_au <- MRFcov(data = full_med_mat_sub2_au, n_nodes = 54, n_covariates = 3,
                      family = "gaussian")
MRF_PA_sub2_au <- MRFcov(data = pres_abs_mat_sub2_au, n_nodes = 54, n_covariates = 3,
                         family = "binomial")

# Spring:
med_mat_sub2_sp <- med_raw %>%
  filter(season == "Spring") %>% 
  group_by(site, lon, lat, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
full_med_mat_sub2_sp <- left_join(med_mat_sub2_sp, med_meta_env, by = c("site", "lon", "lat")) %>%
  na.omit(full_med_mat_sub2_sp)
full_med_mat_sub2_sp <- as.data.frame(full_med_mat_sub2_sp) # required for changing row names
rownames(full_med_mat_sub2_sp) <- make.unique(full_med_mat_sub2_sp$site, sep = "_")
full_med_mat_sub2_sp <-  full_med_mat_sub2_sp %>% select(-c("site", "lon", "lat"))
pres_abs_mat_sub2_sp <- full_med_mat_sub2_sp
pres_abs_mat_sub2_sp[1:54] <- ifelse(pres_abs_mat_sub2_sp[1:54] > 0, 1, 0)
MRF_sub2_sp <- MRFcov(data = full_med_mat_sub2_sp, n_nodes = 54, n_covariates = 3,
                      family = "gaussian")
MRF_PA_sub2_sp <- MRFcov(data = pres_abs_mat_sub2_sp, n_nodes = 54, n_covariates = 3,
                         family = "binomial")

plotMRF_hm(MRF_sub2_su, main =  "Estimated co-occurrence summer")
plotMRF_hm(MRF_sub2_au, main =  "Estimated co-occurrence autumn")
plotMRF_hm(MRF_sub2_sp, main =  "Estimated co-occurrence spring")

#################################################################

## Subset 3: Belmaker only
# Create species matrix of data.origin == "Belmaker"
med_mat_sub3 <- med_raw %>%
  filter(data.origin == "Belmaker") %>% 
  group_by(site, lon, lat, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
head(med_mat_sub3)
# Add the meta-data and remove NAs
full_med_mat_sub3 <- left_join(med_mat_sub3, med_meta_env, by = c("site", "lon", "lat")) %>%
  na.omit(full_med_mat_sub3)
# Change row names for MRFcov
full_med_mat_sub3 <- as.data.frame(full_med_mat_sub3) # required for changing row names
rownames(full_med_mat_sub3) <- make.unique(full_med_mat_sub3$site, sep = "_")
full_med_mat_sub3 <-  full_med_mat_sub3 %>% select(-c("site", "lon", "lat"))
View(full_med_mat_sub3)

# Convert the abundance matrix to presence-absence matrix
pres_abs_mat_sub3 <- full_med_mat_sub3
pres_abs_mat_sub3[1:54] <- ifelse(pres_abs_mat_sub3[1:54] > 0, 1, 0)
View(pres_abs_mat_sub3)

# Run model
MRF_sub3 <- MRFcov(data = full_med_mat_sub3, n_nodes = 54, n_covariates = 3, family = "gaussian")
MRF_PA_sub3 <- MRFcov(data = pres_abs_mat_sub3, n_nodes = 54, n_covariates = 3, family = "binomial")
plotMRF_hm(MRF_sub3)
