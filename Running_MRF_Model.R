# Required packcages:
library(igraph)
library(tidyverse)
library(MRFcov)

med_raw <- read_csv("med_raw.csv")
med_raw$depth <- as.numeric(med_raw$depth) # fix class issue (instead of logic, as is has been parsed)

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

##### ==== CREATE SPECIES MATRIX from SUBSETTED DATA ==== #####

## sub1 = species matrix where the count of fish > 10
med_mat_sub1 <- med_raw %>%
  filter(sp.n > 0) %>% # remove mistakes where species count is 0
  group_by(site, lon, lat, species) %>%
  filter(sp.n > 10) %>% 
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
head(med_mat_sub1)

## Add environmental (meta) data
full_med_mat_sub1 <- left_join(med_mat_sub1, med_meta_env, by = c("site", "lon", "lat"))
# View(full_med_mat_sub1)

# Preps for MRFcov analysis: 1. remove NAs
full_med_mat_sub1 <- na.omit(full_med_mat_sub1)
# unique(is.na(full_med_mat_sub1)) # check there are no NAs

# Preps for MRFcov analysis: 2. Sites to rownames
full_med_mat_sub1 <- as.data.frame(full_med_mat_sub1) # required for changing row names
rownames(full_med_mat_sub1) <- make.unique(full_med_mat_sub1$site, sep = "_")
full_med_mat_sub1 <-  full_med_mat_sub1 %>% select(-c("site", "lon", "lat"))
# View(full_med_mat_sub1)

# Convert the abundance matrix to presence-absence matrix
pres_abs_mat_sub1 <- full_med_mat_sub1
pres_abs_mat_sub1[1:49] <- ifelse(pres_abs_mat_sub1[1:49] > 0, 1, 0)
# View(pres_abs_mat_sub1)

MRF_sub1 <- MRFcov(data = full_med_mat_sub1, n_nodes = grep("tmean", colnames(full_med_mat_sub1)) - 1,
                   n_covariates = 3, family = "gaussian")
# MRF_PA_sub1 <- MRFcov(data = pres_abs_mat_sub1, n_nodes = 54, n_covariates = 3, family = "binomial")
plotMRF_hm(MRF_sub1,
           main =  paste("Sum of sp.n > 10, ", grep("tmean", colnames(full_med_mat_sub1)) - 1, " species"))
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
# pres_abs_mat_sub2_su <- full_med_mat_sub2_su
# pres_abs_mat_sub2_su[1:96] <- ifelse(pres_abs_mat_sub2_su[1:96] > 0, 1, 0)
MRF_sub2_su <- MRFcov(data = full_med_mat_sub2_su, n_nodes = grep("tmean", colnames(full_med_mat_sub2_su)) - 1,
                      n_covariates = 3, family = "gaussian")
# MRF_PA_sub2_su <- MRFcov(data = pres_abs_mat_sub2_su, n_nodes = 96, n_covariates = 3, family = "binomial")
plotMRF_hm(MRF_sub2_su,
           main =  paste("Summer, ", grep("tmean", colnames(full_med_mat_sub2_su)) - 1, " species"))

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
pres_abs_mat_sub2_au[1:88] <- ifelse(pres_abs_mat_sub2_au[1:88] > 0, 1, 0)
MRF_sub2_au <- MRFcov(data = full_med_mat_sub2_au, n_nodes = grep("tmean", colnames(full_med_mat_sub2_au)) - 1,
                      n_covariates = 3, family = "gaussian")
MRF_PA_sub2_au <- MRFcov(data = pres_abs_mat_sub2_au, n_nodes = 88, n_covariates = 3,
                         family = "binomial")
plotMRF_hm(MRF_sub2_au,
           main =  paste("Autumn, ", grep("tmean", colnames(full_med_mat_sub2_au)) - 1, " species"))

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
pres_abs_mat_sub2_sp[1:75] <- ifelse(pres_abs_mat_sub2_sp[1:75] > 0, 1, 0)
MRF_sub2_sp <- MRFcov(data = full_med_mat_sub2_sp, n_nodes = grep("tmean", colnames(full_med_mat_sub2_sp)) - 1,
                      n_covariates = 3, family = "gaussian")
# MRF_PA_sub2_sp <- MRFcov(data = pres_abs_mat_sub2_sp, n_nodes = 75, n_covariates = 3, family = "binomial")

plotMRF_hm(MRF_sub2_sp,
           main =  paste("Spring, ", grep("tmean", colnames(full_med_mat_sub2_sp)) - 1, " species"))

#################################################################

## Subset 3: Belmaker only
# Create species matrix of data.origin == "Belmaker"
med_mat_sub3 <- med_raw %>%
  filter(data.origin == "Belmaker") %>% 
  group_by(site, lon, lat, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
# Add the meta-data and remove NAs
full_med_mat_sub3 <- left_join(med_mat_sub3, med_meta_env, by = c("site", "lon", "lat")) %>%
  na.omit(full_med_mat_sub3)
# Change row names for MRFcov
full_med_mat_sub3 <- as.data.frame(full_med_mat_sub3) # required for changing row names
rownames(full_med_mat_sub3) <- make.unique(full_med_mat_sub3$site, sep = "_")
full_med_mat_sub3 <-  full_med_mat_sub3 %>% select(-c("site", "lon", "lat"))
# View(full_med_mat_sub3)

# # Convert the abundance matrix to presence-absence matrix
# pres_abs_mat_sub3 <- full_med_mat_sub3
# pres_abs_mat_sub3[1:97] <- ifelse(pres_abs_mat_sub3[1:97] > 0, 1, 0)
# View(pres_abs_mat_sub3)

# Run model
MRF_sub3 <- MRFcov(data = full_med_mat_sub3, n_nodes = grep("tmean", colnames(full_med_mat_sub3)) - 1,
                   n_covariates = 3, family = "gaussian")
# MRF_PA_sub3 <- MRFcov(data = pres_abs_mat_sub3, n_nodes = 97, n_covariates = 3, family = "binomial")
plotMRF_hm(MRF_sub3,
           main = paste("Belmaker only, ", grep("tmean", colnames(full_med_mat_sub3)) - 1, " species"))

##########################################################
## Considering temperature only as covariate:
## sub4 = species matrix where the count of fish > 10 and only one covariate = temperature

# 1. Create matrix
med_mat_sub4 <- med_raw %>%
  group_by(site, lon, lat, species) %>%
  filter(sp.n > 10) %>% 
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
# Add meta-data (temperature only)
full_med_mat_sub4 <- left_join(med_mat_sub4, med_temp_mean, by = c("site", "lon", "lat"))
# Preps for MRFcov analysis: 1. remove NAs
full_med_mat_sub4 <- na.omit(full_med_mat_sub4)
# unique(is.na(full_med_mat_sub4)) # check there are no NAs
# Preps for MRFcov analysis: 2. Sites to rownames
full_med_mat_sub4 <- as.data.frame(full_med_mat_sub4) # required for changing row names
rownames(full_med_mat_sub4) <- make.unique(full_med_mat_sub4$site, sep = "_")
full_med_mat_sub4 <-  full_med_mat_sub4 %>% select(-c("site", "lon", "lat"))
# View(full_med_mat_sub4)

# # Convert the abundance matrix to presence-absence matrix
# pres_abs_mat_sub4 <- full_med_mat_sub4
# pres_abs_mat_sub4[1:54] <- ifelse(pres_abs_mat_sub4[1:54] > 0, 1, 0)
# View(pres_abs_mat_sub4)

MRF_sub4 <- MRFcov(data = full_med_mat_sub4, n_nodes = grep("tmean", colnames(full_med_mat_sub4)) - 1,
                   n_covariates = 1, family = "gaussian")
# MRF_PA_sub4 <- MRFcov(data = pres_abs_mat_sub4, n_nodes = 54, n_covariates = 1, family = "binomial")
plotMRF_hm(MRF_sub4,
           main = paste("Temperature only, sum of sp.n > 10; ", grep("tmean", colnames(full_med_mat_sub4)) - 1, " species"))

#################################################################
######## MRFs with covariates (CRFs) using sub1 matrices ########
#################################################################

full_med_mat_sub1 # abundances
pres_abs_mat_sub1 # presence absence
MRF_sub1 <- MRFcov(data = full_med_mat_sub1, n_nodes = grep("tmean", colnames(full_med_mat_sub1)) - 1,
                   n_covariates = 3, family = "gaussian")
sub1_cov <- prep_MRF_covariates(data = full_med_mat_sub1, n_nodes = 49)
sub1_MRF_cov <- MRFcov(data = full_med_mat_sub1, n_nodes = 49, prep_covariates = TRUE, family = "gaussian")
plotMRF_hm(sub1_MRF_cov)
# Calculate linear predictors for species:
fish_predictors <- predict_MRF(data = full_med_mat_sub1, MRF_mod = MRF_sub1, prep_covariates = TRUE)
boot <- bootstrap_MRF(data = full_med_mat_sub1, n_nodes = 49, n_covariates = 3, family = "gaussian")
network <- predict_MRFnetworks(data = full_med_mat_sub1, MRF_mod = sub1_MRF_cov,
                               cached_predictions = fish_predictors, prep_covariates = TRUE)
# Graphic networks
graph_network <- graph.adjacency(sub1_MRF_cov$graph, weighted = T, mode = "undirected")
plot.igraph(graph_network, layout = layout.circle(graph_network),
            edge.width = abs(E(graph_network)$weight),
            edge.color = ifelse(E(graph_network)$weight < 0, 
                                'blue',
                                'red'))
