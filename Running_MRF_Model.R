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

## herb: species matrix for herbivore fish species and similar
med_mat_sub1 <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  group_by(site, lon, lat, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  select(site, lon, lat, Sarpa.salpa, Siganus.luridus, Siganus.rivulatus, Diplodus.sargus, Thalassoma.pavo)
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

# # Convert the abundance matrix to presence-absence matrix
# pres_abs_mat_sub1 <- full_med_mat_sub1
# pres_abs_mat_sub1[1:4] <- ifelse(pres_abs_mat_sub1[1:4] > 0, 1, 0)
# # View(pres_abs_mat_sub1)

MRF_sub1 <- MRFcov(data = full_med_mat_sub1, n_nodes = grep("tmean", colnames(full_med_mat_sub1)) - 1,
                   n_covariates = 3, family = "gaussian")
# MRF_PA_sub1 <- MRFcov(data = pres_abs_mat_sub1, n_nodes = 4, n_covariates = 3, family = "binomial")
plotMRF_hm(MRF_sub1,
           main =  paste("Co-occurrence matrix of herbivores", grep("tmean", colnames(full_med_mat_sub1)) - 1, " species"))

network_1 <- predict_MRFnetworks(data = full_med_mat_sub1, MRF_mod = MRF_sub1,
                               cached_predictions = fish_predictors, prep_covariates = FALSE)

# Graphic networks
graph_network_1 <- graph.adjacency(MRF_sub1$graph, weighted = T, mode = "undirected")
deg <- degree(graph_network, mode = "all")
windowsFonts(Berlin = windowsFont("Berlin Sans FB"))
plot.igraph(graph_network_1, layout = layout.circle(graph_network_1),
            edge.width = abs(E(graph_network_1)$weight),
            edge.color = ifelse(E(graph_network_1)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "Berlin",
            vertex.label.font	= 3,
            vertex.label.cex = 0.8,
            vertex.label.color = adjustcolor("#333333", .6),
            vertex.color = adjustcolor("#FFFFFF", .5))
#################################################################
######## MRFs with covariates (CRFs) using sub1 matrices ########
#################################################################

full_med_mat_sub1 # abundances
pres_abs_mat_sub1 # presence absence
MRF_sub1 <- MRFcov(data = full_med_mat_sub1, n_nodes = grep("tmean", colnames(full_med_mat_sub1)) - 1,
                   n_covariates = 3, family = "gaussian")
sub1_cov <- prep_MRF_covariates(data = full_med_mat_sub1, n_nodes = 5)
sub1_MRF_cov <- MRFcov(data = full_med_mat_sub1, n_nodes = 5, prep_covariates = TRUE, family = "gaussian")
plotMRF_hm(sub1_MRF_cov)
# Calculate linear predictors for species:
fish_predictors <- predict_MRF(data = full_med_mat_sub1, MRF_mod = MRF_sub1, prep_covariates = TRUE)
boot <- bootstrap_MRF(data = full_med_mat_sub1, n_nodes = 5, n_covariates = 3, family = "gaussian")
network_2 <- predict_MRFnetworks(data = full_med_mat_sub1, MRF_mod = sub1_MRF_cov,
                               cached_predictions = fish_predictors, prep_covariates = TRUE)
# Graphic networks
graph_network_2 <- graph.adjacency(sub1_MRF_cov$graph, weighted = T, mode = "undirected")
deg <- degree(graph_network_2, mode = "all")
windowsFonts(Berlin = windowsFont("Berlin Sans FB"))
plot.igraph(graph_network_2, layout = layout.circle(graph_network_2),
            edge.width = abs(E(graph_network_2)$weight),
            edge.color = ifelse(E(graph_network_2)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "Berlin",
            vertex.label.font	= 3,
            vertex.label.cex = 0.8,
            vertex.label.color = adjustcolor("#333333", .6),
            vertex.color = adjustcolor("#FFFFFF", .5))

############# SUMMARY so far: The covariates changed the relationship! Hurray! ##########
