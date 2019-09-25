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
herb <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  group_by(site, lon, lat, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  select(site, lon, lat, Sarpa.salpa, Siganus.luridus, Siganus.rivulatus, Diplodus.sargus, Thalassoma.pavo)
head(herb)

## Add environmental (meta) data
full_herb <- left_join(herb, med_meta_env, by = c("site", "lon", "lat"))
head(full_herb)

# Preps for MRFcov analysis: 1. remove NAs
full_herb <- na.omit(full_herb)
unique(is.na(full_herb)) # check there are no NAs

# Preps for MRFcov analysis: 2. Sites to rownames
full_herb <- as.data.frame(full_herb) # required for changing row names
rownames(full_herb) <- make.unique(full_herb$site, sep = "_")
full_herb <-  full_herb %>% select(-c("site", "lon", "lat"))
View(full_herb)

# # Convert the abundance matrix to presence-absence matrix
# pres_abs_herb <- full_herb
# pres_abs_herb[1:4] <- ifelse(pres_abs_herb[1:4] > 0, 1, 0)
# # View(pres_abs_herb)

MRF_herb <- MRFcov(data = full_herb, n_nodes = grep("tmean", colnames(full_herb)) - 1,
                   n_covariates = 3, family = "gaussian")
# MRF_PA_herb <- MRFcov(data = pres_abs_herb, n_nodes = 4, n_covariates = 3, family = "binomial")
# png(filename = "co-occurrence/herbivores_mrf.png")
plotMRF_hm(MRF_herb,
           main =  paste("Co-occurrence matrix of herbivore species and alike"))
# dev.off()
network_1 <- predict_MRFnetworks(data = full_herb, MRF_mod = MRF_herb,
                               cached_predictions = fish_predictors, prep_covariates = FALSE)

# Graphic networks
graph_network_1 <- graph.adjacency(MRF_herb$graph, weighted = T, mode = "undirected")
deg <- degree(graph_network_1, mode = "all")

# png("networks/herbivores_mrf.png")
plot.igraph(graph_network_1, layout = layout.circle(graph_network_1),
            edge.width = abs(E(graph_network_1)$weight),
            edge.color = ifelse(E(graph_network_1)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 2,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5))
# dev.off()

#################################################################
######## MRFs with covariates (CRFs) using herb matrices ########
#################################################################

# full_herb # abundances
# pres_abs_herb # presence absence
MRF_herb_cov <- MRFcov(data = full_herb, n_nodes = grep("tmean", colnames(full_herb)) - 1,
                   n_covariates = 3, family = "gaussian")
herb_cov <- prep_MRF_covariates(data = full_herb, n_nodes = 5)
herb_MRF_cov <- MRFcov(data = full_herb, n_nodes = 5, prep_covariates = TRUE, family = "gaussian")
# png(filename = "co-occurrence/herbivores_crf.png")
plotMRF_hm(herb_MRF_cov)
# dev.off()
# Calculate linear predictors for species:
fish_predictors <- predict_MRF(data = full_herb, MRF_mod = MRF_herb, prep_covariates = TRUE)
boot <- bootstrap_MRF(data = full_herb, n_nodes = 5, n_covariates = 3, family = "gaussian")
network_2 <- predict_MRFnetworks(data = full_herb, MRF_mod = herb_MRF_cov,
                               cached_predictions = fish_predictors, prep_covariates = TRUE)
# Graphic networks
graph_network_2 <- graph.adjacency(herb_MRF_cov$graph, weighted = T, mode = "undirected")
deg <- degree(graph_network_2, mode = "all")
png("networks/herbivores_crf.png")
plot.igraph(graph_network_2, layout = layout.circle(graph_network_2),
            edge.width = abs(E(graph_network_2)$weight),
            edge.color = ifelse(E(graph_network_2)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 2,
            vertex.label.color = adjustcolor("#333333", .85),
            vertex.color = adjustcolor("#FFFFFF", .5))
dev.off()
############# SUMMARY so far: The covariates changed the relationship! Hurray! ##########
