# Required packcages:
library(igraph)
library(tidyverse)
library(MRFcov)

med_raw <- read_csv("med_raw.csv")
str(med_raw)

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
rownames(full_herb) <- make.unique(full_herb$site, sep = "_") # TODO don't make unique, make sure all sites/transects are deifned properly
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

# MRFs with covariates (CRFs) using lrg matrices

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
# png("networks/herbivores_crf.png")
plot.igraph(graph_network_2, layout = layout.circle(graph_network_2),
            edge.width = abs(E(graph_network_2)$weight),
            edge.color = ifelse(E(graph_network_2)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 2,
            vertex.label.color = adjustcolor("#333333", .85),
            vertex.color = adjustcolor("#FFFFFF", .5))
# dev.off()
# The covariates changed the relationship! Hurray! #

#########################################################################################
#########################################################################################

# 01/10/2019

# Create known networks with only temperature

# Metadata tibble - mean annual temperature
med_temp <- med_raw %>% 
  distinct(site, lon, lat, tmean)

##### Subset data (1) Herbivores #####
herb <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  group_by(site, lon, lat, species) %>%
  summarise(n = mean(sp.n)) %>% # mean abundance within transects in the same location
  spread(species, n, fill = 0) %>% 
  select(site, lon, lat, Sarpa.salpa, Siganus.luridus, Siganus.rivulatus, Diplodus.sargus, Thalassoma.pavo)
head(herb)

# Add environmental (meta) data
herb_env <- left_join(herb, med_temp, by = c("site", "lon", "lat"))
head(herb_env)

# Preps for MRFcov analysis
# (1) Remove NAs:
herb_env <- na.omit(herb_env)
unique(is.na(herb_env)) # check that there are no NAs
# (2) Sites to rownames
herb_env <- as.data.frame(herb_env) # required for changing row names
rownames(herb_env) <- make.unique(herb_env$site, sep = "_") # create unique row names
herb_env <-  herb_env %>% select(-c("site", "lon", "lat"))
View(herb_env)

# Run MRF:
MRF_herb <- MRFcov(data = herb_env, n_nodes = grep("tmean", colnames(herb_env)) - 1, family = "gaussian")

# Plot co-occurrence calculated from MRF
# png(filename = "co-occurrence/herbivores_mrf_temp_only.png")
plotMRF_hm(MRF_herb,
           main = "Co-occurrence matrix of herbivore species")
# dev.off()
# file.show("co-occurrence/herbivores_mrf_temp_only.png")

herb_cov <- prep_MRF_covariates(data = herb_env, n_nodes = 5) 
MRF_temp_network <- predict_MRFnetworks(data = herb_env, MRF_mod = MRF_herb)

# Graphic networks
herb_temp_graph <- graph.adjacency(MRF_herb$graph, weighted = T, mode = "undirected") # MRF_herb$graph = association matrix between species
deg <- degree(herb_temp_graph, mode = "all") # analising a property of the graph: the number of the graph's edges (species)

# png("networks/herbivores_mrf_temp.png")
plot.igraph(herb_temp_graph, layout = layout.circle(herb_temp_graph),
            edge.width = abs(E(herb_temp_graph)$weight),
            edge.color = ifelse(E(herb_temp_graph)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 2,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5))
# dev.off()
# file.show("networks/herbivores_mrf_temp.png")

# ---- interim summary of variables ---- #
# MRF_herb = MRF co-occurrence model
# herb_env = Full species matrix with covariate; 5 species; 1 covariate
# herb_cov = ?

# Run CRFs (with temperature as covariate)
CRF_herb <- MRFcov(data = herb_env, n_nodes = 5, prep_covariates = TRUE, n_covariates = 1,
                   family = "gaussian", bootstrap = FALSE)
# png(filename = "co-occurrence/herbivores_crf_temp.png")
plotMRF_hm(CRF_herb, main = "Co-occurrence matrix for herbivores with temperature as covariate")
# dev.off()
file.show("co-occurrence/herbivores_crf_temp.png")

# Calculate linear predictors for species:
fish_predictors <- predict_MRF(data = herb_env, MRF_mod = CRF_herb, prep_covariates = TRUE)
CRF_herb_temp_net <- predict_MRFnetworks(data = herb_env, MRF_mod = CRF_herb,
                                 cached_predictions = fish_predictors, prep_covariates = TRUE)

# Graphic networks
CRF_herb_temp_graph <- graph.adjacency(CRF_herb$graph, weighted = T, mode = "undirected")
deg <- degree(CRF_herb_temp_graph, mode = "all")
# png("networks/herbivores_crf_temp.png")
plot.igraph(CRF_herb_temp_graph, layout = layout.circle(CRF_herb_temp_graph),
            edge.width = abs(E(CRF_herb_temp_graph)$weight),
            edge.color = ifelse(E(CRF_herb_temp_graph)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 2,
            vertex.label.color = adjustcolor("#333333", .85),
            vertex.color = adjustcolor("#FFFFFF", .5))
# dev.off()
file.show("co-occurrence/herbivores_crf_temp.png")

# ---- summary of variables ---- #
# med_raw = raw mediterranean UVC data (post-cleaning)
# MRF_herb = MRF co-occurrence model
# herb_env = Full species matrix with covariate; 5 species; 1 covariate
# herb_cov = ? Calculated tmean for each species..?
# MRF_temp_network = Network of herbivore species from MRF (no cov.)
# herb_temp_graph = Visualisation of MRF_temp_network
# CRF_herb = CRF co-occurrence model
# fish_predictors = ? Used for CRF network
# CRF_herb_temp_net = Network of herbivore species from CRF (with temp. as cov.)
# CRF_herb_temp_graph = Visualisation of CRF_herb_temp_net

##### Subset data (2) Large predatory fish #####
all_traits <- read_csv("species_traits.csv")
large_predators <- all_traits %>% 
  filter(size_class_1 > "S5") %>% # large species
  filter(troph > 4) %>% # high trophic level
  select(species) %>% 
  distinct()

lrg <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  group_by(site, lon, lat, species) %>%
  summarise(n = sum(sp.n)) %>% # total abundance within transects in the same location
  spread(species, n, fill = 0) %>% 
  select(site, lon, lat, large_predators$species)
head(lrg)

# Add environmental (meta) data
lrg_env <- left_join(lrg, med_temp, by = c("site", "lon", "lat"))
head(lrg_env)

# Preps for MRFcov analysis
# (1) Remove NAs:
lrg_env <- na.omit(lrg_env)
unique(is.na(lrg_env)) # check that there are no NAs
# (2) Sites to rownames
lrg_env <- as.data.frame(lrg_env) # required for changing row names
rownames(lrg_env) <- make.unique(lrg_env$site, sep = "_") # create unique row names
lrg_env <-  lrg_env %>% select(-c("site", "lon", "lat"))
View(lrg_env)

# Run MRF:
MRF_lrg <- MRFcov(data = lrg_env, n_nodes = grep("tmean", colnames(lrg_env)) - 1, family = "gaussian")

# Plot co-occurrence calculated from MRF
# png(filename = "co-occurrence/large_mrf.png")
plotMRF_hm(MRF_lrg,
           main = "Co-occurrence matrix of large predators")
# dev.off()
file.show("co-occurrence/large_mrf.png")

# Calculate network
lrg_network <- predict_MRFnetworks(data = lrg_env, MRF_mod = MRF_lrg)

# Graphic networks
lrg_graph <- graph.adjacency(MRF_lrg$graph, weighted = T, mode = "undirected") # MRF_lrg$graph = association matrix between species
deg <- degree(lrg_graph, mode = "all") # analising a property of the graph: the number of the graph's edges (species)

# png("networks/large_mrf.png")
plot.igraph(lrg_graph, layout = layout.circle(lrg_graph),
            edge.width = abs(E(lrg_graph)$weight),
            edge.color = ifelse(E(lrg_graph)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1.5,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5))
# dev.off()
file.show("networks/large_mrf.png")

# Run CRFs (with temperature as covariate)
CRF_lrg <- MRFcov(data = lrg_env, n_nodes = grep("tmean", colnames(lrg_env)) - 1,
                  prep_covariates = TRUE, n_covariates = 1, family = "gaussian", bootstrap = FALSE)

# png(filename = "co-occurrence/large_crf.png")
plotMRF_hm(CRF_lrg, main = "Co-occurrence matrix for large predators with temperature as covariate")
# dev.off()
file.show("co-occurrence/large_crf.png")

# Calculate linear predictors for species:
lrg_cov <- prep_MRF_covariates(data = lrg_env, n_nodes = grep("tmean", colnames(lrg_env)) - 1) 
fish_predictors_lrg <- predict_MRF(data = lrg_env, MRF_mod = CRF_lrg, prep_covariates = TRUE)
CRF_lrg_temp_net <- predict_MRFnetworks(data = lrg_env, MRF_mod = CRF_lrg,
                                         cached_predictions = fish_predictors_lrg, prep_covariates = TRUE)

# Graphic networks
CRF_lrg_graph <- graph.adjacency(CRF_lrg$graph, weighted = T, mode = "undirected")
deg <- degree(CRF_lrg_graph, mode = "all")
# png("networks/large_crf_net.png")
plot.igraph(CRF_lrg_graph, layout = layout.circle(CRF_lrg_graph),
            edge.width = abs(E(CRF_lrg_graph)$weight),
            edge.color = ifelse(E(CRF_lrg_graph)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1.5,
            vertex.label.color = adjustcolor("#333333", .85),
            vertex.color = adjustcolor("#FFFFFF", .5))
# dev.off()
file.show("networks/large_crf_net.png")
