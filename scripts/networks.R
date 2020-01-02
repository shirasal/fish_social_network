library(parallel)
library(igraph)
library(tidyverse)
library(MRFcov)

parallel::detectCores() # in case I'd like to speed up MRFcov by spreading processing over >1 core

# load MEData
med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double())) %>%
  mutate(tmean_reg = scale(tmean)) # regularise temperature covariate
str(med_raw)

# Find species names in the dataset:
med_raw %>% filter(grepl(pattern = "cretense", x = .$species)) %>% distinct(species)

(min_temp <- min(med_raw$tmean)) # 16.8
(med_temp <- median(med_raw$tmean)) # 20
(max_temp <- max(med_raw$tmean)) # 23

# Check abundance of species I'm interested in:
spp <- list("Epinephelus.costae", "Epinephelus.marginatus", "Epinephelus.caninus",
              "Epinephelus.aeneus", "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba",
              "Serranus.hepatus", "Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")

med_raw %>% 
  group_by(species) %>% 
  summarise(n = sum(sp.n)) %>% 
  filter(species %in% spp)


### Network 1: Groupers and combers
# list the species (easier to work with vectors)
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
# I didn't include species that appear only a few times in the dataset

# Create a species matrix of 'groupers' species with rownames = sites, tmean as covariate
# this is the format that works with MRF function
Serranidae <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers[1:5], tmean_reg)
str(Serranidae)

# Run MRF with tmean as covariate
serr_mrf <- MRFcov(data = Serranidae, n_nodes = 5, prep_covariates = TRUE, n_covariates = 1,
                   family = "gaussian")

# check out the coefficients of each node to see which predictors determine it's likelihood of occurrence:
serr_mrf$key_coefs$Epinephelus.costae
serr_mrf$key_coefs$Epinephelus.marginatus
serr_mrf$key_coefs$Mycteroperca.rubra
serr_mrf$key_coefs$Serranus.cabrilla
serr_mrf$key_coefs$Serranus.scriba

serr_mrf$direct_coefs
serr_mrf$graph
serr_mrf$param_names

# Look at co occurrence
plotMRF_hm(serr_mrf)

# Create a network and a graph
serr_net <- predict_MRFnetworks(data = Serranidae, MRF_mod = serr_mrf, prep_covariates = TRUE)
serr_graph <- graph.adjacency(serr_mrf$graph, weighted = T, mode = "undirected")
deg <- degree(serr_graph, mode = "all")
plot.igraph(serr_graph, layout = layout.circle(serr_graph),
            edge.width = abs(E(serr_graph)$weight),
            edge.color = ifelse(E(serr_graph)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5))


### Network 2: Diplodus

# Find species names in the dataset:
med_raw %>% filter(grepl(pattern = "ghobban", x = .$species)) %>% distinct(species)

# list the species (easier to work with vectors)
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")

Diplodus <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(diplodus[1:5], tmean_reg)

dip_mrf <- MRFcov(data = Diplodus, n_nodes = 5, prep_covariates = TRUE, n_covariates = 1,
                   family = "gaussian")

# check out the coefficients of each node to see which predictors determine it's likelihood of occurrence:
dip_mrf$key_coefs$Diplodus.annularis
dip_mrf$key_coefs$Diplodus.puntazzo
dip_mrf$key_coefs$Diplodus.sargus
dip_mrf$key_coefs$Diplodus.vulgaris
dip_mrf$key_coefs$Diplodus.cervinus

plotMRF_hm(dip_mrf)

dip_net <- predict_MRFnetworks(data = Diplodus, MRF_mod = dip_mrf, prep_covariates = TRUE)
dip_graph <- graph.adjacency(dip_mrf$graph, weighted = T, mode = "undirected")
deg <- degree(dip_graph, mode = "all")
plot.igraph(dip_graph, layout = layout.circle(dip_graph),
            edge.width = abs(E(dip_graph)$weight),
            edge.color = ifelse(E(dip_graph)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5))



### Network 3: Herbivores

herb <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
              "Scarus.ghobban", "Sparisoma.cretense")
  

Herbiv <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(herb[1:5], tmean_reg)

herb_mrf <- MRFcov(data = Herbiv, n_nodes = 5, prep_covariates = TRUE, n_covariates = 1,
                  family = "gaussian")

# check out the coefficients of each node to see which predictors determine it's likelihood of occurrence:
herb_mrf$key_coefs$Siganus.rivulatus
herb_mrf$key_coefs$Siganus.luridus
herb_mrf$key_coefs$Sarpa.salpa
herb_mrf$key_coefs$Scarus.ghobban
herb_mrf$key_coefs$Sparisoma.cretense

plotMRF_hm(herb_mrf)

herb_net <- predict_MRFnetworks(data = Herbiv, MRF_mod = herb_mrf, prep_covariates = TRUE)
herb_graph <- graph.adjacency(herb_mrf$graph, weighted = T, mode = "undirected")
deg <- degree(herb_graph, mode = "all")
plot.igraph(herb_graph, layout = layout.circle(herb_graph),
            edge.width = abs(E(herb_graph)$weight),
            edge.color = ifelse(E(herb_graph)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5))


