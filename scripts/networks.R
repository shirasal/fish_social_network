library(igraph)
library(tidyverse)
library(MRFcov)

# load MEData
med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double()))
str(med_raw)

### Network 1: Groupers and combers
# list the species (easier to work with vectors)
groupers <- c("Epinephelus.aeneus", "Epinephelus.caninus", "Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.hepatus", "Serranus.scriba")

# Create a species matrix of 'groupers' species with rownames = sites, tmean as covariate
# this is the format that works with MRF function
Serranidae <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = make.unique(paste0(site, "_", trans))) %>% 
  group_by(loc, species, tmean) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(.$loc) %>%
  select(groupers[1:8], tmean) %>% 
  str()

# Run MRF with tmean as covariate
serr_mrf <- MRFcov(data = Serranidae, n_nodes = 8, prep_covariates = TRUE, n_covariates = 1,
                   family = "gaussian")

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
