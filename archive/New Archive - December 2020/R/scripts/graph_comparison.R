library(magrittr)
library(MRFcov)
library(tidyverse)

# Based on: https://www.jessesadler.com/post/network-analysis-with-r/

# load data
med_raw <- read_rds("data/medata.Rds") %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(loc = paste0(site, "_", trans),
         tmean_reg = scale(tmean),
         depth_reg = scale(depth),
         # log_n = log10(sp.n),
         mpa = if_else(enforcement <= 1, FALSE, TRUE))

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")

# a) Create species matrix
spp_mat <- med_raw %>%
  group_by(lat, lon, loc, species, tmean_reg, mpa, depth_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  filter(species %in% groupers) %>% 
  spread(species, n, fill = 0) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(loc = make.unique(.$loc, "_")) %>% 
  column_to_rownames("loc") %>%
  select(groupers, "tmean_reg") %>% 
  as.matrix()

# b) Create a coordinate dataframe:
coords <- med_raw %>%
  group_by(lat, lon, loc, species, tmean_reg, mpa, depth_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  filter(species %in% groupers) %>% 
  spread(species, n, fill = 0) %>% 
  ungroup() %>% 
  mutate(loc = make.unique(.$loc, "_")) %>% 
  as.data.frame() %>% 
  column_to_rownames("loc") %>%
  na.omit() %>% 
  select(lat, lon)

nrow(spp_mat) == nrow(coords)

# Run the model
groupers_temp <- MRFcov_spatial(data = spp_mat, n_nodes = 5, n_covariates = 1,
                                family = "gaussian", coords = coords)

MRFcov::plotMRF_hm(groupers_temp)

# ggnet -------------------------------------------------------------------

library(GGally)

rel_mat <- groupers_temp$graph # extract this element
class(rel_mat) # see what's its class - it's a matrix
rel_mat <- data.frame(groupers_temp$graph, row.names = groupers) # create a df to work with

# Create an edge list
edgelist <- rel_mat %>%
  gather() %>%
  transmute(sp1 = key, sp2 = rep(groupers, times = 5), magnitude = value)

# Create a network object with 'network' package
groupers_network <- network::network(edgelist, matrix.type = "edgelist", ignore.eval = FALSE)
class(groupers_network)
summary(groupers_network)

plot(groupers_network, mode = "circle")


# igraph ------------------------------------------------------------------
detach(package:network)
library(igraph)
groupers_network <- graph.adjacency(groupers_temp$graph, weighted = TRUE, mode = "undirected")
deg <- degree(groupers_network, mode = "all")
class(groupers_network)

plot(groupers_network,
     layout = layout.circle(groupers_network),
     vertex.cex = log10(deg),
     vertex.color = adjustcolor("lightblue", .5),
     edge.width = scale(abs(E(groupers_network)$weight)),
     edge.color = ifelse(E(groupers_network)$weight < 0, '#3399CC', '#FF3333'), # blue = negative, red = positive
     vertex.label.family = "sans",
     vertex.label.font	= 3,
     vertex.label.cex = 1,
     vertex.label.color = adjustcolor("#333333", 0.85),
     main = "Serranidae")


# tidygraph & ggraph ------------------------------------------------------
library(tidygraph)
library(ggraph)

groupers_tidy <- as_tbl_graph(groupers_network)

# what's the difference?
setdiff(class(groupers_tidy), class(groupers_network))
# groupers_tidy is a tbl_graph object while groupers_network is only an igraph object
groupers_tidy
# waaaay tidier than igraph and details are a lot clearer
# if I wanted to rearrange the rows in the edges tibble to list those with the highest “weight” first:
groupers_tidy %>% 
  activate(edges) %>% 
  arrange(desc(magnitude))

# Plot with ggraph
ggraph(groupers_tidy, layout = "circle") + 
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE) +
  geom_edge_link(aes(width = magnitude, colour = ifelse(magnitude < 0, "lightblue", "brown1"))) +
  scale_edge_width(range = c(0.2, 2)) +
  theme(legend.position = "none")
  
# Another plotting option
ggraph(groupers_tidy, layout = "linear") +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE) +
  geom_edge_arc(aes(width = magnitude, colour = ifelse(magnitude < 0, "lightblue", "brown1"))) + 
  scale_edge_width(range = c(0.2, 2)) +
  theme(legend.position = "none") +
  coord_fixed()


# Announcement ------------------------------------------------------------

# And the winner is...
#                     ggraph!





# Not important -----------------------------------------------------------

# Just for fun: create a mandala (I don't really need different magnitude for each one but in case I'll use it for something else in the future..)
edge_mand <- edgelist %>%
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1)) %>% 
  add_row(sp1 = sample(groupers, 1), sp2 = sample(groupers, 2), magnitude = rnorm(1))

mand_network <- graph_from_data_frame(edge_mand, directed = FALSE, vertices = unique(edge_mand$sp1))

plot(mand_network, mode = "circle", vertex.cex = 2, vertex.color = "lightblue", vertex.alpha = 0.3,
     vertex.label = NA)

# This is fun but let's get back to work
rm(list = c("mand_network", "edge_mand"))




