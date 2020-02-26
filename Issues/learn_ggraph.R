
library(ggraph)
library(tidygraph)
library(tidyverse)

# ======================================================================================================= #

graph <- as_tbl_graph(highschool) %>% 
  mutate(Popularity = centrality_degree(mode = 'in'))

ggraph(graph, layout = 'kk') + 
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) + 
  geom_node_point(aes(size = Popularity)) + 
  facet_edges(~year) + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

# ======================================================================================================= #

plotting_func <- function(igraph, category){
  sub_graph <- igraph
  deg <- degree(sub_graph, mode = "all")
  plot <- plot.igraph(sub_graph, layout = layout.circle(sub_graph),
                      edge.width = abs(E(sub_graph)$weight),
                      edge.color = ifelse(E(sub_graph)$weight < 0, '#3399CC', '#FF3333'),
                      vertex.size = deg,
                      vertex.label.family = "sans",
                      vertex.label.font	= 3,
                      vertex.label.cex = 1,
                      vertex.label.color = adjustcolor("#333333", 0.85),
                      vertex.color = adjustcolor("#FFFFFF", .5),
                      main = category)
}

#################
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")

spp_data <- read_csv("data/med_raw.csv", col_types = cols(depth = "d")) %>%
  filter(data.origin != "azz_asi") %>% # remove presence-absence data
  mutate(loc = paste0(site, "_", trans), tmean_reg = scale(tmean)) %>%
  group_by(loc, species, tmean_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  filter(species %in% c(groupers)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers, tmean_reg)
str(spp_data)

# func 2
test_mod <- run_mod(species_mat = spp_data, n_covs = 1, family = "gaussian")
# look at the results
test_mod$mod$direct_coefs
test_mod$boot$mean_key_coefs
test_mod$pred

# Categorise continuous data + Nest categorical (nominal) data
categories <- categorise_cov(species_mat = spp_data, covariate = "tmean_reg")
nested_cats <- nested_data(categorised_data = categories)

# Run MRFcov model with some defaults
cat_model <- get_model(data = spp_data, ncov = 1)
cat_model


# Run model on each category
get_graph <- function(model){
  graph.adjacency(model$graph, weighted = T, mode = "undirected")
}

# Get graph data (func 6):
nested_plot <- nested %>% mutate(plot = map(model, get_graph))

graph <- as_tbl_graph(cat_model$graph)

ggraph(graph, layout = 'circle') + 
  geom_edge_fan(aes(alpha = weight), show.legend = TRUE) + 
  geom_node_label(aes(x = colnames(cat_model$graph)))
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')


# Plot multiple graphs for gradient
temperature_graph <- plot_multi_graphs(nested_df = nested_plot, n_graphs = 3)