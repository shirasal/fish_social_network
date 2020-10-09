# source("R/functions.R")
# load("data/all_objects.RData")
# source("R/packages.R")
# source("R/run_models.R")

weights <- E(sub_graph)$weight
deg <- igraph::degree(sub_graph, mode = "all")
plot <- ggraph::ggraph(sub_graph, layout = "circle") + 
  geom_edge_link(aes(width = weights, color = weights < 0)) +
  scale_edge_width(range = c(0.1, 1)) +
  scale_edge_color_manual(values = c(my_cols[["neg"]], my_cols[["pos"]])) +
  geom_node_point(aes(size = deg, alpha = 0.5), col = "grey") +
  geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = "lightgrey"),
        aspect.ratio = 1) +
  ggtitle(taxa, subtitle = str_glue("{covariate}; {category}"))