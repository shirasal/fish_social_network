# source("R/functions.R")
# load("data/all_objects.RData")
# source("R/packages.R")
# source("R/run_models.R")

my_cols <- c(neg = '#3399CC', pos = '#FF3333')

grps_graph <- grps_mod$graph %>% tidygraph::as_tbl_graph()
g_weights <- E(grps_graph)$weight
g_deg <- igraph::degree(grps_graph, mode = "all")
g_plot <- ggraph::ggraph(grps_graph, layout = "circle") + 
  geom_edge_link(aes(width = g_weights, color = g_weights < 0)) +
  scale_edge_width(range = c(0.1, 1)) +
  scale_edge_color_manual(values = c(my_cols[["neg"]], my_cols[["pos"]])) +
  geom_node_point(aes(size = g_deg, alpha = 0.5), col = "grey") +
  geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        aspect.ratio = 1) +
  ggtitle("Groupers network")

g_plot


dip_graph <- dip_mod$graph %>% tidygraph::as_tbl_graph()
d_weights <- E(dip_graph)$weight
d_deg <- igraph::degree(dip_graph, mode = "all")
d_plot <- ggraph::ggraph(dip_graph, layout = "circle") + 
  geom_edge_link(aes(width = d_weights, color = d_weights < 0)) +
  scale_edge_width(range = c(0.1, 1)) +
  scale_edge_color_manual(values = c(my_cols[["neg"]], my_cols[["pos"]])) +
  geom_node_point(aes(size = d_deg, alpha = 0.5), col = "grey") +
  geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        aspect.ratio = 1) +
  ggtitle("Diplodus network")

d_plot

ggsave(filename = "figures/networks/dip_mean.png", plot = d_plot, dpi = 300)


herb_graph <- herb_mod$graph %>% tidygraph::as_tbl_graph()
h_weights <- E(herb_graph)$weight
h_deg <- igraph::degree(herb_graph, mode = "all")
h_plot <- ggraph::ggraph(herb_graph, layout = "circle") + 
  geom_edge_link(aes(width = h_weights, color = h_weights < 0)) +
  scale_edge_width(range = c(0.1, 1)) +
  scale_edge_color_manual(values = c(my_cols[["neg"]], my_cols[["pos"]])) +
  geom_node_point(aes(size = h_deg, alpha = 0.5), col = "grey") +
  geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        aspect.ratio = 1) +
  ggtitle("Herbivores network")

h_plot
