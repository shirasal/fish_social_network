plotting_func <- function(igraph, category, covariate, basin){
  sub_graph <- as_tbl_graph(igraph)
  deg <- igraph::degree(sub_graph, mode = "all")
  plot <- ggraph(subgraph, layout = "circle") + 
    geom_node_point() +
    geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE) +
    geom_edge_link(aes(width = weight, colour = ifelse(weight < 0, "lightblue", "brown1"))) +
    scale_edge_width(range = c(0.2, 2)) +
    theme(legend.position = "none") +
    ggtitle(category, subtitle = paste0(covriate, "; ", basin))
}


