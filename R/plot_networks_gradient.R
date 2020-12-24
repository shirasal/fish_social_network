source("R/models.R")
plotMRF_net_cont = function(data, MRF_mod, node_names, covariate, main, cutoff, plot){
  
  if(MRF_mod$mod_type == 'MRFcov'){
    plot_booted_coefs <- FALSE
  } else {
    plot_booted_coefs <- TRUE
  }
  
  if(missing(plot)){
    plot <- TRUE
  }
  
  if(missing(main)){
    main <- paste('Estimated node interactions at increasing',
                  covariate,
                  'magnitudes')
  }
  
  if(missing(cutoff)){
    cutoff <- 0
  }
  
  #### Function to create network graphs
  create_netgraph = function(matrix, node_names, cutoff, plot){
    
    # Create the adjacency network graph
    comm.net <- igraph::graph.adjacency(matrix, weighted = T, mode = "undirected")
    # Specify edge colours
    cols <- c(neg = "#FF3333", pos = "#3399CC")
    
    igraph::E(comm.net)$color <- ifelse(igraph::E(comm.net)$weight < 0,
                                        cols[["neg"]],
                                        cols[["pos"]])
    igraph::E(comm.net)$width <- abs(igraph::E(comm.net)$weight)
    igraph::V(comm.net)$label <- str_replace(node_names, "\\.", "\\ ")
    igraph::V(comm.net)$color <- grDevices::adjustcolor("grey", alpha.f = .6)
    
    # Create the network plot
    graphics::par(mar = c(2,2,2,2))
    net.plot <- plot(comm.net,
                     # layout = igraph::layout.davidson.harel(comm.net),
                     vertex.label.cex = 0.84,
                     layout = igraph::layout.circle,
                     vertex.frame.color = grDevices::adjustcolor("grey", alpha.f = .6),
                     vertex.shape = "circle",
                     vertex.label.family = "sans",
                     vertex.label.font = 3,
                     vertex.label.color = "black")
    return(net.plot)
  }
  
  if(!plot_booted_coefs){
    #### Extract model coefficients ####
    interaction_coefficients <- MRF_mod$graph
    
    #### Specify default parameter settings ####
    if(missing(node_names)){
      node_names <- colnames(interaction_coefficients)
    }
    dimnames(interaction_coefficients) <- list(node_names,
                                               node_names)
    
    #### Extract indirect effect matrix that matches the covariate name ####
    indirect_coef_names <- names(MRF_mod$indirect_coefs)
    which_matrix_keep <- grepl(covariate, indirect_coef_names)
    covariate_matrix <- MRF_mod$indirect_coefs[which_matrix_keep]
    covariate_matrix <- as.matrix(covariate_matrix[[1]][[1]])
    baseinteraction_matrix <- as.matrix(MRF_mod$graph)
    
  } else {
    
    #### If plot_booted_coefs = TRUE, extract and plot mean coefficients ####
    #### Extract model coefficients ####
    coef_matrix <- MRF_mod$direct_coef_means
    interaction_coefficients <- coef_matrix[, 2:(nrow(coef_matrix) + 1)]  +
      (Reduce(`+`, MRF_mod$indirect_coef_mean) /
         length(MRF_mod$indirect_coef_mean))
    
    #### Specify default parameter settings ####
    if(missing(node_names)){
      node_names <- rownames(coef_matrix)
    }
    dimnames(interaction_coefficients) <- list(node_names, node_names)
    
    #### Extract indirect effect matrix that matches the covariate name ####
    indirect_coef_names <- names(MRF_mod$indirect_coef_mean)
    which_matrix_keep <- grepl(covariate, indirect_coef_names)
    covariate_matrix <- MRF_mod$indirect_coef_mean[which_matrix_keep][[1]]
    rownames(covariate_matrix) <- node_names
    colnames(covariate_matrix) <- node_names
    baseinteraction_matrix <- interaction_coefficients
    
  }
  
  #### Extract quantiles of observed values for the covariate ####
  observed_cov_values <- as.vector(data[[paste(covariate)]])
  observed_cov_quantiles <- quantile(observed_cov_values,
                                     probs = c(0, 0.5, 1), na.rm = T)
  
  #If number of unique values is low, quantiles may be identical. Instead,
  #generate a sequence of 10 simulated values from the observed min to the observed max
  if(length(unique(observed_cov_quantiles)) < 3){
    observed_cov_quantiles <- quantile(seq(min(observed_cov_values),
                                           max(observed_cov_values),
                                           length.out = 10),
                                       probs = c(0, 0.5, 1), na.rm = T)
  }
  
  #### Create a gridded plot object to plot the three networks
  graphics::par(mfrow = c(1, length(observed_cov_quantiles)), mar = c(2,2,2,2))
  cont.cov.mats <- lapply(observed_cov_quantiles, function(j){
    pred_values <- (covariate_matrix * j) + baseinteraction_matrix
    net.plot <- create_netgraph(matrix = pred_values,
                                node_names = node_names,
                                cutoff = cutoff, plot = plot)
  })
  
  # If plot = FALSE, return the list of weighted adjacency matrices
  if(!plot){
    names(cont.cov.mats) <- c('Min', 'Median', 'Max')
    cont.cov.mats
  } else {
    
    # If plot = TRUE, add text and arrows to the plot and return
    graphics::arrows(x0 = -5.3, y0 = 1.4, x1 = 0,
                     y1 = 1.4, xpd = NA, length = 0.1)
    graphics::mtext(main, side = 3,
                    line = -2, outer = T, cex = 1.2)
  }
}
png(filename = "figures/networks/temp_grps.png", res = 150, width = 5, height = 3.7, units = "in")
plotMRF_net_cont(grps_mat, grps_pois, node_names = groupers, covariate = "temp", main = "Groupers network along temperature gradient", plot = TRUE)
dev.off()

png(filename = "figures/networks/temp_dip.png", res = 150, width = 5, height = 3.7, units = "in")
plotMRF_net_cont(dip_mat, dip_pois, node_name = diplodus, covariate = "temp", main = "Seabreams network along temperature gradient", plot = TRUE)
dev.off()



plotMRF_net_factor <- function(data, MRF_mod, node_names, covariate, main){
  #### Function to create network graphs
  create_netgraph  <- function(matrix, node_names, predictor_value){
    
    # Create the adjacency network graph
    comm.net <- igraph::graph.adjacency(matrix, weighted = T, mode = "undirected")
    # Specify edge colours
    cols <- c(neg = "#FF3333", pos = "#3399CC")
    
    igraph::E(comm.net)$color <- ifelse(igraph::E(comm.net)$weight < 0,
                                        cols[["neg"]],
                                        cols[["pos"]])
    igraph::E(comm.net)$width <- abs(igraph::E(comm.net)$weight)
    igraph::V(comm.net)$label <- str_replace(node_names, "\\.", "\\ ")
    igraph::V(comm.net)$color <- grDevices::adjustcolor("grey", alpha.f = .6)
    
    # Create the network plot
    graphics::par(mar = c(2, 2, 2, 2))
    net.plot <- plot(comm.net,
                     # layout = igraph::layout.davidson.harel(comm.net),
                     layout = igraph::layout.circle,
                     vertex.label.cex = 0.84,
                     vertex.frame.color = grDevices::adjustcolor("grey", alpha.f = .6),
                     vertex.shape = "circle",
                     vertex.label.family = "sans",
                     vertex.label.font = 3,
                     vertex.label.color = "black",
                     main = predictor_value)
    return(net.plot)
  }
  
  interaction_coefficients <- MRF_mod$graph
  
  #### Specify default parameter settings ####
  dimnames(interaction_coefficients) <- list(node_names, node_names)
  
  #### Extract indirect effect matrix that matches the covariate name ####
  indirect_coef_names <- names(MRF_mod$indirect_coefs)
  which_matrix_keep <- grepl(covariate, indirect_coef_names)
  covariate_matrix <- MRF_mod$indirect_coefs[which_matrix_keep]
  covariate_matrix <- as.matrix(covariate_matrix[[1]][[1]])
  baseinteraction_matrix <- as.matrix(MRF_mod$graph)
  
  #### Extract quantiles of observed values for the covariate ####
  observed_cov_values <- as.vector(data[[paste(covariate)]])
  observed_cov_unique <- as.numeric(unique(observed_cov_values, na.rm = T))
  
  #### Create a gridded plot object to plot the three networks
  graphics::par(mfrow = c(1, length(observed_cov_unique)), mar = c(2,2,2,2))
  cont.cov.mats <- lapply(observed_cov_unique, function(j){
    pred_values <- (covariate_matrix * j) + baseinteraction_matrix
    net.plot <- create_netgraph(matrix = pred_values, node_names = node_names, predictor_value = as.logical(j))
  })
}
png(filename = "figures/networks/mpa_grps.png", res = 150, width = 9.79, height = 7.38, units = "in")
plotMRF_net_factor(grps_mat, grps_pois, groupers, "mpa", "Groupers networks along MPAs")
dev.off()

png(filename = "figures/networks/mpa_dip.png", res = 150, width = 9.79, height = 7.38, units = "in")
plotMRF_net_factor(dip_mat, dip_pois, diplodus, "mpa", "Diplodus networks along MPAs")
dev.off()
# My graph:
# my_cols <- c(neg = '#3399CC', pos = '#FF3333')
# 
# graph <- MRF_mod$graph %>% tidygraph::as_tbl_graph()
# weights <- igraph::E(graph)$weight
# deg <- igraph::degree(graph, mode = "all")
# plot <- ggraph::ggraph(graph, layout = "circle") +
#   ggraph::geom_edge_link(aes(width = weights, color = weights < 0)) +
#   ggraph::scale_edge_width(range = c(0.1, 1)) +
#   ggraph::scale_edge_color_manual(values = c(my_cols[["neg"]], my_cols[["pos"]])) +
#   ggraph::geom_node_point(aes(size = deg, alpha = 0.5), col = "grey") +
#   ggraph::geom_node_text(aes(label = name), repel = TRUE, check_overlap = TRUE) +
#   ggplot::theme(legend.position = "none",
#         panel.background = element_blank(),
#         aspect.ratio = 1) +
#   ggplot::ggtitle(main)

