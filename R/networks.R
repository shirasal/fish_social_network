
# source("R/run_models.R")

# Func A: Categorise continuous data
categorise_cov <- function(species_mat, covariate){
  covariate_vector <- as.data.frame(species_mat)[[covariate]] 
  categories <- species_mat %>%
    as_tibble() %>% 
    mutate(category = cut(x = covariate_vector,
                          breaks = c(-Inf,
                                     quantile(covariate_vector, 0.33),
                                     quantile(covariate_vector, 0.66),
                                     Inf),
                          labels = c("low", "med", "hi"),
                          ordered_result = TRUE))
  
}


# Func B: Nest categorical (nominal) data
nested_data <- function(categorised_data, category_name) {
  if ("category" %in% colnames(categorised_data)){
    categorised_data %>%
      arrange(category) %>%
      group_by(category) %>%
      nest() %>% 
      return()
  } else {
    categorised_data %>%
      mutate(category = .[[category_name]]) %>%
      arrange(category) %>%
      group_by(category) %>%
      nest() %>% 
      return()
  } 
  
}


# Func C: Run model on each category ad calculate connectance
nested_models <- function(nested_df, ncov){
  nested_df %>%
    mutate(model = map(data, function(x) get_model(data = x, ncov = ncov)))
}

# Assisting function to nested_models (FuncC): Run MRFcov model with some defaults
get_model <- function(data, ncov){
  MRFcov(data = data, n_nodes = ncol(data) - ncov, n_covariates = ncov,
         family = "gaussian")
}






# Assisting function to Func D (get_plots)
plotting_func <- function(nested_model, taxa, covariate, category){
  sub_graph <- tidygraph::as_tbl_graph(nested_model$graph)
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
}

# Func D: store plots as objects within the nested dataframe
get_plots <- function(nested_model, taxa, covariate){
  nested_model %>% 
    mutate(plot = map2(.x = model, .y = category,
                       .f = function(x, y) plotting_func(nested_model = x,
                                                        category = y,
                                                        taxa = taxa,
                                                        covariate = covariate)))
}





# -------------------------------------------------------------------------

grps_categorised <- categorise_cov(species_mat = grps_mat, covariate = "temp") # add another column of 'category' according to 'covariate'
grps_nested <- nested_data(categorised_data = grps_categorised) # nest each data that belongs to a category in a list within a tibble of categories
grps_nested_mod <- nested_models(nested_df = grps_nested, ncov = 5) # run model for each category
grps_net_plots <- get_plots(nested_model = grps_nested_mod, taxa = "Groupers", covariate = "Temperature")
# Plot all graphs on one panel:
grps_plot <- ggpubr::ggarrange(plotlist = grps_net_plots$plot, ncol = 3)
ggsave(plot = grps_plot, path = "C:/Users/Shira/Documents/MSc/Results/Networks",
       filename = "grps_nets.pdf", dpi = 300, width = 12)
# file.show('C:/Users/Shira/Documents/MSc/Results/Networks/grps_nets.pdf')

dip_categorised <- categorise_cov(species_mat = dip_mat, covariate = "temp") # add another column of 'category' according to 'covariate'
dip_nested <- nested_data(categorised_data = dip_categorised) # nest each data that belongs to a category in a list within a tibble of categories
dip_nested_mod <- nested_models(nested_df = dip_nested, ncov = 5) # run model for each category
dip_net_plots <- get_plots(nested_model = dip_nested_mod, taxa = "Seabreams", covariate = "Temperature")
# Plot all graphs on one panel:
dip_plot <- ggpubr::ggarrange(plotlist = dip_net_plots$plot, ncol = 3)
ggsave(plot = dip_plot, path = "C:/Users/Shira/Documents/MSc/Results/Networks",
       filename = "dip_nets.pdf", dpi = 300, width = 12)

herb_categorised <- categorise_cov(species_mat = herb_mat, covariate = "temp") # add another column of 'category' according to 'covariate'
herb_nested <- nested_data(categorised_data = herb_categorised) # nest each data that belongs to a category in a list within a tibble of categories
herb_nested_mod <- nested_models(nested_df = herb_nested, ncov = 5) # run model for each category
herb_net_plots <- get_plots(nested_model = herb_nested_mod, taxa = "Herbivores", covariate = "Temperature")
# Plot all graphs on one panel:
herb_plot <- ggpubr::ggarrange(plotlist = herb_net_plots$plot, ncol = 3)
ggsave(plot = herb_plot, path = "C:/Users/Shira/Documents/MSc/Results/Networks",
       filename = "herb_nets.pdf", dpi = 300, width = 12)



grps_mpa_nested <- nested_data(categorised_data = grps_mat, category_name = "mpa")
grps_mpa_nested_mod <- nested_models(nested_df = grps_mpa_nested, ncov = 5) # run model for each category
grps_mpa_net_plots <- get_plots(nested_model = grps_mpa_nested_mod, taxa = "Groupers", covariate = "MPA")
# Plot all graphs on one panel:
grps_mpa_plot <- ggpubr::ggarrange(plotlist = grps_mpa_net_plots$plot, ncol = 2)
ggsave(plot = grps_mpa_plot, path = "C:/Users/Shira/Documents/MSc/Results/Networks",
       filename = "grps_nets_mpa.pdf", dpi = 300, width = 8)
# file.show('C:/Users/Shira/Documents/MSc/Results/Networks/grps_nets_mpa.pdf')

dip_mpa_nested <- nested_data(categorised_data = dip_mat, category_name = "mpa")
dip_mpa_nested_mod <- nested_models(nested_df = dip_mpa_nested, ncov = 5) # run model for each category
dip_mpa_net_plots <- get_plots(nested_model = dip_mpa_nested_mod, taxa = "Seabreams", covariate = "MPA")
# Plot all graphs on one panel:
dip_mpa_plot <- ggpubr::ggarrange(plotlist = dip_mpa_net_plots$plot, ncol = 2)
ggsave(plot = dip_mpa_plot, path = "C:/Users/Shira/Documents/MSc/Results/Networks",
       filename = "dip_nets_mpa.pdf", dpi = 300, width = 8)

herb_mpa_nested <- nested_data(categorised_data = herb_mat, category_name = "mpa")
herb_mpa_nested_mod <- nested_models(nested_df = herb_mpa_nested, ncov = 5) # run model for each category
herb_mpa_net_plots <- get_plots(nested_model = herb_mpa_nested_mod, taxa = "Herbivores", covariate = "MPA")
# Plot all graphs on one panel:
herb_mpa_plot <- ggpubr::ggarrange(plotlist = herb_mpa_net_plots$plot, ncol = 2)
ggsave(plot = herb_mpa_plot, path = "C:/Users/Shira/Documents/MSc/Results/Networks",
       filename = "herb_nets_mpa.pdf", dpi = 300, width = 8)

