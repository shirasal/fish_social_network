# load packages:
source("scripts/pckgs_preps.R")

# Func 1: Create species matrix for `group` in `basin` with `covariate`
create_spp_mat <- function(dataset, basin, group, covariate){
  dataset %>%
    filter(country %in% basin) %>% 
    group_by(loc, species, tmean_reg, enforcement, depth) %>%
    summarise(n = sum(sp.n)) %>% 
    spread(species, n, fill = 0) %>% 
    as.data.frame() %>% 
    `rownames<-`(make.unique(.$loc)) %>%
    select(group, covariate)
}

# Func 2: Run model and create occurrence predictions
run_mod <- function(species_mat, n_covs, family){
  mod <- MRFcov(data = species_mat, n_nodes = ncol(species_mat) - n_covs,
                n_covariates = n_covs, family = family)
  boot <- bootstrap_MRF(data = species_mat, n_nodes = ncol(species_mat) - n_covs,
                        n_covariates = n_covs, family = family)
  pred <- predict_MRF(data = species_mat, MRF_mod = boot) %>% invlogit()
  return(list(mod = mod, boot = boot, pred = pred))
}

# Func 3: Categorise continuous data
categorise_cov <- function(species_mat, covariate){
  covariate_vector <- species_mat[[covariate]]
  species_mat %>%
    mutate(category = cut(x = covariate_vector,
                          breaks = c(-Inf,
                                     quantile(covariate_vector, 0.33),
                                     quantile(covariate_vector, 0.66),
                                     Inf),
                          labels = c("low", "med", "hi"),
                          ordered_result = TRUE)) %>% 
    arrange(category) %>% 
    group_by(category) %>%
    nest()
}

# Func 4: Run MRFcov model with some defaults
get_model <- function(data, ncov){
  MRFcov(data = data, n_nodes = ncol(data) - ncov, n_covariates = ncov, family = "gaussian")
}

# Func 5: Run model on each category
nested_models <- function(nested_df){
  nested_df %>%
    mutate(model = map(data, function(x) get_model(data = x, ncov = 1)))
}

# Func 6: Get graph data
get_graph <- function(model){
  graph.adjacency(model$graph, weighted = T, mode = "undirected")
}

# Func 7: Plot graphs
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

# Func 8: Plot multiple graphs on the same grid
plot_multi_graphs <- function(nested_df, n_graphs){
  layout(matrix(c(1:n_graphs), 1, n_graphs, byrow = TRUE))
  map2(.x = nested_df$plot, .y = nested_df$category, plotting_func)
}


