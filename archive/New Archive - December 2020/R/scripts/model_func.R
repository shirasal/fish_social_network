# load packages:
source("scripts/pckgs_preps.R")

# TODO create defaults --- try 'missing()':
# FUNC 1: basin = all_med
# FUNC 2: n_covs = 1

# Func 1: Create species matrix and coordinate dataframe

create_spp_mat <- function(dataset, basin, group, covariate){
  # Create species matrix for `group` in `basin` with `covariate`
  dataset %>%
    filter(country %in% basin) %>% 
    group_by(lat, lon, site, trans, species, tmean_reg, mpa, depth_reg) %>%
    summarise(n = sum(sp.n)) %>% 
    spread(species, n, fill = 0) %>% 
    sample_n(size = 1) %>% 
    ungroup() %>% 
    na.omit() %>% 
    mutate(loc = paste(site, trans)) %>% 
    column_to_rownames("loc") %>%
    select(all_of(group), all_of(covariate)) %>% 
    as.matrix()
}


# Func 2: Run model and create occurrence predictions
run_mod <- function(species_mat, n_covs, family){
  mod <- MRFcov(data = species_mat, n_nodes = ncol(species_mat) - n_covs,
                        n_covariates = n_covs, family = family,
                        n_cores = parallel::detectCores())
  boot <- bootstrap_MRF(data = species_mat, n_nodes = ncol(species_mat) - n_covs,
                        n_covariates = n_covs, family = family, spatial = FALSE,
                        n_cores = parallel::detectCores())
  pred <- predict_MRF(data = species_mat, MRF_mod = boot) %>% invlogit()
  return(list(mod = mod, boot = boot, pred = pred))
}

# Func 3: Categorise continuous data
categorise_cov <- function(species_mat, covariate){
  # data
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



# Func 3b: Nest categorical (nominal) data
nested_data <- function(categorised_data, ncov) {
  if (ncov > 1){
    if ("category" %in% colnames(categorised_data)){
      categorised_data %>%
        arrange(category) %>%
        group_by(category) %>%
        nest() %>% 
        return()
    } else {
      categorised_data %>%
        mutate(category = as.factor(.[, ncol(.)-1])) %>%
        arrange(category) %>%
        group_by(category) %>%
        nest() %>% 
        return()
    } 
  } else {
    if ("category" %in% colnames(categorised_data)){
      categorised_data %>%
        arrange(category) %>%
        group_by(category) %>%
        nest() %>% 
        return()
    } else {
      categorised_data %>%
        mutate(category = as.factor(.[, ncol(.)])) %>%
        arrange(category) %>%
        group_by(category) %>%
        nest() %>% 
        return()
    } 
  }
}

# Func 4: Run MRFcov model with some defaults
get_model <- function(data, ncov){
  MRFcov(data = data, n_nodes = ncol(data) - ncov, n_covariates = ncov,
                 family = "gaussian")
}

# Assistance function: Get the connectance value
connectance <- function(x){
  x[x != 0] = 1
  L <- sum(x)/2 # divided by 2 because it is calculated with the whole data, including the diagonal
  M <- (nrow(x))^2
  connect <- L/M 
  print(connect)
  
}

# Func 5: Run model on each category ad calculate connectance
nested_models <- function(nested_df, ncov){
  nested_df %>%
    mutate(model = map(data, function(x) get_model(data = x, ncov = ncov)),
           connectance = map(model, function(x) connectance(x$graph)))
}

# Func 6: Get graph data
get_graph <- function(model){
  graph.adjacency(model$graph, weighted = T, mode = "undirected")
}

# Assisting function for plotting graphs: Plot a graph
plotting_func <- function(igraph, category){
  sub_graph <- igraph
  deg <- degree(sub_graph, mode = "all")
  plot <- plot.igraph(sub_graph, layout = layout.circle(sub_graph),
                      edge.width = scale(abs(E(sub_graph)$weight)),
                      edge.color = ifelse(E(sub_graph)$weight < 0, '#3399CC', '#FF3333'),
                      vertex.size = deg,
                      vertex.label.family = "sans",
                      vertex.label.font	= 3,
                      vertex.label.cex = 1,
                      vertex.label.color = adjustcolor("#333333", 0.85),
                      vertex.color = adjustcolor("#FFFFFF", .5),
                      main = category)
}

# Func 7: Plot multiple graphs on the same grid
plot_multi_graphs <- function(nested_df, n_graphs){
  layout(matrix(c(1:n_graphs), 1, n_graphs, byrow = TRUE), respect = TRUE)
  map2(.x = nested_df$plot, .y = nested_df$category, plotting_func)
}


# Function to calculate the sum of effect (relative importance) of species in the model
rel_imp <- function(i, cov){
  i %>% 
    filter(str_detect(string = .$Variable,
                      pattern = cov, negate = TRUE)) %>% 
    .$Rel_importance %>% 
    sum()
}

# Calculate the sum of mean_coefficient, see the general direction
mean_coef <- function(j){
  j %>% 
    .$Standardised_coef %>% 
    sum()
}


