
source("scripts/pckgs_preps.R")
source("scripts/model_func.R")

## Create matrices --------------------------------------------------------
# Temperature and depth ---------------------------------------------------
grps_temp_dep <- create_spp_mat(dataset = med_raw, basin = all_med,
                                group = groupers, covariate = c("tmean_reg", "depth_reg"))
grps_temp_dep_mat <- grps_temp_dep[[1]]
grps_temp_dep_coords <- grps_temp_dep[[2]]
rm(grps_temp_dep)
# Temperature -------------------------------------------------------------
grps_temp <- create_spp_mat(dataset = med_raw, basin = all_med,
                            group = groupers, covariate = "tmean_reg")
grps_temp_mat <- grps_temp[[1]]
grps_temp_coords <- grps_temp[[2]]
rm(grps_temp)
# MPAs and depth ----------------------------------------------------------
grps_mpa_dep <- create_spp_mat(dataset = med_raw, basin = all_med,
                               group = groupers, covariate = c("mpa", "depth_reg"))
grps_mpa_dep_mat <- grps_mpa_dep[[1]]
grps_mpa_dep_coords <- grps_mpa_dep[[2]]
rm(grps_mpa)
# MPAs --------------------------------------------------------------------
grps_mpa <- create_spp_mat(dataset = med_raw, basin = all_med,
                           group = groupers, covariate = "mpa")
grps_mpa_mat <- grps_mpa[[1]]
grps_mpa_coords <- grps_mpa[[2]]
rm(grps_mpa)

## Run models -------------------------------------------------------------

# Temperature and depth ---------------------------------------------------

grps_temp_dep_model <- run_mod(species_mat = grps_temp_dep_mat, n_covs = 2,
                           family = "gaussian", coords = grps_temp_dep_coords)
plotMRF_hm(grps_temp_dep_model$mod, main = "Groupers and temperature (incl. depth) - association coefficients")

# MPAs and depth ----------------------------------------------------------

grps_mpa_dep_model <- run_mod(species_mat = grps_mpa_dep_mat, n_covs = 2,
                          family = "gaussian", coords = grps_mpa_dep_coords)
plotMRF_hm(grps_mpa_dep_model$mod, main = "Groupers and MPAs (incl. depth) - association coefficients")


## Networks ---------------------------------------------------------------

# Temperature -------------------------------------------------------------
# without depth
grps_temp_cats <- categorise_cov(species_mat = grps_temp_mat, covariate = "tmean_reg")
grps_temp_cat_nested <- nested_data(categorised_data = grps_temp_cats)
grps_temp_cat_model <- get_model(data = grps_temp_mat, ncov = 1)
grps_temp_nested_mod <- nested_models(nested_df = grps_temp_cat_nested)
grps_temp_nested_plot <- grps_temp_nested_mod %>% mutate(plot = map(model, get_graph))
grps_temp_graph <- plot_multi_graphs(nested_df = grps_temp_nested_plot, n_graphs = 3)

# Connectance 
(grps_temp_connect <- tibble(connectance = unlist(grps_temp_nested_mod$connectance)) %>% 
    add_column(network = factor(c("low", "medium", "high"))) %>% 
    select(network, connectance))

# MPAs --------------------------------------------------------------------
# without depth
grps_mpa_cat_nested <- nested_data(categorised_data = as.data.frame(grps_mpa_mat))
grps_mpa_cat_model <- get_model(data = grps_mpa_mat, ncov = 1)
grps_mpa_nested_mod <- nested_models(nested_df = grps_mpa_cat_nested)
grps_mpa_nested_plot <- grps_mpa_nested_mod %>% mutate(plot = map(model, get_graph))
grps_mpa_graph <- plot_multi_graphs(nested_df = grps_mpa_nested_plot, n_graphs = 2)

# Connectance 
(grps_mpa_connect <- tibble(connectance = unlist(grps_mpa_nested_mod$connectance)) %>% 
  add_column(network = factor(c("outside MPA", "inside MPA"))) %>% 
  select(network, connectance))


## Stationarity -----------------------------------------------------------
# Relative importance
grps_temp_relimp <- sapply(X = grps_temp_dep_model$mod$key_coefs, rel_imp, cov = ("tmean_reg")) %>% 
  tibble::enframe() %>% 
  rename("species" = name, "temp_model" = value)

grps_mpa_relimp <- sapply(X = grps_mpa_dep_model$mod$key_coefs, rel_imp, cov = "mpa") %>% 
  tibble::enframe() %>% 
  rename("species" = name, "mpa_model" = value)

(grps_relimp <- grps_temp_relimp %>%
  left_join(grps_mpa_relimp, by = "species"))

# Mean coefficients
grps_temp_meancoefs <- sapply(X = grps_temp_dep_model$mod$key_coefs, FUN = mean_coef) %>%
  tibble::enframe() %>% 
  rename("species" = name, "sum_mean_coefs_temp" = value)

grps_mpa_meancoefs <- sapply(X = grps_mpa_dep_model$mod$key_coefs, FUN = mean_coef) %>%
  tibble::enframe() %>% 
  rename("species" = name, "sum_mean_coefs_mpa" = value)

(grps_spp_meancoef <- grps_temp_meancoefs %>%
  left_join(grps_mpa_meancoefs, by = "species"))
