
# Load packages and functions ---------------------------------------------

source("scripts/pckgs_preps.R")
# Also loads some variable to environemnts:
# basins: east, west, all_med 
# med_raw
source("scripts/model_func.R")

# GROUPERS ----------------------------------------------------------------

# Temperature as covariate ------------------------------------------------

# Func 1
func_1_result <- create_spp_mat(dataset = med_raw, basin = all_med,
                                group = groupers, covariate = "tmean_reg")

# a) Extract species matrix
spp_mat <- func_1_result[[1]]
nrow(spp_mat)

# b) Extract coordinate dataframe:
coords <- func_1_result[[2]]
nrow(coords)

# Func 2
model_temp <- run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian", coords = coords)


# Func 3
categories <- categorise_cov(species_mat = spp_mat, covariate = "tmean_reg")
nested_cats <- nested_data(categorised_data = categories)

# Func 4
cat_model <- get_model(data = spp_mat, ncov = 1)
cat_model

# Func 5
nested <- nested_models(nested_df = nested_cats)

# Func 6
nested_plot <- nested %>% mutate(plot = map(model, get_graph))

# Func 8
temperature_graph <- plot_multi_graphs(nested_df = nested_plot, n_graphs = 3)

# png("figures/groupers_allmed_temp_spatial.png", height = 450, width = 1200, res = 150)
# plot_multi_graphs(nested_df = nested_plot, n_graphs = 3)
# dev.off()


# MPA as covariate --------------------------------------------------------

# Func 1
func_1_result <- create_spp_mat(dataset = med_raw, basin = all_med,
                                group = groupers, covariate = "mpa")

# a) Extract species matrix
spp_mat <- func_1_result[[1]]
nrow(spp_mat)

# b) Extract coordinate dataframe:
coords <- func_1_result[[2]]
nrow(coords)

# Func 2
model_enf <-  run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian", coords = coords)
coef_enf <- model_enf$mod$direct_coefs
key_coef_enf <- model_enf$boot$mean_key_coefs
predict_enf <- model_enf$pred

# Func 3b (no need for 3a because it's already categorised)
nested_cats <- nested_data(categorised_data = as.data.frame(spp_mat))

# Func 4
cat_model <- get_model(data = spp_mat, ncov = 1)
cat_model

# Func 5
nested <- nested_models(nested_df = nested_cats)

# Func 6
nested_plot <- nested %>% mutate(plot = map(model, get_graph))

# Func 8
MPAs_graph <- plot_multi_graphs(nested_df = nested_plot, n_graphs = 4) 

# png("figures/groupers_allmed_mpa_spatial.png", height = 450, width = 1600, res = 150)
# plot_multi_graphs(nested_df = nested_plot, n_graphs = 2)
# dev.off()

# Depth as covariate ------------------------------------------------------

# Func 1
# Func 1
func_1_result <- create_spp_mat(dataset = med_raw, basin = all_med,
                                group = groupers, covariate = "depth_reg")

# a) Extract species matrix
spp_mat <- func_1_result[[1]]
nrow(spp_mat)

# b) Extract coordinate dataframe:
coords <- func_1_result[[2]]
nrow(coords)

# Func 2
model_dep <- run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian", coords = coords)
coef_dep <- model_dep$mod$direct_coefs
key_coef_dep <- model_dep$boot$mean_key_coefs
predict_dep <- model_dep$pred

# Func 3
categories <- categorise_cov(species_mat = spp_mat, covariate = "depth_reg")
nested_cats <- nested_data(categorised_data = categories)

# Func 4
cat_model <- get_model(data = spp_mat, ncov = 1)
cat_model

# Func 5
nested <- nested_models(nested_df = nested_cats)

# Func 6
nested_plot <- nested %>% mutate(plot = map(model, get_graph))

# Func 8
depth_graph <- plot_multi_graphs(nested_df = nested_plot, n_graphs = 3)

# png("figures/groupers_allmed_depth_spatial.png", height = 450, width = 1200, res = 150)
# plot_multi_graphs(nested_df = nested_plot, n_graphs = 3)
# dev.off()
