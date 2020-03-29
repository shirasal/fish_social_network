
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

#########################################################################################################
# a) Extract species matrix:

# Look for infinite values
as.data.frame(func_1_result[[1]]) %>% unlist %>% is.infinite() %>% which()
# Remove rows with inf. values
spp_df <- func_1_result[[1]][!rowSums(!is.finite(func_1_result[[1]])),]

spp_mat <- as.matrix(spp_df)
nrow(spp_mat)
#########################################################################################################

# b) Extract coordinate dataframe:
coords <- func_1_result[[2]]

nrow(coords)
class(coords)
  

# Func 2
# Having problems with the coords df, I'll run just the MRFcov for now (results are similar anyway)
run_mod <- function(species_mat, n_covs, family){
  mod <- MRFcov(data = species_mat, n_nodes = ncol(species_mat) - n_covs,
                        n_covariates = n_covs, family = family)
  boot <- bootstrap_MRF(data = species_mat, n_nodes = ncol(species_mat) - n_covs,
                        n_covariates = n_covs, family = family)
  pred <- predict_MRF(data = species_mat, MRF_mod = boot) %>% invlogit()
  return(list(mod = mod, boot = boot, pred = pred))
}

model_temp <- run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian")
model_temp$mod
coef_temp <- model_temp$mod$direct_coefs
model_temp$boot
key_coef_temp <- model_temp$boot$mean_key_coefs
str(model_temp$pred)
predict_temp <- model_temp$pred


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
spp_mat <- create_spp_mat(dataset = med_raw, basin = east,
                          group = groupers, covariate = "enforcement")
str(spp_mat)

# Func 2
model_enf <- run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian")
coef_enf <- model_enf$mod$direct_coefs
key_coef_enf <- model_enf$boot$mean_key_coefs
predict_enf <- model_enf$pred

# Func 3b (no need for 3a because it's already categorised)
nested_cats <- nested_data(categorised_data = spp_mat)

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
# plot_multi_graphs(nested_df = nested_plot, n_graphs = 4)
# dev.off()

# Depth as covariate ------------------------------------------------------

# Func 1
spp_mat <- create_spp_mat(dataset = med_raw, basin = east,
                          group = groupers, covariate = "depth") %>% na.omit()
str(spp_mat)

# Func 2
model_dep <- run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian")
coef_dep <- model_dep$mod$direct_coefs
key_coef_dep <- model_dep$boot$mean_key_coefs
predict_dep <- model_dep$pred

# Func 3
categories <- categorise_cov(species_mat = spp_mat, covariate = "depth")
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
