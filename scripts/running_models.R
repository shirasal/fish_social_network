
# Load packages and functions ---------------------------------------------

source("scripts/pckgs_preps.R")
# Also loads some variable to environemnts:
# basins: east, west, all_med 
# med_raw
source("scripts/model_func.R")

# GROUPERS ----------------------------------------------------------------

# Temperature as covariate ------------------------------------------------
group <- groupers
covariate <- "tmean_reg"

# Func 1
# a) Create species matrix:
spp_mat <- med_raw %>%
  group_by(lat, lon, loc, species, tmean_reg, enforcement, depth_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  filter(species %in% group) %>% 
  spread(species, n, fill = 0) %>% 
  ungroup() %>% 
  mutate(loc = make.unique(.$loc, "_")) %>% 
  column_to_rownames("loc") %>%
  select(group, covariate) %>% 
  as.matrix()
str(spp_mat)
nrow(spp_mat)

# b) Create a coordinate dataframe:
coords <- med_raw %>%
  group_by(lat, lon, loc, species, tmean_reg, enforcement, depth_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  filter(species %in% group) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(lat, lon)
nrow(coords)
class(coords)
  

# Func 2
species_mat <- spp_mat
n_covs <- 1
family <- "gaussian"

# Run model + Create predictions
mod <- MRFcov_spatial(data = species_mat, n_nodes = 5,
              n_covariates = n_covs, family = family, coords = coords, bootstrap = TRUE)
boot <- bootstrap_MRF(data = species_mat, n_nodes = 5,
                      n_covariates = n_covs, family = family)
pred <- predict_MRF(data = species_mat, MRF_mod = boot) %>% invlogit()
model_temp <- list(mod = mod, boot = boot, pred = pred)

# Func 3
covariate_vector <- species_mat[, 6]
categories <- species_mat %>%
  as_tibble() %>% 
  mutate(category = cut(x = covariate_vector,
                        breaks = c(-Inf,
                                   quantile(covariate_vector, 0.33),
                                   quantile(covariate_vector, 0.66),
                                   Inf),
                        labels = c("low", "med", "hi"),
                        ordered_result = TRUE))
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
