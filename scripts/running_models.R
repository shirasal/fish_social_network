
# Load packages and functions ---------------------------------------------

source("scripts/pckgs_preps.R")
source("scripts/model_func.R")

# Load raw data -----------------------------------------------------------

med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = "d")) %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(loc = paste0(site, "_", trans), tmean_reg = scale(tmean)) # TODO regularise depth covariate
str(med_raw)
pryr::object_size(med_raw)


# Define variables `basin`; `group` ---------------------------------------
west <- list("France", "Italy", "Spain")
east <- list("Croatia", "Greece", "Israel", "Malta", "Turkey")

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Scarus.ghobban", "Sparisoma.cretense")

# TODO THINGS TO ADDRESS:

# 2. Add title to graph grid
# 3. What happens if a species doesn't appear in the data in one of the basins (relevant mainly to herb grp)


# GROUPERS ----------------------------------------------------------------

# EASTERN BASIN -----------------------------------------------------------

# Temperature as covariate ------------------------------------------------

# Func 1
spp_mat <- create_spp_mat(dataset = med_raw, basin = east,
                          group = groupers, covariate = "tmean_reg")
str(spp_mat)

# Func 2
model_temp <- run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian")
coef_temp <- model_temp$mod$direct_coefs
key_coef_temp <- model_temp$boot$mean_key_coefs
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

# png("figures/groupers_east_temp.png", height = 450, width = 1200, res = 150)
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

# png("figures/groupers_east_mpa.png", height = 450, width = 1600, res = 150)
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

# png("figures/groupers_east_depth.png", height = 450, width = 1200, res = 150)
# plot_multi_graphs(nested_df = nested_plot, n_graphs = 3)
# dev.off()
