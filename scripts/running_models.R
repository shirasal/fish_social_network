
# Load packages and functions ---------------------------------------------

source("scripts/pckgs_preps.R")
source("scripts/model_func.R")

# Load raw data -----------------------------------------------------------

med_raw <- read_rds("data/medata.Rds") %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(loc = paste0(site, "_", trans),
         tmean_reg = scale(tmean),
         depth_reg = scale(depth))
# TODO log abundances?
# TODO merge protection levels - PROTECTED(2-3)/NOT_PROTECTED(0-1)
str(med_raw)
pryr::object_size(med_raw)

med_coords <- med_raw %>% select(loc, lat, lon)

# Define variables `basin`; `group` ---------------------------------------
west <- list("France", "Italy", "Spain")
east <- list("Croatia", "Greece", "Israel", "Malta", "Turkey")
all_med <- list(unique(med_raw$country))

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Scarus.ghobban", "Sparisoma.cretense")

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



# ### Spatial ### ---------------------------------------------------------


# GROUPERS ----------------------------------------------------------------

# Whole Med ---------------------------------------------------------------

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
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
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
categories <- categorise_cov(species_mat = species_mat, covariate = "tmean_reg")
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

