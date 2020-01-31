
# Load packages and functions ---------------------------------------------

source("scripts/pckgs_preps.R")
source("scripts/model_func.R")


# Load raw data -----------------------------------------------------------

med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double())) %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence so at the moment it's irrelevant
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
# 1. Categorical variable (4 levels, not 3)
# 2. What happens if a species doesn't appear in the data in one of the basins (relevant mainly to herb grp)



# Run functions on temperature data ---------------------------------------

# Func 1
spp_mat <- create_spp_mat(dataset = med_raw, basin = east,
                          group = groupers, covariate = "tmean_reg")
str(spp_mat)

# Func 2
model_temp <- run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian")
model_temp$mod
model_temp$boot
str(model_temp$pred)

# Func 3
categories <- categorise_cov(species_mat = spp_mat, covariate = "tmean_reg")

# Func 4
cat_model <- get_model(data = spp_mat, ncov = 1)
cat_model

# Func 5
nested <- nested_models(nested_df = categories)

# Func 6
nested_data <- nested %>% mutate(plot = map(model, get_graph))

# Func 8
temperature_graph <- plot_multi_graphs(nested_df = nested_data,n_graphs = 3)


# Run functions on enforcement --------------------------------------------

# Func 1
spp_mat <- create_spp_mat(dataset = med_raw, basin = east,
                          group = groupers, covariate = "enforcement")
str(spp_mat)

# Func 2
model_temp <- run_mod(species_mat = spp_mat, n_covs = 1, family = "gaussian")
model_temp$mod
model_temp$boot
str(model_temp$pred)

# Func 3
categories <- categorise_cov(species_mat = spp_mat, covariate = "enforcement")

# Func 4
cat_model <- get_model(data = spp_mat, ncov = 1)
cat_model
# FIXME categories are defined by the covariate (categorical in this case)

# Func 5
nested <- nested_models(nested_df = categories)

# Func 6
nested_data <- nested %>% mutate(plot = map(model, get_graph))

# Func 8
MPAs_graph <- plot_multi_graphs(nested_df = nested_data,n_graphs = 3)


