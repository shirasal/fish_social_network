# load packages:
source("scripts/pckgs_preps.R")

# load MEData
med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double())) %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence so at the moment it's irrelevant
  mutate(loc = paste0(site, "_", trans), tmean_reg = scale(tmean)) # TODO regularise depth covariate
str(med_raw)
pryr::object_size(med_raw)

# Define `basin` variable
west <- list("France", "Italy", "Spain")
east <- list("Croatia", "Greece", "Israel", "Malta", "Turkey")

# Define `group` variable
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Scarus.ghobban", "Sparisoma.cretense")

# THINGS TO ADDRESS:
# 1. Categorical variable (4 levels, not 3)
# 2. Title for graph
# 3. Create a grid of graphs of the same covariate (different levels)
# 4. What happens if a species diesn't appear in the data in one of the basins (relevant mainly to herb grp)

my_model <- function(dataset, basin, group, covariate, family){
  # Step 1: Create species matrix for `group` in `basin` with 1 `covariate`
  species_mat <<- dataset %>%
    filter(country %in% basin) %>% 
    group_by(loc, species, tmean_reg, enforcement, depth) %>%
    summarise(n = sum(sp.n)) %>% 
    spread(species, n, fill = 0) %>% 
    as.data.frame() %>% 
    `rownames<-`(make.unique(.$loc)) %>%
    select(group, covariate)
  # Step 2: Run model and create occurrence predictions
  mod <<- MRFcov(data = species_mat, n_nodes = length(group), n_covariates = 1, family = family)
  boot <<- bootstrap_MRF(data = species_mat, n_nodes = length(group), n_covariates = 1, family = family)
  pred <<- predict_MRF(data = species_mat, MRF_mod = boot) %>% invlogit()
  # Step 3: Create graphs
    # 3a. Categorise continuous data
  species_mat_cat <- species_mat %>%
    mutate(category = cut(x = covariate,
                          breaks = c(-Inf,
                                     quantile(covariate, 0.33),
                                     quantile(covariate, 0.66),
                                     Inf),
                          labels = c("low", "med", "hi"))) %>% 
    group_by(category) %>%
    nest()
    # 3b. Run model on each category
  nested_data <- species_mat_cat %>%
    mutate(model = map(data, function(x) MRFcov(data = x,
                                                n_nodes = length(group),
                                                n_covariates = 1,
                                                family = "gaussian")))
    # 3c. Create graph
  sub_graph <- graph.adjacency(nested_data$model[[1]][[1]], weighted = T, mode = "undirected")
  deg <- degree(sub_graph, mode = "all")
  plot.igraph(sub_graph, layout = layout.circle(sub_graph),
              edge.width = abs(E(sub_graph)$weight),
              edge.color = ifelse(E(sub_graph)$weight < 0, '#3399CC', '#FF3333'),
              vertex.size = deg,
              vertex.label.family = "sans",
              vertex.label.font	= 3,
              vertex.label.cex = 1,
              vertex.label.color = adjustcolor("#333333", 0.85),
              vertex.color = adjustcolor("#FFFFFF", .5))
  
}

my_model(dataset = med_raw, basin = east, group = groupers, covariate = "tmean_reg", family = "gaussian")
head(pred)
str(species_mat)

## define group name
# grp_name <- as.character(colnames(groupers))



# fix the problem with arg `covariate`? -----------------------------------
dataset <- med_raw
basin <- east
group <- groupers
covariate <- med_raw$tmean_reg

species_mat <<- dataset %>%
  filter(country %in% basin) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(group, covariate)

# Error: `covariate` must evaluate to column positions or names, not a double vector


# - -----------------------------------------------------------------------

# Function 1: create matrix from subset of the main dataset ---------------

# group_basin_var <- med_raw %>%
#   filter(country %in% east) %>% 
#   group_by(loc, species, tmean_reg, enforcement) %>%
#   summarise(n = sum(sp.n)) %>% 
#   spread(species, n, fill = 0) %>% 
#   as.data.frame() %>% 
#   `rownames<-`(make.unique(.$loc)) %>%
#   select(groupers, tmean_reg)

## Function 2: Run model, bootstrapped model and model predictions ---------

# mrf_model <- function(data, n_nodes, n_covariates, family, MRF_mod){
#   mod <- MRFcov(data = species_mat, n_nodes = n_nodes, n_covariates = n_covariates, family = family)
#   boot <- bootstrap_MRF(data = species_mat, n_nodes = n_nodes, n_covariates = n_covariates, family = family)
#   pred <- predict_MRF(data = species_mat, MRF_mod = boot)
# }

# mod <- MRFcov(data = test, n_nodes = 5, n_covariates = 1, family = "gaussian")
# boot <- bootstrap_MRF(data = test, n_nodes = 5, n_covariates = 1, family = "gaussian")
# pred <- predict_MRF(data = test, MRF_mod = boot)

# mrf_model(data = test, n_nodes = 5, n_covariates = 1, family = "gaussian")


# Function 3: Create network graphs ---------------------------------------



