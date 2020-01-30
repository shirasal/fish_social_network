# load packages:
source("scripts/pckgs_preps.R")
library(todor)# not required

# load MEData
# TODO update medata file (make sure no mistakes with the crete depth data)

med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double())) %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence so at the moment it's irrelevant
  mutate(loc = paste0(site, "_", trans), tmean_reg = scale(tmean)) # TODO regularise depth covariate
str(med_raw)
pryr::object_size(med_raw)

# Define west/east basins = `basin` variable
west <- list("France", "Italy", "Spain")
east <- list("Croatia", "Greece", "Israel", "Malta", "Turkey")

# Define a vector that includes the names of the species of interest =`group` or `grp` variable
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")

# covs <- med_raw %>% select(tmean, sal_mean, pp_mean)
# dim(covs)[[2]]
# dim(med_raw$enforcement)[[2]]
# 
# med_raw$tmean_reg

# Function 1: create matrix from subset of the main dataset ----------------
shuster <- function(dataset, basin, group, covariate, family){
  species_mat <<- dataset %>%
    filter(country %in% basin) %>% 
    group_by(loc, species, tmean_reg, enforcement) %>%
    summarise(n = sum(sp.n)) %>% 
    spread(species, n, fill = 0) %>% 
    as.data.frame() %>% 
    `rownames<-`(make.unique(.$loc)) %>%
    select(group, covariate)
  
  mod <<- MRFcov(data = species_mat, n_nodes = length(group), n_covariates = 1, family = family)
  boot <<- bootstrap_MRF(data = species_mat, n_nodes = length(group), n_covariates = 1, family = family)
  pred <<- predict_MRF(data = species_mat, MRF_mod = boot) %>% invlogit()
  
}

shuster(dataset = med_raw, basin = east, group = groupers, covariate = "tmean_reg", family = "gaussian")
head(pred)
str(species_mat)

# Try one without the function
# group_basin_var <- med_raw %>%
#   filter(country %in% east) %>% 
#   group_by(loc, species, tmean_reg, enforcement) %>%
#   summarise(n = sum(sp.n)) %>% 
#   spread(species, n, fill = 0) %>% 
#   as.data.frame() %>% 
#   `rownames<-`(make.unique(.$loc)) %>%
#   select(groupers, tmean_reg)

# test = mrf_matrix(dataset = med_raw, basin = east, group = groupers, covariate = 'tmean_reg')

# Function 2: Run model, bootstrapped model and model predictions ---------

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

# Categorise covariate

species_mat_cat <- species_mat %>% mutate(category = cut(x = tmean_reg,
                                                          breaks = c(-Inf,
                                                                     quantile(tmean_reg, 0.33),
                                                                     quantile(tmean_reg, 0.66),
                                                                     Inf),
                                                          labels = c("low", "med", "hi")))


head(species_mat_cat)
# Check how many rows of each category
species_mat_cat %>% group_by(category) %>% nest()

# # define the level i want to subset for:
# temp <- "med" # this needs to be automated

################################### work from here ########################################
# --------------------------------------------------------------------------------------- #
sub_temp <- species_mat_cat %>% filter(category == temp) %>% select(-category)
sub_model <- MRFcov(data = sub_temp, n_nodes = 5, n_covariates = 1, family = "gaussian")
sub_graph <- graph.adjacency(sub_model$graph, weighted = T, mode = "undirected")
deg <- degree(sub_graph, mode = "all")
plot.igraph(sub_graph, layout = layout.circle(sub_graph),
            edge.width = abs(E(sub_graph)$weight),
            edge.color = ifelse(E(sub_graph)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = paste(temp, covariate))

# --------------------------------------------------------------------------------------- #


# create graph
net_mod <- MRFcov(data = species_mat_cat, n_nodes = length(group), family = "gaussian")
net_graph <- graph.adjacency(net_mod$graph, weighted = T, mode = "undirected")
deg <- degree(net_graph, mode = "all")
plot.igraph(net_graph, layout = layout.circle(net_graph),
            edge.width = abs(E(net_graph)$weight),
            edge.color = ifelse(E(net_graph)$weight < 0, '#3399CC', '#FF3333'), # blue = neg; red = pos
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5))

# define group name
# grp_name <- as.character(colnames(groupers))

