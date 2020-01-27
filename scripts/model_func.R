# load packages:
source("scripts/pckgs_preps.R")
library(todor)

# load MEData

# TODO update medata file (make sure no mistakes with the crete depth data)

med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double())) %>%
  mutate(tmean_reg = scale(tmean)) %>%  # TODO regularise depth covariate
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence so at the moment it's irrelevant
  mutate(loc = paste0(site, "_", trans))
str(med_raw)
pryr::object_size(med_raw)

# Define west/east basins ------ `basin` variable
west_basin <- list("France", "Italy", "Spain")
east_basin <- list("Croatia", "Greece", "Israel", "Malta", "Turkey")

# Define a vector that includes the names of the species of interest ------ `group` variable
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")

# Create a species matrix of a network group (defined earlier as vector) in east or west
group_basin_var <- med_raw %>%
  filter(country %in% basin) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(group, var) # var = MPA/temp/depth
str(group_basin_var)

# Run model ---- change the n_nodes value
grp_mod <- MRFcov(data = group_basin_var, n_nodes = 5, n_covariates = 1, family = "gaussian")

# Create bootstrapped model ---- change the n_nodes value
grp_boot <- bootstrap_MRF(data = group_basin_var, n_nodes = 5, n_covariates = 1, family = "gaussian", n_cores = 3)

# Predict log odds
grp_pred <- predict_MRF(data = group_basin_var, MRF_mod = grp_boot)
head(grp_pred)

## Graphs

# Define the levels of `var` to be reperesented in the graph ---- `val` variable
bottom <- min(group_basin_var$var)
centre <- median(group_basin_var$var)
top <- max(group_basin_var$var)

# Create a subset for each level
grp_var_value <- group_basin_var %>% filter(var, lower_lvl, lower_lvl + (((lower_lvl - upper_lvl)/100)*0.33))

# Run model and predict probablities of occurrence of each species in each site
grp_mod_val <- MRFcov(data = group_basin_var, n_nodes = 5, family = "gaussian")
grp_pred_val <- predict_MRF(data = group_basin_var, MRF_mod = grp_boot) %>% invlogit()
head(grp_pred_val)

grp_graph_val <- graph.adjacency(grp_mod_val$graph, weighted = T, mode = "undirected")
deg <- degree(grp_graph_val, mode = "all")
plot.igraph(grp_graph_val, layout = layout.circle(grp_graph_val),
            edge.width = abs(E(grp_graph_val)$weight),
            edge.color = ifelse(E(grp_graph_val)$weight < 0, '#3399CC', '#FF3333'), # blue = neg; red = pos
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = paste(grp_name,"network in", val, var, "(",basin,")"))

# 1. You need to pick a name for the function. 
# Here I’ve used rescale01 because this function rescales a vector to lie between 0 and 1.
# 2. You list the inputs, or arguments, to the function inside function. 
# Here we have just one argument. If we had more the call would look like function(x, y, z).
# 3. You place the code you have developed in body of the function,
# a { block that immediately follows function(...).
