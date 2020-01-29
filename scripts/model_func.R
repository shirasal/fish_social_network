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


# Function 1: create matrix from subset of the main dataset ----------------
mrf_matrix <- function(dataset, basin, group, covariate){
  species_mat <- dataset %>%
    filter(country %in% basin) %>% 
    group_by(loc, species, tmean_reg, enforcement) %>%
    summarise(n = sum(sp.n)) %>% 
    spread(species, n, fill = 0) %>% 
    as.data.frame() %>% 
    `rownames<-`(make.unique(.$loc)) %>%
    select(group, covariate)
}

# Try one without the function
group_basin_var <- med_raw %>%
  filter(country %in% east) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers, tmean_reg)

mrf_subset(med_raw, east, groupers, "tmean_reg")
str(group_basin_var)


# Function 2: Run model, bootstrapped model and model predictions ---------

mrf_model <- function(data, n_nodes, n_covariates, family, MRF_mod){
  mod <- MRFcov(data = group_basin_var, n_nodes = n_nodes, n_covariates = n_covariates, family = family)
  boot <- bootstrap_MRF(data = group_basin_var, n_nodes = n_nodes, n_covariates = n_covariates, family = family)
  pred <- predict_MRF(data = group_basin_var, MRF_mod = boot)
}

head(pred)


# Function 3: Create network graphs ---------------------------------------

# Define the levels of `covariate` to be reperesented in the graph ---- `val` variable
bottom <- min(group_basin_var$covariate)
centre <- median(group_basin_var$covariate)
top <- max(group_basin_var$covariate)

# Create a subset for each level
val <- group_basin_var %>% filter(covariate, lower_lvl, lower_lvl + (((lower_lvl - upper_lvl)/100)*0.33))

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
            main = paste(grp_name,"network in", val, covariate, "(",basin,")"))






