source("scripts/pckgs_preps.R")

# load MEData
med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double())) %>%
  mutate(tmean_reg = scale(tmean)) # regularise temperature covariate
str(med_raw)

# (min_temp <- min(med_raw$tmean)) # 16.8
# (med_temp <- median(med_raw$tmean)) # 20
# (max_temp <- max(med_raw$tmean)) # 23


### Network 1a: Groupers and combers + temperatures
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")

# Create a species matrix of 'groupers'
Serranidae <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers[1:5], tmean_reg)
str(Serranidae)

serr_mod <- MRFcov(data = Serranidae, n_nodes = 5, n_covariates = 1, family = "gaussian")
serr_boot <- bootstrap_MRF(data = Serranidae, n_nodes = 5, n_covariates = 1, family = "gaussian", n_cores = 3)
serr_pred <- predict_MRF(data = Serranidae, MRF_mod = serr_boot)
head(serr_pred)

## Graphs

# min(Serranidae$tmean_reg) # -1.62
# median(Serranidae$tmean_reg) # 0.18
# max(Serranidae$tmean_reg) # 1.49

# Subset temperature data: low
Serr_temp_low <- Serranidae %>% filter(between(tmean_reg, -1.62, 0))

serr_mod_low <- MRFcov(data = Serr_temp_low, n_nodes = 5, family = "gaussian")
serr_pred_low <- predict_MRF(data = Serr_temp_low, MRF_mod = serr_boot) %>% invlogit()
head(serr_pred_low) # probabilities

serr_graph_low <- graph.adjacency(serr_mod_low$graph, weighted = T, mode = "undirected")
deg <- degree(serr_graph_low, mode = "all")
plot.igraph(serr_graph_low, layout = layout.circle(serr_graph_low),
            edge.width = abs(E(serr_graph_low)$weight),
            edge.color = ifelse(E(serr_graph_low)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Serranidae network in low temperatures")

# Subset temperature data: med
Serr_temp_med <- Serranidae %>% filter(between(tmean_reg, 0.00001, 1))

serr_mod_med <- MRFcov(data = Serr_temp_med, n_nodes = 5, family = "gaussian")
serr_pred_med <- predict_MRF(data = Serr_temp_med, MRF_mod = serr_boot) %>% invlogit()
head(serr_pred_med) # probabilities

serr_graph_med <- graph.adjacency(serr_mod_med$graph, weighted = T, mode = "undirected")
deg <- degree(serr_graph_med, mode = "all")
plot.igraph(serr_graph_med, layout = layout.circle(serr_graph_med),
            edge.width = abs(E(serr_graph_med)$weight),
            edge.color = ifelse(E(serr_graph_med)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Serranidae network in median temperatures")

# Subset temperature data: high
Serr_temp_hi <- Serranidae %>% filter(between(tmean_reg, 1.00001, 1.49))

serr_mod_hi <- MRFcov(data = Serr_temp_hi, n_nodes = 5, family = "gaussian")
serr_pred_hi <- predict_MRF(data = Serr_temp_hi, MRF_mod = serr_boot) %>% invlogit()
head(serr_pred_hi) # probabilities

serr_graph_hi <- graph.adjacency(serr_mod_hi$graph, weighted = T, mode = "undirected")
deg <- degree(serr_graph_hi, mode = "all")
plot.igraph(serr_graph_hi, layout = layout.circle(serr_graph_hi),
            edge.width = abs(E(serr_graph_hi)$weight),
            edge.color = ifelse(E(serr_graph_hi)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Serranidae network in high temperatures")




### Network 1b: Groupers and combers + MPAs

# Create a species matrix of 'groupers'
Serranidae <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers[1:5], enforcement)
str(Serranidae)

## Graphs

# Subset MPA data: enforcement == 3
Serr_MPA <- Serranidae %>% filter(enforcement == 3)

serr_mod_MPA <- MRFcov(data = Serr_MPA, n_nodes = 5, family = "gaussian")
serr_pred_MPA <- predict_MRF(data = Serr_MPA, MRF_mod = serr_boot) %>% invlogit()
head(serr_pred_MPA) # probabilities

serr_graph_MPA <- graph.adjacency(serr_mod_MPA$graph, weighted = T, mode = "undirected")
deg <- degree(serr_graph_MPA, mode = "all")
plot.igraph(serr_graph_MPA, layout = layout.circle(serr_graph_MPA),
            edge.width = abs(E(serr_graph_MPA)$weight),
            edge.color = ifelse(E(serr_graph_MPA)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Serranidae network inside MPAs")

# Subset MPA data: enforcement < 3
Serr_noMPA <- Serranidae %>% filter(enforcement != 3)

serr_mod_noMPA <- MRFcov(data = Serr_noMPA, n_nodes = 5, family = "gaussian")
serr_pred_noMPA <- predict_MRF(data = Serr_noMPA, MRF_mod = serr_boot) %>% invlogit()
head(serr_pred_noMPA) # probabilities

serr_graph_noMPA <- graph.adjacency(serr_mod_noMPA$graph, weighted = T, mode = "undirected")
deg <- degree(serr_graph_noMPA, mode = "all")
plot.igraph(serr_graph_noMPA, layout = layout.circle(serr_graph_noMPA),
            edge.width = abs(E(serr_graph_noMPA)$weight),
            edge.color = ifelse(E(serr_graph_noMPA)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Serranidae network outside MPAs")

################################################################################################################################
################################################################################################################################

### Network 2a: Diplodus + temperatures
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")

# Create a species matrix of 'diplodus'
Diplodus <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(diplodus[1:5], tmean_reg)

dip_mod <- MRFcov(data = Diplodus, n_nodes = 5, family = "gaussian")
dip_boot <- bootstrap_MRF(data = Diplodus, n_nodes = 5, n_covariates = 1, family = "gaussian", n_cores = 3)
dip_pred <- predict_MRF(data = Diplodus, MRF_mod = dip_boot)
head(dip_pred)

## Graphs

# min(Diplodus$tmean_reg) # -1.62
# median(Diplodus$tmean_reg) # 0.18
# max(Diplodus$tmean_reg) # 1.49

# Subset temperature data: low
Dip_temp_low <- Diplodus %>% filter(between(tmean_reg, -1.62, 0))

dip_mod_low <- MRFcov(data = Dip_temp_low, n_nodes = 5, family = "gaussian")
dip_pred_low <- predict_MRF(data = Dip_temp_low, MRF_mod = dip_boot) %>% invlogit()
head(dip_pred_low) # probabilities

dip_graph_low <- graph.adjacency(dip_mod_low$graph, weighted = T, mode = "undirected")
deg <- degree(dip_graph_low, mode = "all")
plot.igraph(dip_graph_low, layout = layout.circle(dip_graph_low),
            edge.width = abs(E(dip_graph_low)$weight),
            edge.color = ifelse(E(dip_graph_low)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Diplodus network in low temperatures")

# Subset temperature data: med
Dip_temp_med <- Diplodus %>% filter(between(tmean_reg, 0.00001, 1))

dip_mod_med <- MRFcov(data = Dip_temp_med, n_nodes = 5, family = "gaussian")
dip_pred_med <- predict_MRF(data = Dip_temp_med, MRF_mod = dip_boot) %>% invlogit()
head(dip_pred_med) # probabilities

dip_graph_med <- graph.adjacency(dip_mod_med$graph, weighted = T, mode = "undirected")
deg <- degree(dip_graph_med, mode = "all")
plot.igraph(dip_graph_med, layout = layout.circle(dip_graph_med),
            edge.width = abs(E(dip_graph_med)$weight),
            edge.color = ifelse(E(dip_graph_med)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Diplodus network in median temperatures")

# Subset temperature data: high
Dip_temp_hi <- Diplodus %>% filter(between(tmean_reg, 1.00001, 1.49))

dip_mod_hi <- MRFcov(data = Dip_temp_hi, n_nodes = 5, family = "gaussian")
dip_pred_hi <- predict_MRF(data = Dip_temp_hi, MRF_mod = dip_boot) %>% invlogit()
head(dip_pred_hi) # probabilities

dip_graph_hi <- graph.adjacency(dip_mod_hi$graph, weighted = T, mode = "undirected")
deg <- degree(dip_graph_hi, mode = "all")
plot.igraph(dip_graph_hi, layout = layout.circle(dip_graph_hi),
            edge.width = abs(E(dip_graph_hi)$weight),
            edge.color = ifelse(E(dip_graph_hi)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Diplodus network in high temperatures")




### Network 2b: Diplodus + MPAs

# Create a species matrix of 'groupers'
Diplodus <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(diplodus[1:5], enforcement)
str(Diplodus)

## Graphs

# Subset MPA data: enforcement == 3
Dip_MPA <- Diplodus %>% filter(enforcement == 3)

dip_mod_MPA <- MRFcov(data = Dip_MPA, n_nodes = 5, family = "gaussian")
dip_pred_MPA <- predict_MRF(data = Dip_MPA, MRF_mod = dip_boot) %>% invlogit()
head(dip_pred_MPA) # probabilities

dip_graph_MPA <- graph.adjacency(dip_mod_MPA$graph, weighted = T, mode = "undirected")
deg <- degree(dip_graph_MPA, mode = "all")
plot.igraph(dip_graph_MPA, layout = layout.circle(dip_graph_MPA),
            edge.width = abs(E(dip_graph_MPA)$weight),
            edge.color = ifelse(E(dip_graph_MPA)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Diplodus network inside MPAs")

# Subset MPA data: enforcement < 3
Dip_noMPA <- Diplodus %>% filter(enforcement != 3)

dip_mod_noMPA <- MRFcov(data = Dip_noMPA, n_nodes = 5, family = "gaussian")
dip_pred_noMPA <- predict_MRF(data = Dip_noMPA, MRF_mod = dip_boot) %>% invlogit()
head(dip_pred_noMPA) # probabilities

dip_graph_noMPA <- graph.adjacency(dip_mod_noMPA$graph, weighted = T, mode = "undirected")
deg <- degree(dip_graph_noMPA, mode = "all")
plot.igraph(dip_graph_noMPA, layout = layout.circle(dip_graph_noMPA),
            edge.width = abs(E(dip_graph_noMPA)$weight),
            edge.color = ifelse(E(dip_graph_noMPA)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Diplodus network outside MPAs")

########################################################################################################################################
########################################################################################################################################

### Network 3a: Herbivores + temperature
herb <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
          "Scarus.ghobban", "Sparisoma.cretense")

# Create a species matrix of 'herb'
herb <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(herb[1:5], tmean_reg)

herb_mod <- MRFcov(data = herb, n_nodes = 5, family = "gaussian")
herb_boot <- bootstrap_MRF(data = herb, n_nodes = 5, n_covariates = 1, family = "gaussian", n_cores = 3)
herb_pred <- predict_MRF(data = herb, MRF_mod = herb_boot)
head(herb_pred)

## Graphs

# min(herb$tmean_reg) # -1.62
# median(herb$tmean_reg) # 0.18
# max(herb$tmean_reg) # 1.49

# Subset temperature data: low
herb_temp_low <- herb %>% filter(between(tmean_reg, -1.62, 0))

herb_mod_low <- MRFcov(data = herb_temp_low, n_nodes = 5, family = "gaussian")
herb_pred_low <- predict_MRF(data = herb_temp_low, MRF_mod = herb_boot) %>% invlogit()
head(herb_pred_low) # probabilities

herb_graph_low <- graph.adjacency(herb_mod_low$graph, weighted = T, mode = "undirected")
deg <- degree(herb_graph_low, mode = "all")
plot.igraph(herb_graph_low, layout = layout.circle(herb_graph_low),
            edge.width = abs(E(herb_graph_low)$weight),
            edge.color = ifelse(E(herb_graph_low)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Herbivore network in low temperatures")

# Subset temperature data: med
herb_temp_med <- herb %>% filter(between(tmean_reg, 0.00001, 1))

herb_mod_med <- MRFcov(data = herb_temp_med, n_nodes = 5, family = "gaussian")
herb_pred_med <- predict_MRF(data = herb_temp_med, MRF_mod = herb_boot) %>% invlogit()
head(herb_pred_med) # probabilities

herb_graph_med <- graph.adjacency(herb_mod_med$graph, weighted = T, mode = "undirected")
deg <- degree(herb_graph_med, mode = "all")
plot.igraph(herb_graph_med, layout = layout.circle(herb_graph_med),
            edge.width = abs(E(herb_graph_med)$weight),
            edge.color = ifelse(E(herb_graph_med)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Herbivore network in median temperatures")

# Subset temperature data: high
herb_temp_hi <- herb %>% filter(between(tmean_reg, 1.00001, 1.49))

herb_mod_hi <- MRFcov(data = herb_temp_hi, n_nodes = 5, family = "gaussian")
herb_pred_hi <- predict_MRF(data = herb_temp_hi, MRF_mod = herb_boot) %>% invlogit()
head(herb_pred_hi) # probabilities

herb_graph_hi <- graph.adjacency(herb_mod_hi$graph, weighted = T, mode = "undirected")
deg <- degree(herb_graph_hi, mode = "all")
plot.igraph(herb_graph_hi, layout = layout.circle(herb_graph_hi),
            edge.width = abs(E(herb_graph_hi)$weight),
            edge.color = ifelse(E(herb_graph_hi)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Herbivore network in high temperatures")




### Network 2b: herb + MPAs

herb <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
          "Scarus.ghobban", "Sparisoma.cretense")

# Create a species matrix of 'herb'
herb <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(herb[1:5], enforcement)
str(herb)

## Graphs

# Subset MPA data: enforcement == 3
herb_MPA <- herb %>% filter(enforcement == 3)

herb_mod_MPA <- MRFcov(data = herb_MPA, n_nodes = 5, family = "gaussian")
herb_pred_MPA <- predict_MRF(data = herb_MPA, MRF_mod = herb_boot) %>% invlogit()
head(herb_pred_MPA) # probabilities

herb_graph_MPA <- graph.adjacency(herb_mod_MPA$graph, weighted = T, mode = "undirected")
deg <- degree(herb_graph_MPA, mode = "all")
plot.igraph(herb_graph_MPA, layout = layout.circle(herb_graph_MPA),
            edge.width = abs(E(herb_graph_MPA)$weight),
            edge.color = ifelse(E(herb_graph_MPA)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Herbivore network inside MPAs")

# Subset MPA data: enforcement < 3
herb_noMPA <- herb %>% filter(enforcement != 3)

herb_mod_noMPA <- MRFcov(data = herb_noMPA, n_nodes = 5, family = "gaussian")
herb_pred_noMPA <- predict_MRF(data = herb_noMPA, MRF_mod = herb_boot) %>% invlogit()
head(herb_pred_noMPA) # probabilities

herb_graph_noMPA <- graph.adjacency(herb_mod_noMPA$graph, weighted = T, mode = "undirected")
deg <- degree(herb_graph_noMPA, mode = "all")
plot.igraph(herb_graph_noMPA, layout = layout.circle(herb_graph_noMPA),
            edge.width = abs(E(herb_graph_noMPA)$weight),
            edge.color = ifelse(E(herb_graph_noMPA)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.85),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Herbivore network outside MPAs")


