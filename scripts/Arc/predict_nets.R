
# load MEData
med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double())) %>%
  mutate(tmean_reg = scale(tmean)) # regularise temperature covariate
str(med_raw)

west <- med_raw %>% filter(country %in% c("France", "Italy", "Spain"))
east <- med_raw %>% filter(country %in% c("Croatia", "Greece", "Israel", "Malta", "Turkey"))

### Network 1a: Groupers and combers + temperatures
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")

# Create a species matrix of 'groupers' for the WEST
Serranidae_W <- west %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers[1:5], tmean_reg, enforcement)
str(Serranidae_W)

Serranidae_W_t <- Serranidae_W %>% select(-enforcement)
serr_mod <- MRFcov(data = Serranidae_W_t, n_nodes = 5, n_covariates = 1, family = "gaussian")
serr_boot <- bootstrap_MRF(data = Serranidae_W_t, n_nodes = 5, n_covariates = 1, family = "gaussian", n_cores = 3)
serr_pred <- predict_MRF(data = Serranidae_W_t, MRF_mod = serr_boot)
head(serr_pred)

## Graphs

# min(Serranidae_W_t$tmean_reg) # -1.62
# median(Serranidae_W_t$tmean_reg) # -0.23
# max(Serranidae_W_t$tmean_reg) # 0.25

# Subset temperature data: low
Serr_temp_low <- Serranidae_W_t %>% filter(between(tmean_reg, -1.62, -0.2))

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
            main = "Serranidae network in low temperatures (west)")

# Subset temperature data: med
Serr_temp_med <- Serranidae_W_t %>% filter(between(tmean_reg, -0.2, 0))

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
            main = "Serranidae network in median temperatures (west)")

# Subset temperature data: high
Serr_temp_hi <- Serranidae_W_t %>% filter(between(tmean_reg, 0, 0.25))

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
            main = "Serranidae network in high temperatures (west)")




### Network 1b: Groupers and combers + MPAs

Serranidae_W_m <- Serranidae_W %>% select(-tmean_reg)

# Subset MPA data: enforcement == 3
Serr_MPA <- Serranidae_W %>% filter(enforcement == 3)

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
            main = "Serranidae network inside MPAs (west)")

# Subset MPA data: enforcement == 2
Serr_MPA <- Serranidae_W %>% filter(enforcement == 2)

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
            main = "Serranidae network ; enforcement = 2")

# Subset MPA data: enforcement == 1
Serr_MPA <- Serranidae_W %>% filter(enforcement == 1)

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
            main = "Serranidae network ; enforcement = 1")

# Subset MPA data: enforcement == 0
Serr_noMPA <- Serranidae_W %>% filter(enforcement == 0)

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
            main = "Serranidae network ; enforcement = 0")





# Create a species matrix of 'groupers' for the EAST
Serranidae_E <- east %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(groupers[1:5], tmean_reg, enforcement)
str(Serranidae_E)

Serranidae_E_t <- Serranidae_E %>% select(-enforcement)
serr_mod <- MRFcov(data = Serranidae_E_t, n_nodes = 5, n_covariates = 1, family = "gaussian")
serr_boot <- bootstrap_MRF(data = Serranidae_E_t, n_nodes = 5, n_covariates = 1, family = "gaussian", n_cores = 3)
serr_pred <- predict_MRF(data = Serranidae_E_t, MRF_mod = serr_boot)
head(serr_pred)

## Graphs

# min(Serranidae_E$tmean_reg) # -1.49
# median(Serranidae_E$tmean_reg) # 1.4
# max(Serranidae_E$tmean_reg) # 1.49

# Subset temperature data: low
Serr_temp_low <- Serranidae_E_t %>% filter(between(tmean_reg, -1.48, -0.5))

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
            main = "Serranidae network in low temperatures (east)")

# Subset temperature data: med
Serr_temp_med <- Serranidae_E_t %>% filter(between(tmean_reg, -0.5, 1.4))

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
            main = "Serranidae network in median temperatures (east)")

# Subset temperature data: high
Serr_temp_hi <- Serranidae_E_t %>% filter(between(tmean_reg, 1.4, 1.49))

serr_mod_hi <- MRFcov(data = Serr_temp_hi, n_nodes = 5, family = "gaussian")
serr_pred_hi <- predict_MRF(data = Serr_temp_hi, MRF_mod = serr_boot) %>% invlogit()
head(serr_pred_hi) # probabilities

############################################# <<<<<<<<< NOT WORKING >>>>>>>>>> ##################################
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
            main = "Serranidae network in high temperatures (east)")
########################################### <<<<<<<<<<<<<<<< >>>>>>>>>>>>>>> ###################################

### Network 1b: Groupers and combers + MPAs

Serranidae_E_m <- Serranidae_E %>% select(-tmean_reg)

# Subset MPA data: enforcement == 3
Serr_MPA <- Serranidae_E_m %>% filter(enforcement == 3)

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
            main = "Serranidae network inside MPAs (west)")

# Subset MPA data: enforcement == 2
Serr_MPA <- Serranidae_E_m %>% filter(enforcement == 2)

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
            main = "Serranidae network ; enforcement = 2")

# Subset MPA data: enforcement == 1
Serr_MPA <- Serranidae_E_m %>% filter(enforcement == 1)

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
            main = "Serranidae network ; enforcement = 1")

# Subset MPA data: enforcement == 3
Serr_noMPA <- Serranidae_E_m %>% filter(enforcement == 0)

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
            main = "Serranidae network ; enforcement = 0")

################################################################################################################################
################################################################################################################################

### Network 2a: Diplodus + temperatures
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")

# Create a species matrix of 'diplodus'
Diplodus_e <- east %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(diplodus[1:5], tmean_reg, enforcement)

Diplodus <- Diplodus_e %>% select(-enforcement)

dip_mod <- MRFcov(data = Diplodus, n_nodes = 5, family = "gaussian")
dip_boot <- bootstrap_MRF(data = Diplodus, n_nodes = 5, n_covariates = 1, family = "gaussian", n_cores = 3)
dip_pred <- predict_MRF(data = Diplodus, MRF_mod = dip_boot)
head(dip_pred)

## Graphs

# min(Diplodus$tmean_reg) # -1.49
# median(Diplodus$tmean_reg) # 1.4
# max(Diplodus$tmean_reg) # 1.49

# Subset temperature data: low
Dip_temp_low <- Diplodus %>% filter(between(tmean_reg, -1.5, -0.23))

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
            main = "Diplodus network in low temperatures (east)")


# Subset temperature data: med
Dip_temp_med <- Diplodus %>% filter(between(tmean_reg, -0.23, 0))

dip_mod_med <- MRFcov(data = Dip_temp_med, n_nodes = 5, family = "gaussian")
dip_pred_med <- predict_MRF(data = Dip_temp_med, MRF_mod = dip_boot) %>% invlogit()
head(dip_pred_med) # probabilities

################################## <<<<<<<<<<<<<<<< NOT WORKING  >>>>>>>>>>>>>>>>> ##################################
dip_graph_med <- graph.adjacency(dip_mod_med$graph, weighted = T, mode = "undirected")
deg <- degree(dip_graph_med, mode = "all")
plot.igraph(dip_graph_med, layout = layout.circle(dip_graph_med),
            edge.width = abs(E(dip_graph_med)$weight),
            edge.color = ifelse(E(dip_graph_med)$weight < 0, '#3399CC', '#FF3333'),
            vertex.size = deg,
            vertex.label.family = "sans",
            vertex.label.font	= 3,
            vertex.label.cex = 1,
            vertex.label.color = adjustcolor("#333333", 0.25),
            vertex.color = adjustcolor("#FFFFFF", .5),
            main = "Diplodus network in median temperatures (east)")

#################################### <<<<<<<<<<<<<<<<< >>>>>>>>>>>>>>>>>> #####################################
# Subset temperature data: high
Dip_temp_hi <- Diplodus %>% filter(between(tmean_reg, 0, 0.25))

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
            main = "Diplodus network in high temperatures (east)")




### Network 2b: Diplodus + MPAs

Diplodus <- Diplodus_e %>% select(-tmean_reg)


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
            main = "Diplodus network ; enforcement = 3 (west)")


########################################################################################################################################
########################################################################################################################################


### Network 3a: Herbivores + temperature
herb <- c("Siganus.luridus", "Sarpa.salpa", "Sparisoma.cretense") # west

herb <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa", "Scarus.ghobban", "Sparisoma.cretense") # east

# Create a species matrix of 'herb'
herb_e <- east %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  mutate(loc = paste0(site, "_", trans)) %>% 
  group_by(loc, species, tmean_reg, enforcement) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  as.data.frame() %>% 
  `rownames<-`(make.unique(.$loc)) %>%
  select(herb[1:3], tmean_reg, enforcement)

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


