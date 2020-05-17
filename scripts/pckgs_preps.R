
# Load packages -----------------------------------------------------------

library(parallel)
library(formattable)
library(igraph)
library(tidygraph)
library(ggraph)
library(magrittr)
library(tidyverse)
library(MRFcov)
library(LaplacesDemon)

# parallel::detectCores() # in case I'd like to speed up MRFcov by spreading processing over >1 core

# Create variables --------------------------------------------------------

# Basins
west <- list("France", "Italy", "Spain")
east <- list("Croatia", "Greece", "Israel", "Malta", "Turkey")
all_med <- list("France", "Italy", "Spain", "Croatia", "Greece", "Israel", "Malta", "Turkey")

# Groups
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
             "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
               "Scarus.ghobban", "Sparisoma.cretense")

# Add medata --------------------------------------------------------------

med_raw <- read_rds("data/medata.Rds") %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(tmean_reg = scale(tmean),
         depth_reg = scale(depth),
         # log_n = log10(sp.n),
         mpa = if_else(enforcement <= 1, FALSE, TRUE))


# Create coordinate dfs ---------------------------------------------------

coords_all <- med_raw %>%
  filter(country %in% all_med) %>% 
  group_by(lat, lon, site, trans, species, tmean_reg, mpa, depth_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  sample_n(size = 1) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc")

coords_w <- med_raw %>%
  filter(country %in% west) %>% 
  group_by(lat, lon, site, trans, species, tmean_reg, mpa, depth_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  sample_n(size = 1) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc")

coords_e <- med_raw %>%
  filter(country %in% east) %>% 
  group_by(lat, lon, site, trans, species, tmean_reg, mpa, depth_reg) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  sample_n(size = 1) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(loc = paste(site, trans)) %>% 
  column_to_rownames("loc")
