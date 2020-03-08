
# Load packages -----------------------------------------------------------

library(parallel)
library(igraph)
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