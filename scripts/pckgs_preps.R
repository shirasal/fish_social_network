# Packages ----------------------------------------------------------------

library(parallel)
library(formattable)
library(igraph)
# library(tidygraph)
# library(ggraph)
library(magrittr)
library(tidyverse)
library(MRFcov)
# library(LaplacesDemon)
library(usethis)

# Graphical agents
neg_col <- '#3399CC'
pos_col <- '#FF3333'
col_formatter <- formatter("span",
                           style = x ~ style(color =
                                               ifelse(x > 0, pos_col, ifelse(x < 0, neg_col, "black"))))

# Create TAXA vectors -----------------------------------------------------

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Scarus.ghobban", "Sparisoma.cretense")

# Create ENV/ANTHRO vectors -----------------------------------------------

env_vector <- c("temp", "depth") # TODO add:, country, sal, prod -- these have been removed atm. They have many NAs and I'd like to dix that first
anthro_vector <- c("mpa")