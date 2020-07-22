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

env_vector <- c("country", "temp", "depth", "sal", "prod") # TODO complete 'sal' NAs
anthro_vector <- c("mpa")

# Add medata --------------------------------------------------------------

med_raw <- read_rds("data/medata.Rds")

med_clean <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         country = as.numeric(as.factor(country)),
         temp = scale(tmean),
         depth = scale(depth),
         sal = scale(sal_mean),
         prod = scale(pp_mean)) %>%
  select(site, lon, lat, trans, species, sp.n, country, mpa, temp, depth, sal, prod)

# TODO add covariates: invasive species count/biomass (spatial), MPA age, MPA size

# Create species matrix for each taxa -------------------------------------

# Create species matrix to run the model on (using FUNC 1)
# This matrix should include all species from the taxa I'm interested in
# and the covariates I'd like to include in the model (these are pre-determined in FUNC 1)

## GROUPERS
grps_mat <- create_spp_mat(dataset = med_clean, taxa = groupers, covariate = c("country", "mpa", "temp", "depth", "sal", "prod"))
# grps_mat %>% View()

## SEABREAM (Diplodus species)
dip_mat <- create_spp_mat(dataset = med_clean, taxa = diplodus, covariate = c("country", "mpa", "temp", "depth", "sal", "prod"))
# dip_mat %>% View()

## HERBIVORES
herb_mat <- create_spp_mat(dataset = med_clean, taxa = herbivores, covariate = c("country", "mpa", "temp", "depth", "sal", "prod"))
# herb_mat %>% View()
