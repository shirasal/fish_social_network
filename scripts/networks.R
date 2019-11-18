library(igraph)
library(tidyverse)
library(MRFcov)

med_raw <- read_csv("data/med_raw.csv", col_types = cols(depth = col_double()))
str(med_raw)

groupers <- c("Epinephelus.aeneus", "Epinephelus.caninus", "Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.hepatus", "Serranus.scriba")

Serranidae <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # azz_asi is only presence-absence
  group_by(site, lon, lat, trans, species) %>%
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0) %>% 
  select(site, trans, lon, lat, Epinephelus.aeneus, Epinephelus.caninus, Epinephelus.costae,
         Epinephelus.marginatus, Mycteroperca.rubra, Serranus.cabrilla, Serranus.hepatus, Serranus.scriba)
head(Serranidae)

serr_mrf <- MRFcov(data = Serranidae, n_nodes = 8, family = "gaussian")
# no Inf values, I don't know what it wants :(
# TODO create a matrix with covariates and run properly
