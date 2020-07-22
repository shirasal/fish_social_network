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
# TODO fix me
med_raw %<>% filter(!(site %in% c("assecret2210191mlsc_a", "assecret2210191mlsc_b", "assecret2210191mlsc_c")))

med_clean <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         country = as.numeric(as.factor(country)),
         temp = scale(tmean),
         depth = scale(depth),
         # enforce = as.factor(enforcement),
         sal = scale(sal_mean),
         prod = scale(pp_mean)) %>%
  select(site, lon, lat, trans, species, sp.n, country, mpa, temp, depth, sal, prod)

