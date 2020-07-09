
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

# Create variables --------------------------------------------------------

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Scarus.ghobban", "Sparisoma.cretense")

# Add medata --------------------------------------------------------------

med_raw <- read_rds("data/medata.Rds")
med_clean <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         country = as.factor(country),
         temp = scale(tmean),
         depth = scale(depth),
         # enforce = as.factor(enforcement),
         sal = scale(sal_mean),
         prod = scale(pp_mean)) %>%
  select(site, lon, lat, trans, species, sp.n, country, mpa, temp, depth, sal, prod)
  

med_clean %>% colnames

# Functions ---------------------------------------------------------------

# Func 1: Create species matrix for a specific taxa with all environmental variables
create_spp_mat <- function(dataset, taxa, covariate){
  dataset %>%
    group_by(lat, lon, site, trans, species, temp, mpa, depth) %>% # TODO add:, country, sal, prod -- these have been removed atm. They have many NAs and I'd like to 
    summarise(n = sum(sp.n)) %>% 
    spread(species, n, fill = 0) %>% 
    sample_n(size = 1) %>% 
    ungroup() %>% 
    na.omit() %>% 
    mutate(loc = paste(site, trans)) %>% 
    column_to_rownames("loc") %>%
    select(all_of(taxa), all_of(covariate)) #TODO add:, country, sal, prod --- see above comment
}

#*** *** ***#



#*** *** ***#


# Running Script ----------------------------------------------------------

# Create species matrix to run the model on (using FUNC 1)
# This matrix should include all species from the taxa I'm interested in
# and the covariates I'd like to include in the model (these are pre-determined in FUNC 1)
groupers_mat <- create_spp_mat(dataset = med_clean, taxa = groupers, covariate = c("temp", "depth", "mpa"))
groupers_mat %>% View()

# Run MRF model with covariates
groupers_mod <- MRFcov(data = groupers_mat, n_nodes = length(groupers), n_covariates = 3, family = "gaussian")
groupersHM_cov <- plotMRF_hm(groupers_mod, main = "with covariates")

groupers_mod$indirect_coefs$temp
groupers_mod$indirect_coefs$depth
groupers_mod$indirect_coefs$mpa

groupers_mod_nocov <- MRFcov(data = groupers_mat, n_nodes = length(groupers), n_covariates = 3, family = "gaussian")
groupersHM_nocov <- plotMRF_hm(groupers_mod_nocov, main = "without covariates")

# Compare relationships between species with and without covariates:
gridExtra::grid.arrange(groupersHM_cov, groupersHM_nocov, nrow = 1, top = "Groupers co-occurrence")

# Create species matrix to run the model on, this time for diplodus
# This matrix should include all species from the taxa I'm interested in
# and the covariates I'd like to include in the model (these are pre-determined in FUNC 1)
seabream_mat <- create_spp_mat(dataset = med_clean, taxa = diplodus, covariate = c("temp", "depth", "mpa"))
seabream_mat %>% View()

# Run MRF model with covariates
seabream_mod <- MRFcov(data = seabream_mat, n_nodes = length(diplodus), n_covariates = 3, family = "gaussian")
seabreamHM_cov <- plotMRF_hm(seabream_mod, main = "with covariates")

seabream_mod$indirect_coefs$temp
seabream_mod$indirect_coefs$depth
seabream_mod$indirect_coefs$mpa

seabream_mod_nocov <- MRFcov(data = seabream_mat, n_nodes = length(diplodus), n_covariates = 3, family = "gaussian")
seabreamHM_nocov <- plotMRF_hm(seabream_mod_nocov, main = "without covariates")

# Compare relationships between species with and without covariates:
gridExtra::grid.arrange(seabreamHM_cov, seabreamHM_nocov, nrow = 1, top = "Seabream co-occurrence")


