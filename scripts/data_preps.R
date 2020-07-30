
# Graphical agents
neg_col <- '#3399CC'
pos_col <- '#FF3333'
col_formatter <- formatter("span",
                           style = x ~ style(color =
                                               ifelse(x > 0, pos_col, ifelse(x < 0, neg_col, "black"))))


# Taxa vectors ------------------------------------------------------------

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Scarus.ghobban", "Sparisoma.cretense")

# Vector of all species that were observed over 10 times in the whole dataset
all_spps_over10 <- med_clean %>% 
  group_by_at(.vars = c("lat", "lon", "site", "trans", "species")) %>% # group for summarise
  summarise(n = sum(sp.n)) %>% # sum sp.n for each grouped variable
  spread(species, n, fill = 0) %>%  # convert to species matrix
  ungroup() %>% 
  select(5:127) %>% 
  colSums() %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(V1 > 10) %>%
  select(rowname) %>% as.list() %>% as.array()

# Env/Anthro vectors ------------------------------------------------------

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

## SEABREAM (Diplodus species)
dip_mat <- create_spp_mat(dataset = med_clean, taxa = diplodus, covariate = c("country", "mpa", "temp", "depth", "sal", "prod"))

## HERBIVORES
herb_mat <- create_spp_mat(dataset = med_clean, taxa = herbivores, covariate = c("country", "mpa", "temp", "depth", "sal", "prod"))