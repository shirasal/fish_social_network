source("R/packages.R")

create_spp_mat <- function(dataset, taxa, covariate){
  cols <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)
  dataset %>% 
    group_by_at(.vars = cols) %>% # group for summarise
    summarise(n = sum(sp.n)) %>% # sum sp.n for each grouped variable
    spread(species, n, fill = 0) %>% # convert to species matrix
    ungroup() %>% 
    na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
    mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which should be unique
    group_by(loc) %>% 
    column_to_rownames("loc") %>% # create row names by location
    select(all_of(taxa), all_of(covariate)) # keep the species and covariates columns
}

# # Graphical agents
# my_cols <- c(neg = '#3399CC', pos = '#FF3333')


# Taxa vectors ------------------------------------------------------------

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Sparisoma.cretense")
# "Scarus.ghobban" removed from 'herbivores'. Only 19 observations in the whole dataset, 
# in 6 transects in achziv and 2 transects in shikmona

# Env/Anthro vectors ------------------------------------------------------

env_vector <- c("temp", "depth", "prod")
anthro_vector <- c("mpa")

# Add medata --------------------------------------------------------------

med_raw <- read_rds("data/medata.Rds") %>% ungroup()

med_clean <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         sal = scale(sal_mean),
         prod = scale(pp_mean)) %>%
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)

locs <- med_raw %>% mutate(loc = paste0(site, "_", trans)) %>% distinct(lon, lat, loc)


# Create species matrix for each taxa -------------------------------------

# Create species matrix to run the model on (using FUNC 1)
# This matrix should include all species from the taxa I'm interested in
# and the covariates I'd like to include in the model (these are pre-determined in FUNC 1)

## GROUPERS
grps_mat <- create_spp_mat(dataset = med_clean,
                           taxa = groupers,
                           covariate = c("mpa", "temp", "depth", "prod"))

## SEABREAM (Diplodus species)
dip_mat <- create_spp_mat(dataset = med_clean, taxa = diplodus, covariate = c("mpa", "temp", "depth", "prod"))

## HERBIVORES
herb_mat <- create_spp_mat(dataset = med_clean, taxa = herbivores, covariate = c("mpa", "temp", "depth", "prod"))


# Create base matrix for predictions --------------------------------------

# Create a tibble of metadata
med_mat <- med_raw %>% 
  group_by_at(.vars = c("lat", "lon", "site", "trans", "species",
                        "enforcement", "tmean", "pp_mean", "depth")) %>%
  summarise(n = sum(sp.n)) %>%
  spread(species, n, fill = 0) %>%
  ungroup() %>% 
  na.omit() %>%
  mutate(loc = paste(site, trans)) %>%
  group_by(loc) %>% 
  column_to_rownames("loc") %>%
  select(all_of(c(groupers, diplodus, herbivores)), all_of(c("enforcement", "tmean", "pp_mean", "depth")))

# Constants
mean_depth <- median(scale(med_raw$depth), na.rm = TRUE)
mean_prod <- median(scale(med_raw$pp_mean))
mean_temp <- median(scale(med_raw$tmean))

# Create the matrix
spp_mat_for_predictions <- med_mat %>% 
  rownames_to_column("loc") %>% 
  mutate(temperature = tmean,
         temp_scaled = scale(tmean),
         depth = mean_depth,
         productivity = mean_prod,
         mpa = if_else(enforcement > 1, TRUE, FALSE)) %>% 
  select(loc, all_of(c(groupers, diplodus, herbivores)), temperature, temp_scaled, depth, productivity, mpa)


rm(create_spp_mat)
save.image(file = "data/all_objects.RData")
