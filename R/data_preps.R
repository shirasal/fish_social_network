# source("R/packages.R")

create_spp_mat <- function(dataset, taxa, covariate){
  cols <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)
  dataset %>% 
    group_by_at(.vars = cols) %>% # group for summarise
    summarise(n = sum(sp.n)) %>% # sum sp.n for each grouped variable
    spread(species, n, fill = 0) %>% # convert to species matrix
    ungroup() %>% 
    na.omit() %>% # remove NAs; make sure this part it minimised in the raw data
    mutate(loc = paste(site, trans)) %>% # Create a variable of the location, which shpuld be unique
    group_by(loc) %>% 
    column_to_rownames("loc") %>% # create row names by location
    select(all_of(taxa), all_of(covariate)) # keep the species and covariates columns
}

# Graphical agents
my_cols <- c(neg = '#3399CC', pos = '#FF3333')

col_formatter <- formattable::formatter("span",
                           style = x ~ style(color =
                                               ifelse(x > 0, my_cols[["pos"]], ifelse(x < 0, my_cols[["neg"]], "black"))))


# Taxa vectors ------------------------------------------------------------

groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris", "Diplodus.cervinus")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Scarus.ghobban", "Sparisoma.cretense")


# Env/Anthro vectors ------------------------------------------------------

env_vector <- c("temp", "depth", "prod") # Salinity (sal) removed to avoid data loss (NAs)
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

locs <- med_raw %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, tmean) %>% unique()

# TODO add covariates: invasive species count/biomass (spatial), MPA age, MPA size; salinity after completing NAs

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

rm(create_spp_mat)
save.image(file = "data/all_objects.RData")
