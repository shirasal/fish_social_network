source("R/packages.R")

# Data --------------------------------------------------------------------

### Variables (fitting with med_clean)
env_vector <- c("temp", "depth", "prod")
anthro_vector <- c("mpa")

### Guilds
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus",
              "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus",
              "Diplodus.vulgaris")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa",
                "Sparisoma.cretense")

### MEData
med_raw <- read_rds("data/medata.Rds") %>% ungroup()

med_clean <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         sal = scale(sal_mean),
         prod = scale(pp_mean)) %>%
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)

mean_depth <- median(scale(med_raw$depth), na.rm = TRUE)
mean_prod <- median(scale(med_raw$pp_mean))
mean_temp <- median(scale(med_raw$tmean))

# Functions ---------------------------------------------------------------

### Create species matrix - for guild with environmental variables
create_spp_mat <- function(dataset, guild, covariate){
  cols <- c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)
  dataset %>%
    group_by_at(.vars = cols) %>%
    summarise(n = sum(sp.n)) %>% 
    spread(species, n, fill = 0) %>% 
    ungroup() %>%
    na.omit() %>% 
    mutate(loc = paste(site, trans)) %>% 
    group_by(loc) %>%
    column_to_rownames("loc") %>% 
    select(all_of(guild), all_of(covariate)) 
}


# Data preps --------------------------------------------------------------

### Create species matrices
grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, covariate = c("mpa", "temp", "depth", "prod"))
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, covariate = c("mpa", "temp", "depth", "prod"))
herb_mat <- create_spp_mat(dataset = med_clean, guild = herbivores, covariate = c("mpa", "temp", "depth", "prod"))

# save.image(file = "data/base_data_and_matrices.RData")
