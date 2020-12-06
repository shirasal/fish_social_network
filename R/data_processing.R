source("R/packages.R")
source("R/functions.R")

### Variables (fitting with med_clean)
env_vector <- c("temp", "depth", "prod")
anthro_vector <- c("mpa")

### Guilds
groupers <- c("Epinephelus.costae", "Epinephelus.marginatus", "Serranus.cabrilla", "Serranus.scriba")
diplodus <- c("Diplodus.annularis", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris")
herbivores <- c("Siganus.rivulatus", "Siganus.luridus", "Sarpa.salpa", "Sparisoma.cretense")

guild_colours <- list(grps = "#C54607", dip = "#145D82", herb = "#43AA8B")
all_guilds <- list(groupers = groupers, seabreams = diplodus, herbivores = herbivores)

### MEData
med_raw <- read_rds("data/medata.Rds") %>%
  ungroup() %>% 
  filter(data.origin != "azz_asi") # presence-absence
# 41,285 rows; more here:
# file.edit("issues/exploring_issues.R")
# or here:
# browseURL(url = "https://shirasal.github.io/medata/index.html")

# MEData relevant to my thesis: scaled variables, MPA variable, tidy.
# Used for creating species matrices
med_clean <- med_raw %>%
  mutate(mpa = case_when(enforcement == 1 | enforcement == 0 ~ FALSE,
                         enforcement == 2 | enforcement == 3 ~ TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         prod = scale(pp_mean)) %>%
  group_by(lon, lat, site, trans, species, mpa, temp, depth, prod) %>% 
  summarise(abundance = sum(sp.n), .groups = "drop") %>%
  select(site, lon, lat, trans, species, abundance, mpa, temp, depth, prod)
# Notice NAs in Depth and MPA; more here:
# file.edit("issues/exploring_issues.R")

# Same as `med_clean` but with guild information, and unscaled temperature
# Used for plotting raw data
guilds_data <- med_raw %>%
  filter(species %in% c(groupers, diplodus, herbivores)) %>% 
  mutate(mpa = case_when(enforcement == 1 | enforcement == 0 ~ FALSE,
                         enforcement == 2 | enforcement == 3 ~ TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         prod = scale(pp_mean),
         guild = case_when(species %in% groupers ~ "groupers",
                           species %in% diplodus ~ "seabreams",
                           species %in% herbivores ~ "herbivores")) %>%
  group_by(lon, lat, site, trans, species, guild, mpa, temp, temperature = tmean, depth, prod) %>% 
  summarise(abundance = sum(sp.n), .groups = "drop") %>%
  select(site, lon, lat, trans, species, abundance, guild, mpa, temp, temperature, depth, prod)

### Species matrices
# Relying on function 'create_spp_mat' found in 'R/final/functions.R'

grps_mat <- create_spp_mat(dataset = med_clean, guild = groupers, covariate = c("mpa", "temp", "depth", "prod"))
dip_mat <- create_spp_mat(dataset = med_clean, guild = diplodus, covariate = c("mpa", "temp", "depth", "prod"))
herb_mat <- create_spp_mat(dataset = med_clean, guild = herbivores, covariate = c("mpa", "temp", "depth", "prod"))

rm(coefs_sum, create_spp_mat, plot_relimp, rel_imp_sum, vis_mpa_pred_pair, vis_raw_mpa, vis_raw_temp, vis_temp_pred_pair) # Removed to avoid appending
save.image(file = "data/data_and_objects.RData")
