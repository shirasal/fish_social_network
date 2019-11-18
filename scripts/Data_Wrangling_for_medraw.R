# 20 AUG 2019
library(tidyverse)
# Upload file as tibble and check it:
list.files()

med_raw <- read_csv("data/med_raw.csv", na = "NA")
spec(med_raw) # lists all the columns and their types
# Change 'protection' to logical
med_raw <- med_raw %>% 
  mutate(protection = if_else(protection == "YES", TRUE, FALSE))

##### ========= CHECK DATA ========= #####

# Which sites have counts of 1 of A. boyeri?
med_raw %>% 
  select(site, species, sp.n) %>% 
  filter(species == "Atherina.boyeri") %>% 
  filter(!(sp.n > 1)) %>% distinct(site)
# TODO Which other species shpuld be seen with more that one individual?

# Where are some unnessesary zeros?
med_raw %>% 
  select(site, species, sp.n) %>% 
  filter(sp.n == 0) %>% 
  group_by(site, species) %>% 
  summarise(.) %>% distinct(species)

species_list <- med_raw %>% select(species) %>% distinct(.)

##### ========= CREATE SPECIES MATRIX ========= #####
# Create a tibble of metadata
med_meta <- med_raw %>% 
  distinct(country, site, lon, lat, season, protection, enforcement, total.mpa.ha, size.notake, 
           yr.creation, age.reserve.yr, tmean, trange, sal_mean, pp_mean, pp_range)

# Create a species matrix with summation of each species in each site (abundance)
med_mat <- med_raw %>% 
  group_by(site, lon, lat, species) %>% # Note it only shows sites and species (with coordinate-locations)
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
head(med_mat)

# Put the 2 data sets together (species and metadata)
full_med_mat <- left_join(med_meta, med_mat, by = c("site", "lon", "lat"))
# write_csv(full_med_mat, "data/med_species_matrix.csv")
# read_csv("data/med_species_matrix.csv")
View(full_med_mat)

# Convert the abundance matrix to presence-absence matrix
pres_abs_mat <- full_med_mat
pres_abs_mat[17:ncol(pres_abs_mat)] <- ifelse(pres_abs_mat[17:ncol(pres_abs_mat)] > 0, 1, 0)
View(pres_abs_mat)
write_csv(pres_abs_mat, "presence_absence_med_mat.csv")

##### ========= CREATE SPECIES LIST ========= #####
spp_list <- med_raw %>% 
  select(species) %>% 
  distinct()
spp_list$sci_name <- gsub(pattern = "\\.",replacement = " ", x = spp_list$species)
write_csv(spp_list, path = "data/species_list.csv", col_names = TRUE)
list.files("data")
