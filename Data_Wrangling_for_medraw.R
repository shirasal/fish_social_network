# 20 AUG 2019
library(tidyverse)
# Upload file as tibble and check it:
list.files()

raw_med <- read_csv("med_raw.csv", na = "NA")
spec(raw_med) # lists all the columns and their types
raw_med <- raw_med %>% 
  mutate(protection = if_else(protection == "YES", TRUE, FALSE))

check_single_a_boyeri <- raw_med %>% 
  select(site, species, sp.n) %>% 
  filter(species == "Atherina.boyeri") %>% 
  filter(!(sp.n > 1))
# ASINARA_add looks like presence-absence data (because of A. boyeri being only 1 in all observations in this site)
# Also Linosa has some lone Atherines, Kornati has one too and Haboanim has 0 Atherines?

write_csv(check_single_a_boyeri, "Issues/a_boyeri_sing.csv")
read_csv("Issues/a_boyeri_sing.csv")
# Which other species shpuld be seen with more that one individual?
# TODO Make a list and repeat above code for each one of the species

check_zeros <- raw_med %>% 
  select(site, species, sp.n) %>% 
  filter(sp.n == 0) %>% 
  group_by(site, species) %>% 
  summarise(.)
write_csv(check_zeros, "Issues/zeros.csv")
read_csv("Issues/zeros.csv")
# TODO find out why there are 0s

species_list <- raw_med %>% select(species) %>% distinct(.)

##### ========= CREATE SPECIES MATRIX ========= #####
# Create a tibble of metadata
med_meta <- raw_med %>% 
  distinct(country, site, lon, lat, season, protection, enforcement, total.mpa.ha, size.notake, 
           yr.creation, age.reserve.yr, tmean, trange, sal_mean, pp_mean, pp_range)

# Create a species matrix with summation of each species in each site (abundance)
# TODO is this the correct way to do this?
med_mat <- raw_med %>% 
  group_by(site, lon, lat, species) %>% # Note it only shows sites and species (with coordinate-locations)
  summarise(n = sum(sp.n)) %>% 
  spread(species, n, fill = 0)
head(med_mat)

# Put the 2 data sets together (species and metadata)
full_med_mat <- left_join(med_meta, med_mat, by = c("site", "lon", "lat"))
write_csv(full_med_mat, "med_species_matrix.csv")
read_csv("med_species_matrix.csv")
View(full_med_mat)

# Convert the abundance matrix to presence-absence matrix
pres_abs_mat <- full_med_mat
pres_abs_mat[17:ncol(pres_abs_mat)] <- ifelse(pres_abs_mat[17:ncol(pres_abs_mat)] > 0, 1, 0)
View(pres_abs_mat)
write_csv(pres_abs_mat, "presence_absence_med_mat.csv")
