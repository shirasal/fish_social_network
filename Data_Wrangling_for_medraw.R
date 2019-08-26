# 20 AUG 2019
library(tidyverse)
# Upload file as tibble and check it:
list.files()

raw_med <- read_csv("med_raw.csv", na = "NA")
spec(raw_med) # lists all the columns and their types

check_single_a_boyeri <- raw_med %>% 
  select(site, species, sp.n) %>% 
  filter(species == "Atherina.boyeri") %>% 
  filter(!(sp.n > 1))
# ASINARA_add looks like presence-absence data (because of A. boyeri being only 1 in all observations in this site)
# Also Linosa has some lone Atherines, Kornati has one too and Haboanim has 0 Atherines?

write_csv(check_single_a_boyeri, "a_boyeri_sing.csv")
read_csv("a_boyeri_sing.csv")
# Which other species shpuld be seen with more that one individual?
# TODO Make a list and repeat above code for each one of the species

check_zeros <- raw_med %>% 
  select(site, species, sp.n) %>% 
  filter(sp.n == 0) %>% 
  group_by(site, species) %>% 
  summarise(.)
write_csv(check_zeros, "zeros.csv")
read_csv("zeros.csv")
# TODO find out why there are 0s

# TODO Change "protection" to logical

