library(tidyverse)
load("data/data_and_objects.RData")

# How many species of each site?
med_raw %>% distinct(species) %>% arrange(species) %>% print(n = Inf)

dip_test <- c("Diplodus.annularis", "Diplodus.cervinus", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris")
grps_test <- c("Epinephelus.aeneus", "Epinephelus.caninus", "Epinephelus.costae", "Epinephelus.marginatus", "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.hepatus", "Serranus.scriba")
herb_test <- c("Sparisoma.cretense", "Siganus.luridus", "Siganus.rivulatus", "Sarpa.salpa", "Scarus.ghobban")

# Check in how many transects species appear:
med_raw %>% 
  mutate(guild = case_when(species %in% dip_test ~ "diplodus",
                           species %in% grps_test ~ "groupers",
                           species %in% herb_test ~ "herbivores")) %>% 
  group_by(guild, species) %>% count(site, trans) %>% 
  group_by(guild, species) %>% summarise(count_trans = n()) %>% 
  filter(!is.na(guild)) %>%
  arrange(guild, count_trans) %>% print(n = Inf)

med_raw %>% 
  mutate(guild = case_when(species %in% dip_test ~ "diplodus",
                           species %in% grps_test ~ "groupers",
                           species %in% herb_test ~ "herbivores")) %>% 
  group_by(guild, species) %>% count(site) %>% 
  group_by(guild, species) %>% summarise(count_site = n()) %>% 
  filter(!is.na(guild)) %>%
  arrange(guild, count_site) %>% print(n = Inf)

# NAs #
depth_NA <- med_clean %>%
  group_by_at(.vars = c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)) %>%
  summarise(n = sum(abundance)) %>% 
  spread(species, n, fill = 0) %>% 
  ungroup() %>% 
  filter(is.na(depth)) %>% 
  select(site, trans, all_of(c(groupers, diplodus, herbivores)))

write_csv(depth_NA, "issues/depth_NAs.csv")

MPA_NA <- med_clean %>%
  group_by_at(.vars = c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)) %>%
  summarise(n = sum(abundance)) %>% 
  spread(species, n, fill = 0) %>% 
  ungroup() %>% 
  filter(is.na(mpa)) %>% 
  select(site, trans, all_of(c(groupers, diplodus, herbivores)))

write_csv(MPA_NA, "issues/MPA_NAs.csv")
