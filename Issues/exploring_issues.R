library(tiduyerse)

# How many species of each site?
med_raw %>% distinct(species) %>% arrange(species) %>% print(n = Inf)

dip_test <- c("Diplodus.annularis", "Diplodus.cervinus", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris")
grps_test <- c("Epinephelus.aeneus", "Epinephelus.caninus", "Epinephelus.costae", "Epinephelus.marginatus", "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.hepatus", "Serranus.scriba")
herb_test <- c("Sparisoma.cretense", "Siganus.luridus", "Siganus.rivulatus", "Sarpa.salpa", "Scarus.ghobban")

med_raw %>% group_by(species) %>% count(site) %>% 
  group_by(species) %>% summarise(blah = n()) %>% 
  filter(species %in% c(dip_test, grps_test, herb_test)) %>%
  arrange(species) %>% print(n = Inf)

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
