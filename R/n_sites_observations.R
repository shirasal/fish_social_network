med_raw %>% distinct(species) %>% arrange(species) %>% print(n = Inf)

dip_test <- c("Diplodus.annularis", "Diplodus.cervinus", "Diplodus.puntazzo", "Diplodus.sargus", "Diplodus.vulgaris")
grps_test <- c("Epinephelus.aeneus", "Epinephelus.caninus", "Epinephelus.costae", "Epinephelus.marginatus", "Mycteroperca.rubra", "Serranus.cabrilla", "Serranus.hepatus", "Serranus.scriba")
herb_test <- c("Sparisoma.cretense", "Siganus.luridus", "Siganus.rivulatus", "Sarpa.salpa", "Scarus.ghobban")

med_raw %>% group_by(species) %>% count(site) %>% 
  group_by(species) %>% summarise(blah = n()) %>% 
  filter(species %in% c(dip_test, grps_test, herb_test)) %>%
  arrange(species) %>% print(n = Inf)



