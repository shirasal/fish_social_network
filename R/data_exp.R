library(tidyverse)
load("data/data_and_objects.RData")

# Check how many observations for each species
med_raw %>% 
  filter(data.origin != "azz_asi") %>% 
  mutate(species_name = str_replace(species, "\\.", "\\ ")) %>% 
  group_by(species_name) %>% summarise(count = n()) %>% 
  arrange(desc(count))
  # write_csv("C:/Users/shira/Documents/MSc/thesis/Presentation/species_count_observations.csv")

# Check in how many sites species appear:
med_raw %>%
  group_by(species) %>% count(site) %>%
  group_by(species) %>% summarise(count_site = n()) %>%
  arrange(count_site)
  # write_csv("C:/Users/shira/Documents/MSc/thesis/Presentation/species_count_sites.csv")

# Potential species I'd like to include in my study:
dip_test <- med_raw %>% filter(str_detect(species, "Diplodus")) %>% distinct(species) %>% unname() %>% simplify()
grps_test <- med_raw %>% filter(str_detect(species, "Epinephelus|Serranus|Mycteroperca.rubra")) %>% distinct(species) %>% unname() %>% simplify()
herb_test <- med_raw %>% filter(species %in% c(herbivores, "Scarus.ghobban")) %>% distinct(species) %>% unname() %>% simplify()

# Check in how many transects species appear:
med_raw %>%
  mutate(guild = case_when(species %in% dip_test ~ "diplodus",
                           species %in% grps_test ~ "groupers",
                           species %in% herb_test ~ "herbivores")) %>%
  group_by(guild, species) %>% count(site, trans) %>%
  group_by(guild, species) %>% summarise(count_trans = n()) %>%
  filter(!is.na(guild)) %>%
  arrange(guild, count_trans) %>% print(n = Inf)


# Check in how many sites species appear:
med_raw %>%
  mutate(guild = case_when(species %in% dip_test ~ "diplodus",
                           species %in% grps_test ~ "groupers",
                           species %in% herb_test ~ "herbivores")) %>%
  group_by(guild, species) %>% count(site) %>%
  group_by(guild, species) %>% summarise(count_site = n()) %>%
  filter(!is.na(guild)) %>%
  arrange(guild, count_site) %>% print(n = Inf)
