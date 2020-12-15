bind_rows(grps_pois$key_coefs, .id = "name") %>% arrange(name, Variable) %>% 
  separate(col = Variable, into = c("Covariate", "Interaction"), sep = "_") %>% 
  mutate(name = str_replace(name, "\\.", "\\ "),
         Interaction = str_replace(Interaction, "\\.", "\\ ")) %>% 
  rename(Species = name, Coefficient = Raw_coef) %>% select(-Standardised_coef) %>% 
  write_csv("data/grps_key_coefs.csv")

bind_rows(dip_pois$key_coefs, .id = "name") %>% arrange(name, Variable) %>%
  separate(col = Variable, into = c("Covariate", "Interaction"), sep = "_") %>% 
  mutate(name = str_replace(name, "\\.", "\\ "),
         Interaction = str_replace(Interaction, "\\.", "\\ ")) %>% 
  rename(Species = name, Coefficient = Raw_coef) %>% select(-Standardised_coef) %>% write_csv("data/dip_key_coefs.csv")

bind_rows(herb_pois$key_coefs, .id = "name") %>% arrange(name, Variable) %>%
  separate(col = Variable, into = c("Covariate", "Interaction"), sep = "_") %>% 
  mutate(name = str_replace(name, "\\.", "\\ "),
         Interaction = str_replace(Interaction, "\\.", "\\ ")) %>% 
  rename(Species = name, Coefficient = Raw_coef) %>% select(-Standardised_coef) %>% write_csv("data/herb_key_coefs.csv")
