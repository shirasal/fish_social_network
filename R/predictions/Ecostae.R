
grps_fake1 <- med_clean %>%
  group_by_at(.vars = c(c("lat", "lon", "site", "trans", "species"), env_vector, anthro_vector)) %>%
  summarise(n = sum(sp.n)) %>%
  spread(species, n, fill = 0) %>%
  ungroup() %>%
  na.omit() %>%
  mutate(Epinephelus.marginatus = 1,
         Mycteroperca.rubra = 0,
         Serranus.scriba = 0,
         Serranus.cabrilla = 0,
         loc = paste(site, trans)) %>%
  column_to_rownames("loc") %>%
  select(all_of(groupers), all_of(env_vector), all_of(anthro_vector))

grps_fake1_mod <- MRFcov_spatial(grps_fake1, n_nodes = 5, n_covariates = 4, coords = grps_coords,
                                 family = "poisson", n_cores = 8)

grps_fake_pred <- MRFcov::predict_MRF(data = grps_fake1, MRF_mod = grps_fake1_mod) %>%
  `colnames<-`(groupers) %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  pivot_longer(2:length(.),
               names_to = "species",
               values_to = "predict_obs") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  left_join(locs, by = "loc") %>% 
  select(loc, tmean, 2:5)

# Plot probability of occurrence of species as a function of temperature:
grps_fake_pred %>%
  filter(species == "Epinephelus.costae") %>% 
  ggplot() +
  aes(x = tmean, y = predict_obs) +
  geom_point(cex = 3, alpha = 0.6, col = "#CC0C00FF") +
  stat_smooth(method = "lm", formula = y ~ x, col = "#CC0C00FF") +
  xlab("Temperature (degC)") +
  ylab("Predicted observations") +
  labs(title = "Observation predictions for fake data", subtitle = "Epinephelus costae")

grps_predict %>%
  filter(species == "Epinephelus.costae") %>% 
  ggplot() +
  aes(x = tmean, y = predict_obs) +
  geom_point(cex = 3, alpha = 0.6, col = "#CC0C00FF") +
  stat_smooth(method = "lm", formula = y ~ x, col = "#CC0C00FF") +
  xlab("Temperature (degC)") +
  ylab("Predicted observations") +
  labs(title = "Observation predictions for real data", subtitle = "Epinephelus costae")
