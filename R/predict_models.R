source("R/run_models.R")

# Create a dataframe of locations and temperatures
locs <- med_raw %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, tmean) %>% unique()

# Run predictions on the model
grps_predict <- MRFcov::predict_MRF(data = grps_mat, MRF_mod = grps_mod) %>%
  arm::invlogit() %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  select(loc, 2:6) %>% 
  left_join(locs, by = "loc")

# Plot probability of occurrence of species as a function of temperature:
ec_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Epinephelus.costae) +
  geom_point() +
  stat_smooth(aes(x = tmean, y = Epinephelus.costae), method = "lm", formula = y ~ x)

em_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Epinephelus.marginatus) +
  geom_point() +
  stat_smooth(aes(x = tmean, y = Epinephelus.marginatus), method = "lm", formula = y ~ x)

mr_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Mycteroperca.rubra) +
  geom_point() +
  stat_smooth(aes(x = tmean, y = Mycteroperca.rubra), method = "lm", formula = y ~ x)

ss_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Serranus.scriba) +
  geom_point() +
  stat_smooth(aes(x = tmean, y = Serranus.scriba), method = "lm", formula = y ~ x)

sc_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Serranus.cabrilla) +
  geom_point() +
  stat_smooth(aes(x = tmean, y = Serranus.cabrilla), method = "lm", formula = y ~ x)

gridExtra::grid.arrange(ec_plot, em_plot, mr_plot, ss_plot, sc_plot, nrow = 2)

# grps_predict %>% 
#   ggplot() +
#   geom_point(aes(x = tmean, y = Epinephelus.marginatus), col = "cornflowerblue") +
#   stat_smooth(aes(x = tmean, y = Epinephelus.marginatus), method = "lm", formula = y ~ x, col = "cornflowerblue") +
#   geom_point(aes(x = tmean, y = Epinephelus.costae), col = "cadetblue") +
#   stat_smooth(aes(x = tmean, y = Epinephelus.costae), method = "lm", formula = y ~ x, col = "cadetblue") +
#   geom_point(aes(x = tmean, y = Mycteroperca.rubra), col = "cyan4") +
#   stat_smooth(aes(x = tmean, y = Mycteroperca.rubra), method = "lm", formula = y ~ x, col = "cyan4") +
#   geom_point(aes(x = tmean, y = Serranus.scriba), col = "aquamarine3") +
#   stat_smooth(aes(x = tmean, y = Serranus.scriba), method = "lm", formula = y ~ x, col = "aquamarine3") + 
#   geom_point(aes(x = tmean, y = Serranus.cabrilla), col = "blue4") +
#   stat_smooth(aes(x = tmean, y = Serranus.cabrilla), method = "lm", formula = y ~ x, col = "blue4")
# 
