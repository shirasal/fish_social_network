source("R/run_models.R")

# Run predictions on the model

# ----------------------------------Temperature-------------------------- #

# Create a dataframe of locations and temperatures
locs <- med_raw %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, tmean) %>% unique()

# Groupers ----------------------------------------------------------------

grps_predict <- MRFcov::predict_MRF(data = grps_mat, MRF_mod = grps_mod) %>%
  invlogit() %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  select(loc, 2:6) %>% 
  left_join(locs, by = "loc")

# Plot probability of occurrence of species as a function of temperature:
ec_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Epinephelus.costae) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Epinephelus.costae), method = "lm", formula = y ~ x)

em_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Epinephelus.marginatus) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Epinephelus.marginatus), method = "lm", formula = y ~ x)

mr_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Mycteroperca.rubra) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Mycteroperca.rubra), method = "lm", formula = y ~ x)

ss_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Serranus.scriba) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Serranus.scriba), method = "lm", formula = y ~ x)

sc_plot <- grps_predict %>% 
  ggplot() +
  aes(x = tmean, y = Serranus.cabrilla) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Serranus.cabrilla), method = "lm", formula = y ~ x)

gridExtra::grid.arrange(ec_plot, em_plot, mr_plot, ss_plot, sc_plot, nrow = 2)


# All on one plot ---------------------------------------------------------

# grps_predict %>% 
#   ggplot() +
#   geom_point(aes(x = tmean, y = Epinephelus.marginatus), col = "cornflowerblue") +
#   # stat_smooth(aes(x = tmean, y = Epinephelus.marginatus), method = "lm", formula = y ~ x, col = "cornflowerblue") +
#   geom_point(aes(x = tmean, y = Epinephelus.costae), col = "cadetblue") +
#   # stat_smooth(aes(x = tmean, y = Epinephelus.costae), method = "lm", formula = y ~ x, col = "cadetblue") +
#   geom_point(aes(x = tmean, y = Mycteroperca.rubra), col = "cyan4") +
#   # stat_smooth(aes(x = tmean, y = Mycteroperca.rubra), method = "lm", formula = y ~ x, col = "cyan4") +
#   geom_point(aes(x = tmean, y = Serranus.scriba), col = "aquamarine3") +
#   # stat_smooth(aes(x = tmean, y = Serranus.scriba), method = "lm", formula = y ~ x, col = "aquamarine3") + 
#   geom_point(aes(x = tmean, y = Serranus.cabrilla), col = "blue4") +
#   # stat_smooth(aes(x = tmean, y = Serranus.cabrilla), method = "lm", formula = y ~ x, col = "blue4")
# 


# Seabream ----------------------------------------------------------------

dip_predict <- MRFcov::predict_MRF(data = dip_mat, MRF_mod = dip_mod) %>%
  invlogit() %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  select(loc, 2:6) %>% 
  left_join(locs, by = "loc")

# Plot probability of occurrence of species as a function of temperature:
da_plot <- dip_predict %>% 
  ggplot() +
  aes(x = tmean, y = Diplodus.annularis) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Diplodus.annularis), method = "lm", formula = y ~ x)

dp_plot <- dip_predict %>% 
  ggplot() +
  aes(x = tmean, y = Diplodus.puntazzo) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Diplodus.puntazzo), method = "lm", formula = y ~ x)

ds_plot <- dip_predict %>% 
  ggplot() +
  aes(x = tmean, y = Diplodus.sargus) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Diplodus.sargus), method = "lm", formula = y ~ x)

dv_plot <- dip_predict %>% 
  ggplot() +
  aes(x = tmean, y = Diplodus.vulgaris) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Diplodus.vulgaris), method = "lm", formula = y ~ x)

dc_plot <- dip_predict %>% 
  ggplot() +
  aes(x = tmean, y = Diplodus.cervinus) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Diplodus.cervinus), method = "lm", formula = y ~ x)

gridExtra::grid.arrange(da_plot, dp_plot, ds_plot, dv_plot, dc_plot, nrow = 2)


# Herbivores --------------------------------------------------------------

herb_predict <- MRFcov::predict_MRF(data = herb_mat, MRF_mod = herb_mod) %>%
  invlogit() %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  select(loc, 2:6) %>% 
  left_join(locs, by = "loc")

# Plot probability of occurrence of species as a function of temperature:
sr_plot <- herb_predict %>% 
  ggplot() +
  aes(x = tmean, y = Siganus.rivulatus) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Siganus.rivulatus), method = "lm", formula = y ~ x)

sl_plot <- herb_predict %>% 
  ggplot() +
  aes(x = tmean, y = Siganus.luridus) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Siganus.luridus), method = "lm", formula = y ~ x)

sas_plot <- herb_predict %>% 
  ggplot() +
  aes(x = tmean, y = Sarpa.salpa) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Sarpa.salpa), method = "lm", formula = y ~ x)

sg_plot <- herb_predict %>% 
  ggplot() +
  aes(x = tmean, y = Scarus.ghobban) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Scarus.ghobban), method = "lm", formula = y ~ x)

spc_plot <- herb_predict %>% 
  ggplot() +
  aes(x = tmean, y = Sparisoma.cretense) +
  geom_point()
  # stat_smooth(aes(x = tmean, y = Sparisoma.cretense), method = "lm", formula = y ~ x)

gridExtra::grid.arrange(sr_plot, sl_plot, sas_plot, sg_plot, spc_plot, nrow = 2)

# ----------------------------------MPA-------------------------- #

# Create a dataframe of locations and MPA
locs_mpas <- med_clean %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, mpa)

# Groupers ----------------------------------------------------------------

grps_mpa_predict <- MRFcov::predict_MRF(data = grps_mat, MRF_mod = grps_mod) %>%
  invlogit() %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>%
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  select(loc, 2:6) %>% 
  left_join(locs_mpas, by = "loc") %>% 
  group_by(mpa) %>% 
  

grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa) +
  geom_bar()

# Plot probability of occurrence of species as a function of mpa:
ec_barplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(y = Epinephelus.costae) +
  geom_bar()

em_barplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Epinephelus.marginatus) +
  geom_bar()
  # stat_smooth(aes(x = mpa, y = Epinephelus.marginatus), method = "lm", formula = y ~ x)

mr_barplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Mycteroperca.rubra) +
  geom_bar()
  # stat_smooth(aes(x = mpa, y = Mycteroperca.rubra), method = "lm", formula = y ~ x)

ss_barplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Serranus.scriba) +
  geom_bar()
  # stat_smooth(aes(x = mpa, y = Serranus.scriba), method = "lm", formula = y ~ x)

sc_barplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Serranus.cabrilla) +
  geom_bar()
  # stat_smooth(aes(x = mpa, y = Serranus.cabrilla), method = "lm", formula = y ~ x)

gridExtra::grid.arrange(ec_barplot, em_barplot, mr_barplot, ss_barplot, sc_barplot, nrow = 2)


