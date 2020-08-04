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


# All in one plot ---------------------------------------------------------

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
  left_join(locs_mpas, by = "loc")
  

# Plot probability of occurrence of species as a function of mpa:
ec_boxplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Epinephelus.costae) +
  geom_boxplot()

em_boxplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Epinephelus.marginatus) +
  geom_boxplot()

mr_boxplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Mycteroperca.rubra) +
  geom_boxplot()

ss_boxplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Serranus.scriba) +
  geom_boxplot()

sc_boxplot <- grps_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Serranus.cabrilla) +
  geom_boxplot()



# Seabream ----------------------------------------------------------------

dip_mpa_predict <- MRFcov::predict_MRF(data = dip_mat, MRF_mod = dip_mod) %>%
  invlogit() %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>%
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  select(loc, 2:6) %>% 
  left_join(locs_mpas, by = "loc")


# Plot probability of occurrence of species as a function of mpa:
da_boxplot <- dip_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Diplodus.annularis) +
  geom_boxplot()

dc_boxplot <- dip_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Diplodus.cervinus) +
  geom_boxplot()

dp_boxplot <- dip_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Diplodus.puntazzo) +
  geom_boxplot()

ds_boxplot <- dip_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Diplodus.sargus) +
  geom_boxplot()

dv_boxplot <- dip_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Diplodus.vulgaris) +
  geom_boxplot()

# Herbivores --------------------------------------------------------------

herb_mpa_predict <- MRFcov::predict_MRF(data = herb_mat, MRF_mod = herb_mod) %>%
  invlogit() %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>%
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  select(loc, 2:6) %>% 
  left_join(locs_mpas, by = "loc")


# Plot probability of occurrence of species as a function of mpa:
sr_boxplot <- herb_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Siganus.rivulatus) +
  geom_boxplot()

sl_boxplot <- herb_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Siganus.luridus) +
  geom_boxplot()

sas_boxplot <- herb_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Sarpa.salpa) +
  geom_boxplot()

sg_boxplot <- herb_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Scarus.ghobban) +
  geom_boxplot()

spc_boxplot <- herb_mpa_predict %>% 
  ggplot() +
  aes(x = mpa, y = Sparisoma.cretense) +
  geom_boxplot()
