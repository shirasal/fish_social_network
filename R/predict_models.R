source("R/run_models.R")

# Run predictions on the model

# ----------------------------------Temperature-------------------------- #

# Create a dataframe of locations and temperatures
locs <- med_raw %>% mutate(loc = paste0(site, "_", trans)) %>% select(loc, tmean) %>% unique()


# Groupers ----------------------------------------------------------------

grps_predict <- MRFcov::predict_MRF(data = grps_mat, MRF_mod = grps_spat) %>%
  `colnames<-`(groupers) %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  pivot_longer(2:length(.),
               names_to = "species",
               values_to = "predict_obs") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  left_join(locs, by = "loc") %>% 
  mutate(temp = scale(tmean)) %>% 
  select(loc, tmean, 2:5)

# Plot probability of occurrence of species as a function of temperature:
grps_predict %>%
  group_by(loc, tmean, species) %>%
  summarise(mean_obs = mean(predict_obs), .groups = "keep") %>%
  ggplot() +
  aes(x = tmean, y = mean_obs, col = species) +
  geom_point(cex = 3, alpha = 0.6) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.3) +
  scale_color_paletteer_d("ggsci::uniform_startrek") +
  xlab("Temperature (degC)") +
  ylab("Predicted observations") +
  labs(title = "Observation predictions", subtitle = "Groupers")


# Seabream ----------------------------------------------------------------

dip_predict <- MRFcov::predict_MRF(data = dip_mat, MRF_mod = dip_spat) %>%
  `colnames<-`(diplodus) %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  pivot_longer(2:length(.),
               names_to = "species",
               values_to = "predict_obs") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  left_join(locs, by = "loc") %>% 
  mutate(temp = scale(tmean)) %>% 
  select(loc, tmean, 2:5)

# Plot probability of occurrence of species as a function of temperature:
dip_predict %>%
  group_by(loc, tmean, species) %>%
  summarise(mean_obs = mean(predict_obs), .groups = "keep") %>%
  ggplot() +
  aes(x = tmean, y = mean_obs, col = species) +
  geom_point(cex = 3, alpha = 0.6) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.3) +
  scale_color_paletteer_d("ggsci::uniform_startrek") +
  xlab("Temperature (degC)") +
  ylab("Predicted observations") +
  labs(title = "Observation predictions", subtitle = "Seabreams")


# Herbivores --------------------------------------------------------------

herb_predict <- MRFcov::predict_MRF(data = herb_mat, MRF_mod = herb_spat) %>%
  `colnames<-`(herbivores) %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  pivot_longer(2:length(.),
               names_to = "species",
               values_to = "predict_obs") %>% 
  mutate(loc = stringr::str_replace(string = .$site, " ", "_")) %>% 
  left_join(locs, by = "loc") %>% 
  mutate(temp = scale(tmean)) %>% 
  select(loc, tmean, 2:5)

# Plot probability of occurrence of species as a function of temperature:
herb_predict %>%
  group_by(loc, tmean, species) %>%
  summarise(mean_obs = log(mean(predict_obs)+0.1), .groups = "keep") %>%
  ggplot() +
  aes(x = tmean, y = mean_obs, col = species) +
  geom_point(cex = 3, alpha = 0.6) +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.3) +
  scale_color_paletteer_d("ggsci::uniform_startrek") +
  xlab("Temperature (degC)") +
  ylab("Predicted observations, log scaled") +
  labs(title = "Observation predictions", subtitle = "Herbivores")


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
