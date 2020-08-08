
# Check covariates --------------------------------------------------------

med_raw %>% glimpse()

med_raw %>% 
  ggplot() +
  aes(x = lon, y = tmean) + 
  geom_point(col = "darkred")

med_clean %>% 
  ggplot() + 
  aes(x = lon, y = temp) +
  geom_point(col = "darkblue")

# Poisson vs. Gaussian on transformed -------------------------------------

# Create matrix of scaled covariates and abundances to run with 'gaussian':
med_scaled <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE),
         temp = scale(tmean),
         depth = scale(depth),
         prod = scale(pp_mean),
         sp.n = scale(sp.n)) %>%
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)
spp_mat_scaled <- create_spp_mat(dataset = med_scaled, taxa = groupers, covariate = c(as.array(env_vector), as.array(anthro_vector)))

# Run model on standardised data
gaus <- MRFcov(spp_mat_scaled, n_nodes = 5, n_covariates = 4, family = "gaussian")

# Create matrix of UNSCALED data
med_data <- med_raw %>%
  filter(data.origin != "azz_asi") %>% # presence-absence
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE)) %>% 
  rename(temp = tmean, depth = depth, prod = pp_mean) %>% 
  select(site, lon, lat, trans, species, sp.n, mpa, temp, depth, prod)
spp_mat <- create_spp_mat(dataset = med_data, taxa = groupers, covariate = c(as.array(env_vector), as.array(anthro_vector)))

# Run model with raw data and family 'poisson'
pois <- MRFcov(spp_mat, n_nodes = 5, n_covariates = 4, family = "poisson")

plotMRF_hm(gaus, main = "Gaussian")
plotMRF_hm(pois, main = "Poisson")


# Run model on presence-absence data --------------------------------------
pa_mat <- med_data %>%
  mutate(temp = scale(temp),
         depth = scale(depth),
         prod = scale(prod)) %>% 
  create_spp_mat(taxa = groupers, covariate = c(as.array(env_vector), as.array(anthro_vector)))
pa_mat[,1:5] <- ifelse(pa_mat[,1:5] > 0, 1, 0)
pa_mod <- MRFcov(data = pa_mat, n_nodes = length(groupers), n_covariates = 4, family = "binomial")
plotMRF_hm(pa_mod, main = "Binomial")


# Data transformations (abundance) ----------------------------------------

med_raw %>% 
  filter(species %in% groupers) %>% 
  ggplot() + 
  aes(x = species, y = sp.n) +
  geom_violin(fill = "#eccbae") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Groupers - raw")

# transformation according to 'vignette("Gaussian_Poisson_CRFs")'
med_raw %>% 
  filter(species %in% groupers) %>% 
  mutate(n_trans = log2(sp.n + 0.1)) %>% 
  ggplot() + 
  aes(x = species, y = n_trans) +
  geom_violin(fill = "#eccbae") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Groupers - nonpararnormal transformed")

# standartisation
med_raw %>% 
  mutate(n_trans = scale(sp.n)) %>% 
  filter(species %in% groupers) %>% 
  ggplot() + 
  aes(x = species, y = n_trans) +
  geom_violin(fill = "#eccbae") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Groupers - standaradised per whole dataset")

med_raw %>% 
  filter(species %in% groupers) %>% 
  mutate(n_trans = scale(sp.n)) %>% 
  ggplot() + 
  aes(x = species, y = n_trans) +
  geom_violin(fill = "#eccbae") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Groupers - standaradised per all species in taxa")

