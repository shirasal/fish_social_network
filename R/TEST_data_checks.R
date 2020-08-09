
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

