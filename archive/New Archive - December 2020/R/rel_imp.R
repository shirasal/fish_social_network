# source("R/functions.R")
# load("data/all_objects.RData")
# source("R/packages.R")
# source("R/run_models.R")

# Figure 2. Relative importance per taxa ----------------------------------
# How much is does a predictor affect the data
grps_relimp <- rel_imp_sum(grps_mod)
dip_relimp <- rel_imp_sum(dip_mod)
herb_relimp <- rel_imp_sum(herb_mod)

all_relimp <- list(groupers = grps_relimp,
                   seabream = dip_relimp,
                   herbivores = herb_relimp)

# Table of mean relative importance of covariates per taxa:
relimp_table <- sapply(X = all_relimp, FUN = function(x) x[,-1] %>% colMeans())

all_relimp_p <- all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>% # Create a tibble of all taxa
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>%
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = taxa) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = wesanderson::wes_palette(n = 3, name = "Darjeeling2")) +
  labs(title = "Relative importance of factors in the model", subtitle = "All three taxa mean")

# ggsave("rel_imp_all_taxa_nonspat.png", all_relimp_p, "png", "figures/", dpi = 150)

# Relative importance of covariates for each taxa -------------------------

grps_relimp_p <- grps_relimp %>% pivot_longer(2:length(.)) %>% # Create a tibble of all species
  rename(species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  mutate(facet.title = case_when(covariate == "env" ~ "Environment",
                                 covariate == "anthro" ~ "MPA",
                                 covariate == "biotic" ~ "Biotic Associations",
                                 covariate == "env_bio" ~ "Temp * Biotic",
                                 covariate == "mpa_bio" ~ "MPA * Biotic")) %>% 
  mutate(facet.title = fct_relevel(facet.title, 
                                   "Environment", "MPA", "Biotic Associations",
                                   "Temp * Biotic", "MPA * Biotic")) %>% 
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = "#eccbae") +
  facet_wrap(~facet.title, nrow = 1) +
  labs(subtitle = "Groupers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))

# ggsave("rel_imp_grps_spat.png", grps_relimp_p, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")

dip_relimp_p <- dip_relimp %>% pivot_longer(2:length(.)) %>% # Create a tibble of all species
  rename(species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  mutate(facet.title = case_when(covariate == "env" ~ "Environment",
                                 covariate == "anthro" ~ "MPA",
                                 covariate == "biotic" ~ "Biotic Associations",
                                 covariate == "env_bio" ~ "Temp * Biotic",
                                 covariate == "mpa_bio" ~ "MPA * Biotic")) %>% 
  mutate(facet.title = fct_relevel(facet.title, 
                                   "Environment", "MPA", "Biotic Associations",
                                   "Temp * Biotic", "MPA * Biotic")) %>% 
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = "#d69c4e") +
  facet_wrap(~facet.title, nrow = 1) +
  labs(subtitle = "Seabreams") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))

# ggsave("rel_imp_diplodus_spat.png", dip_relimp_p, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")

herb_relimp_p <- herb_relimp %>% pivot_longer(2:length(.)) %>% # Create a tibble of all species
  rename(species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  mutate(facet.title = case_when(covariate == "env" ~ "Environment",
                                 covariate == "anthro" ~ "MPA",
                                 covariate == "biotic" ~ "Biotic Associations",
                                 covariate == "env_bio" ~ "Temp * Biotic",
                                 covariate == "mpa_bio" ~ "MPA * Biotic")) %>% 
  mutate(facet.title = fct_relevel(facet.title, 
                                   "Environment", "MPA", "Biotic Associations",
                                   "Temp * Biotic", "MPA * Biotic")) %>% 
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = "#046c9a") +
  facet_wrap(~facet.title, nrow = 1) +
  labs(subtitle = "Herbivores") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))

# ggsave("rel_imp_dherb_spat.png", herb_relimp_p, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")

species_relimp_plots <- egg::ggarrange(grps_relimp_p, dip_relimp_p, herb_relimp_p)

# ggsave("rel_imp_species_nonspat.png", species_relimp_plots, "png", "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")



