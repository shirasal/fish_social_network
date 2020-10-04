# source("R/run_models.R")
# rm(list = c("grps_mod_nocov", "dip_mod_nocov", "herb_mod_nocov", 
#             "grpsHM_nocov", "dipHM_nocov", "herbHM_nocov"))
# source("R/run_models_spatial.R")

# Figure 2. Relative importance per taxa ----------------------------------
# How much is does a predictor affect the data
grps_relimp <- rel_imp_sum(grps_spat)
dip_relimp <- rel_imp_sum(dip_spat)
herb_relimp <- rel_imp_sum(herb_spat)

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

# ggsave("rel_imp_all_taxa.png", all_relimp_p, "png", "results/", dpi = 150)

# Relative importance of covariates for each taxa -------------------------

grps_relimp_p <- grps_relimp %>% pivot_longer(2:length(.)) %>% # Create a tibble of all species
  rename(species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = "#eccbae") +
  facet_wrap(~covariate, nrow = 1) +
  labs(subtitle = "Groupers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# ggsave("rel_imp_groupers.png", grps_relimp_p, "png", "results/",
#        dpi = 150, height = 8, width = 7, units = "in")

dip_relimp_p <- dip_relimp %>% pivot_longer(2:length(.)) %>% # Create a tibble of all species
  rename(species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = "#d69c4e") +
  facet_wrap(~covariate, nrow = 1) +
  labs(subtitle = "Seabream") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# ggsave("rel_imp_diplodus.png", dip_relimp_p, "png", "results/",
#        dpi = 150, height = 8, width = 7, units = "in")

herb_relimp_p <- herb_relimp %>% pivot_longer(2:length(.)) %>% # Create a tibble of all species
  rename(species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  ggplot() +
  aes(x = species, y = rel_imp) +
  stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = "#046c9a") +
  facet_wrap(~covariate, nrow = 1) +
  labs(subtitle = "Herbivores") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.placement = "outside") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# ggsave("rel_imp_herbs.png", herb_relimp_p, "png", "results/",
#        dpi = 150, height = 8, width = 7, units = "in")

species_relimp_plots <- egg::ggarrange(grps_relimp_p, dip_relimp_p, herb_relimp_p)

# ggsave("rel_imp_species.png", species_relimp_plots, "png", "results/", dpi = 150, height = 10, width = 10, units = "in")



