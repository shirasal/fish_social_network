source("R/packages.R")
source("R/functions.R")
load("data/data_and_objects.RData")


# Nonspatial Poisson CRF --------------------------------------------------

grps_pois <- MRFcov(grps_mat, n_nodes = 4, family = "poisson")
dip_pois <- MRFcov(dip_mat, n_nodes = 4, family = "poisson")
herb_pois <- MRFcov(herb_mat, n_nodes = 4, family = "poisson")

## Relative importance summary
grps_pois_relimp <- rel_imp_sum(grps_pois)
dip_pois_relimp <- rel_imp_sum(dip_pois)
herb_pois_relimp <- rel_imp_sum(herb_pois)

p_relimp_grps_pois <- plot_relimp(grps_pois_relimp, guild_colours$grps, "Groupers")
# ggsave("p_relimp_grps_pois_nonspat.png", p_relimp_grps_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_dip_pois <- plot_relimp(dip_pois_relimp, guild_colours$dip, "Seabreams")
# ggsave("p_relimp_dip_pois_nonspat.png", p_relimp_dip_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_herb_pois <- plot_relimp(herb_pois_relimp, guild_colours$herb, "Herbivores")
# ggsave("p_relimp_herb_pois_nonspat.png", p_relimp_herb_pois, "png", "figures/rel_imp/", dpi = 300, width = 11.74, height = 4, units = "in")
p_relimp_pois <- egg::ggarrange(p_relimp_grps_pois, p_relimp_dip_pois, p_relimp_herb_pois)
# ggsave(filename = "rel_imp_pois_nonspat.png", device = "png", path = "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")

# Relative importance means
colMeans(grps_pois_relimp[2:ncol(grps_pois_relimp)])
colMeans(dip_pois_relimp[2:ncol(dip_pois_relimp)])
colMeans(herb_pois_relimp[2:ncol(herb_pois_relimp)])

# Mean relative importance explained
rowSums(grps_pois_relimp[2:ncol(grps_pois_relimp)]) %>% mean()
rowSums(dip_pois_relimp[2:ncol(dip_pois_relimp)]) %>% mean()
rowSums(herb_pois_relimp[2:ncol(herb_pois_relimp)]) %>% mean()

# Nonstationary effect
grps_pois_relimp %>% 
  pivot_longer(2:6, names_to = "type", values_to = "rel_imp") %>% 
  mutate(nonstationary = str_detect(type, "_bio_")) %>% 
  group_by(species, nonstationary) %>% 
  summarise(sum = sum(rel_imp)) %>% 
  group_by(nonstationary) %>% 
  summarise(mean(sum))

dip_pois_relimp %>% 
  pivot_longer(2:6, names_to = "type", values_to = "rel_imp") %>% 
  mutate(nonstationary = str_detect(type, "_bio_")) %>% 
  group_by(species, nonstationary) %>% 
  summarise(sum = sum(rel_imp)) %>% 
  group_by(nonstationary) %>% 
  summarise(mean(sum))

herb_pois_relimp %>% 
  pivot_longer(2:6, names_to = "type", values_to = "rel_imp") %>% 
  mutate(nonstationary = str_detect(type, "_bio_")) %>% 
  group_by(species, nonstationary) %>% 
  summarise(sum = sum(rel_imp)) %>% 
  group_by(nonstationary) %>% 
  summarise(mean(sum))

# # Spatial Poisson CRF -----------------------------------------------------
# 
# med_coords <- med_clean %>% 
#   distinct(site, trans, lat, lon) %>% 
#   mutate(loc = paste(site, trans)) %>% 
#   column_to_rownames("loc") %>% 
#   select(lon, lat)
# grps_coords <- med_coords %>% 
#   filter(rownames(med_coords) %in% rownames(grps_mat))
# dip_coords <- med_coords %>% 
#   filter(rownames(med_coords) %in% rownames(dip_mat))
# herb_coords <- med_coords %>% 
#   filter(rownames(med_coords) %in% rownames(herb_mat))
# 
# grps_pois_spat <- MRFcov_spatial(grps_mat, n_nodes = 4, family = "poisson", coords = grps_coords)
# dip_pois_spat <- MRFcov_spatial(dip_mat, n_nodes = 4, family = "poisson", coords = dip_coords)
# herb_pois_spat <- MRFcov_spatial(herb_mat, n_nodes = 4, family = "poisson", coords = herb_coords)
# 
# ## Relative importance summary
# grps_pois_spat_relimp <- rel_imp_sum(grps_pois_spat)
# dip_pois_spat_relimp <- rel_imp_sum(dip_pois_spat)
# herb_pois_spat_relimp <- rel_imp_sum(herb_pois_spat)
# 
# p_relimp_grps_pois_spat <- plot_relimp(grps_pois_spat_relimp, guild_colours$grps, "Groupers")
# p_relimp_dip_pois_spat <- plot_relimp(dip_pois_spat_relimp, guild_colours$dip, "Diplodus")
# p_relimp_herb_pois_spat <- plot_relimp(herb_pois_spat_relimp, guild_colours$herb, "Herbivores")
# p_relimp_pois_spat <- egg::ggarrange(p_relimp_grps_pois_spat, p_relimp_dip_pois_spat, p_relimp_herb_pois_spat)
# # ggsave(filename = "rel_imp_pois_spat.png", device = "png", path = "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")
# 
# 
# 
# # Nonspatial Gaussian CRF with scaled abundances --------------------------
# 
# std_grps_mat <- grps_mat %>% mutate(across(1:4, .fns = scale))
# std_dip_mat <- dip_mat %>% mutate(across(1:4, .fns = scale))
# std_herb_mat <- herb_mat %>% mutate(across(1:4, .fns = scale))
# 
# grps_gaus <- MRFcov(std_grps_mat, n_nodes = 4, family = "gaussian")
# dip_gaus <- MRFcov(std_dip_mat, n_nodes = 4, family = "gaussian")
# herb_gaus <- MRFcov(std_herb_mat, n_nodes = 4, family = "gaussian")
# 
# ## Relative importance summary
# grps_gaus_relimp <- rel_imp_sum(grps_gaus)
# dip_gaus_relimp <- rel_imp_sum(dip_gaus)
# herb_gaus_relimp <- rel_imp_sum(herb_gaus)
# 
# p_relimp_grps_gaus <- plot_relimp(grps_gaus_relimp, guild_colours$grps, "Groupers")
# p_relimp_dip_gaus <- plot_relimp(dip_gaus_relimp, guild_colours$dip, "Diplodus")
# p_relimp_herb_gaus <- plot_relimp(herb_gaus_relimp, guild_colours$herb, "Herbivores")
# p_relimp_gaus_std <- egg::ggarrange(p_relimp_grps_gaus, p_relimp_dip_gaus, p_relimp_herb_gaus)
# # ggsave(filename = "rel_imp_gaus_nonspat.png", device = "png", path = "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")
# 
# 
# 
# # Nonspatial, Gaussian CRF on Abundance Rankings --------------------------
# 
# grps_rank <- grps_mat %>% 
#   mutate(across(1:4, .fns = rank))
# dip_rank <- dip_mat %>% 
#   mutate(across(1:4, .fns = rank))
# herb_rank <- herb_mat %>% 
#   mutate(across(1:4, .fns = rank))
# 
# # Check the rankings, because something here is odd
# grps_rank %>% 
#   pivot_longer(cols = all_of(groupers), names_to = "species", values_to = "ranking") %>% 
#   summarise(median(ranking))
# 
# grps_rank %>% 
#   pivot_longer(cols = all_of(groupers), names_to = "species", values_to = "ranking") %>% 
#   group_by(species) %>% 
#   summarise(median(ranking))
# 
# grps_rank %>% 
#   pivot_longer(cols = all_of(groupers), names_to = "species", values_to = "rank") %>% 
#   ggplot() +
#   aes(rank) +
#   geom_histogram(fill = guild_colours$grps, binwidth = 200) +
#   xlab("Rank") + ylab("Frequency") +
#   facet_wrap(~species)
# 
# dip_rank %>% 
#   pivot_longer(cols = all_of(diplodus), names_to = "species", values_to = "rank") %>% 
#   ggplot() +
#   aes(rank) +
#   geom_histogram(fill = guild_colours$dip, binwidth = 200) +
#   xlab("Rank") + ylab("Frequency") +
#   facet_wrap(~species)
# 
# herb_rank %>% 
#   pivot_longer(cols = all_of(herbivores), names_to = "species", values_to = "rank") %>% 
#   ggplot() +
#   aes(rank) +
#   geom_histogram(fill = guild_colours$herb, binwidth = 200) +
#   xlab("Rank") + ylab("Frequency") +
#   facet_wrap(~species)
# 
# grps_rank_mod <- MRFcov(grps_rank, n_nodes = 4, family = "gaussian")
# dip_rank_mod <- MRFcov(dip_rank, n_nodes = 4, family = "gaussian")
# herb_rank_mod <- MRFcov(herb_rank, n_nodes = 4, family = "gaussian")
# 
# ## Relative importance summary
# grps_rank_relimp <- rel_imp_sum(grps_rank_mod)
# dip_rank_relimp <- rel_imp_sum(dip_rank_mod)
# herb_rank_relimp <- rel_imp_sum(herb_rank_mod)
# 
# p_relimp_grps_rank <- plot_relimp(grps_rank_relimp, guild_colours$grps, "Groupers")
# p_relimp_dip_rank <- plot_relimp(dip_rank_relimp, guild_colours$dip, "Diplodus")
# p_relimp_herb_rank <- plot_relimp(herb_rank_relimp, guild_colours$herb, "Herbivores")
# p_relimp_rank <- egg::ggarrange(p_relimp_grps_rank, p_relimp_dip_rank, p_relimp_herb_rank)
# # ggsave(filename = "rel_imp_rank_nonspat.png", device = "png", path = "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")
# 
# 

# Coefficients ------------------------------------------------------------
# Poisson non-spatial

grps_coefs <- coefs_sum(grps_pois) %>%
  pivot_longer(cols = 3:8) %>%
  mutate(coefficient_type = str_remove(.$name, "_coef")) %>%
  na.omit(value) %>%
  select(species, coefficient_type, value, direction) %>%
  arrange(coefficient_type, species, direction)

dip_coefs <- coefs_sum(dip_pois) %>%
  pivot_longer(cols = 3:8) %>%
  mutate(coefficient_type = str_remove(.$name, "_coef")) %>%
  na.omit(value) %>%
  select(species, coefficient_type, value, direction) %>%
  arrange(coefficient_type, species, direction)



