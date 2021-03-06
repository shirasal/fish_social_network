
# source("R/run_models.R")

# Extract positive and negative covariate standardised coefficients (effect size) on each taxa:

grps_coefs <- std_coefs(taxa = grps_spat)
dip_coefs <- std_coefs(taxa = dip_spat)
herb_coefs <- std_coefs(taxa = herb_spat)

# Calculate the mean effect size (and it's sd) of each covariate

mean_effect_size <- list(groupers = grps_coefs,
     seabream = dip_coefs,
     herbivores = herb_coefs) %>%
  bind_rows(.id = "taxa") %>% 
  pivot_longer(4:length(.),
               names_to = "covariate") %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_coefficient")) %>%
  group_by(taxa, covariate, direction) %>% 
  summarise(mean_es = mean(value, na.rm = TRUE)) %>% 
  spread(covariate, mean_es, fill = 0)

sd_effect_size <- list(groupers = grps_coefs,
     seabream = dip_coefs,
     herbivores = herb_coefs) %>%
  bind_rows(.id = "taxa") %>% 
  pivot_longer(4:length(.),
               names_to = "covariate") %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_coefficient")) %>%
  group_by(taxa, covariate, direction) %>% 
  summarise(sd_es = sd(value, na.rm = TRUE)) %>% 
  spread(covariate, sd_es, fill = 0)


mean_effect_size %>%
  pivot_longer(cols = 3:ncol(.), names_to = "covariate") %>%
  ggplot() +
  aes(x = covariate, y = value, fill = taxa) +
  geom_abline(aes(intercept = 0, slope = 0), col = "red", linetype = "dashed") +
  stat_summary(geom = "point", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  facet_wrap(~taxa, nrow = 3) +
  # scale_fill_manual(values = wesanderson::wes_palette(n = 3, name = "Darjeeling2")) +
  theme(legend.position = "none")

mean_effect_size %>% 
  pivot_longer(cols = 3:ncol(.), names_to = "covariate") %>% 
  ggplot() +
  aes(x = covariate, y = value, fill = taxa) +
  geom_abline(aes(intercept = 0, slope = 0), col = "red", linetype = "dashed") +
  geom_boxplot() +
  scale_x_discrete(limits = c("anthro", "env", "bio", "bio_anth", "bio_env")) +
  facet_wrap(~taxa, nrow = 1) +
  scale_fill_manual(values = wesanderson::wes_palette(n = 3, name = "Darjeeling2")) +
  theme(legend.position = "none")




