
source("R/run_models.R")

# Extract positive and negative covariate standardised coefficients (effect size) on each taxa:

grps_coefs <- std_coefs(taxa = grps_mod)
dip_coefs <- std_coefs(taxa = dip_mod)
herb_coefs <- std_coefs(taxa = herb_mod)

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


