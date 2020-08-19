# source("R/run_models.R")
source("R/")# source("R/run_models.R")
# source("R/run_models_spatial.R")

# rm(list = c("grps_mod_nocov", "dip_mod_nocov", "herb_mod_nocov", 
#             "grpsHM_nocov", "dipHM_nocov", "herbHM_nocov"))

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

all_relimp %>% bind_rows(.id = "taxa") %>% pivot_longer(3:length(.)) %>% # Create a tibble of all taxa
  rename(taxa = taxa, species = species, covariate = name, rel_imp = value) %>%
  mutate(covariate = str_remove(string = covariate, pattern = "_rel_imp")) %>% 
  ggplot() +
  aes(x = covariate, y = rel_imp, fill = taxa)+
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
  scale_fill_manual(values = wesanderson::wes_palette(n = 3, name = "Darjeeling2")) +
  labs(title = "Relative importance of factors in the model", subtitle = "All three taxa mean")
