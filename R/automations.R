# Functions I need to work on

# Pairwise raw data in two scenarios
## Temperature
scenarios_temp <- function(spp_mat, sp_i, sp_j, guild_col){
  spp_mat %>%
  mutate(species_j = if_else(sp_j == 0, "absent", "present")) %>%
  ggplot() +
  aes(x = temp, y = sp_i) +
  geom_point(col = guild_col) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "darkcyan", alpha = 0.3) +
  xlab("Temperature") + ylab(str_glue("{sp_i} Abundance")) +
  facet_wrap(~species_j, ncol = 2)
}