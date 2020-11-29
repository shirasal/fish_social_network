source("R/packages.R")
load("data/all_objects.RData")

med_raw %>% 
  filter(species == all_of(c(groupers, diplodus, herbivores))) %>% 
  filter(data.origin != "azz_asi") %>% 
  mutate(guild = case_when(species %in% groupers ~ "Groupers",
                           species %in% diplodus ~ "Seabreams",
                           species %in% herbivores ~ "Herbivores")) %>% 
  ggplot() + 
  aes(x = tmean, y = sp.n) + 
  geom_point(aes(col = guild)) + 
  scale_y_log10() +
  facet_wrap(~species) + 
  xlab("Temperature") + ylab("Abundance (log)") +
  scale_color_manual("Guild", 
                     values = c("Groupers" = "#eccbae", "Seabreams" = "#d29a4c" , "Herbivores" = "#145d82")) +
  theme(strip.text = element_text(face = "italic"))

species_temp <- lapply(as.list(c(groupers, diplodus, herbivores)), function(sp) {
  med_raw %>% 
    filter(species == sp) %>% 
    filter(data.origin != "azz_asi") %>% 
    ggplot() + 
    aes(x = tmean, y = sp.n) + 
    geom_point() +
    ggtitle(str_replace(sp, "\\.", "\\ ")) + xlab("Temperature") + ylab("Abundance") +
    theme(title = element_text(face = "italic"))
})

ggpubr::ggarrange(plotlist = species_temp, ncol = 4, nrow = 4)
ggsave("species_dist_temp.png", device = "png", path = "figures", scale = 3)
