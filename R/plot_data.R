source("R/packages.R")
load("data/data_and_objects.RData")

med_raw %>% 
  filter(species == all_of(c(groupers, diplodus, herbivores))) %>% 
  filter(data.origin != "azz_asi") %>% 
  mutate(guild = case_when(species %in% groupers ~ "Groupers",
                           species %in% diplodus ~ "Seabreams",
                           species %in% herbivores ~ "Herbivores")) %>% 
  ggplot() + 
  aes(x = tmean, y = log10(sp.n)) + 
  geom_point(aes(col = guild)) + 
  facet_wrap(~species) + 
  xlab("Temperature") + ylab("Abundance (log)") +
  scale_color_manual("Guild", 
                     values = c("Groupers" = guild_colours$grps,
                                "Seabreams" = guild_colours$dip ,
                                "Herbivores" = guild_colours$herb)) +
  theme(strip.text = element_text(face = "italic"))

species_temp <- lapply(as.list(c(groupers, diplodus, herbivores)), function(sp) {
  med_raw %>% 
    filter(species == sp) %>% 
    filter(data.origin != "azz_asi") %>% 
    ggplot() + 
    aes(x = tmean, y = sp.n) + 
    geom_point() +
    ggtitle(str_replace(sp, "\\.", "\\ ")) + xlab("") + ylab("") +
    theme(title = element_text(face = "italic"))
})

ggpubr::ggarrange(plotlist = species_temp, ncol = 5, nrow = 3) %>% 
  ggpubr::annotate_figure(bottom = ggpubr::text_grob("Temperature (C)"),
                left = ggpubr::text_grob("Abundance", rot = 90))
ggsave("species_dist_temp.png", device = "png", path = "figures", width = 15, height = 10, units = "in")




med_raw %>% 
  mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE)) %>% 
  filter(species == all_of(c(groupers, diplodus, herbivores))) %>% 
  filter(data.origin != "azz_asi") %>% 
  mutate(guild = case_when(species %in% groupers ~ "Groupers",
                           species %in% diplodus ~ "Seabreams",
                           species %in% herbivores ~ "Herbivores")) %>% 
  na.omit() %>% 
  ggplot() + 
  aes(x = mpa, y = sp.n) + 
  geom_bar(aes(fill = guild), stat = "identity") + 
  scale_y_log10() +
  facet_wrap(~species) + 
  xlab("MPA") + ylab("Abundance (log)") +
  scale_fill_manual("Guild", 
                     values = c("Groupers" = "#eccbae", "Seabreams" = "#d29a4c" , "Herbivores" = "#145d82")) +
  theme(strip.text = element_text(face = "italic"))

species_mpa <- lapply(as.list(c(groupers, diplodus, herbivores)), function(sp) {
  med_raw %>% 
    mutate(mpa = if_else(enforcement <= 1, FALSE, TRUE)) %>% 
    filter(species == sp) %>% 
    filter(data.origin != "azz_asi") %>% 
    na.omit() %>% 
    ggplot() + 
    aes(x = mpa, y = sp.n) + 
    geom_point() + 
    ggtitle(str_replace(sp, "\\.", "\\ ")) + xlab("") + ylab("") +
    theme(title = element_text(face = "italic"))
})

ggpubr::ggarrange(plotlist = species_mpa, ncol = 5, nrow = 3) %>% 
  ggpubr::annotate_figure(bottom = ggpubr::text_grob("MPA"),
                          left = ggpubr::text_grob("Abundance", rot = 90))
ggsave("species_dist_mpa.png", device = "png", path = "figures", width = 15, height = 10, units = "in")
