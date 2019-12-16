library(vegan)
library(tidyverse)

traits_data <- read.csv("traits/traits.csv", stringsAsFactors = TRUE, row.names = 1) %>% 
  dplyr::select(c(-depth_min, -depth_max))

traits_data$home_range <- relevel(traits_data$home_range, "sed", "mob", "vmob")
traits_data$diet <- relevel(traits_data$diet, "h", "c", "d", "p", "z")
traits_data$size_class <- relevel(traits_data$size_class, "b", "c", "d", "e", "f")
traits_data$schooling <- relevel(traits_data$schooling, "sol", "small", "medium", "large")
traits_data$water_level <- relevel(traits_data$water_level, "bottom", "low", "high")
traits_data$activity <- relevel(traits_data$activity, "day", "both", "night")

head(traits_data)

trait_data_num <- traits_data %>% 
  mutate_all(.funs = as.numeric)

head(trait_data_num)

nmds_traits <- metaMDS(comm = trait_data_num, distance = "gower")


data.scores <- as.data.frame(scores(nmds_traits))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(traits_data)  # create a column of site names, from the rownames of data.scores
data.scores$schooling <- traits_data$schooling  #  add the grp variable
data.scores$mob <- traits_data$home_range  #  add the grp variable
data.scores$level <- traits_data$water_level
data.scores$act <- traits_data$activity
data.scores$size <- traits_data$size_class
data.scores$diet <- traits_data$diet

head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(nmds_traits, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

school.a <- data.scores[data.scores$schooling == "sol", ][chull(data.scores[data.scores$schooling == "sol", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
school.b <- data.scores[data.scores$schooling == "small", ][chull(data.scores[data.scores$schooling == "small", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
school.c <- data.scores[data.scores$schooling == "medium", ][chull(data.scores[data.scores$schooling == "medium", c("NMDS1", "NMDS2")]), ]  # hull values for grp C
school.d <- data.scores[data.scores$schooling == "large", ][chull(data.scores[data.scores$schooling == "large", c("NMDS1", "NMDS2")]), ]  # hull values for grp D

mob.a <- data.scores[data.scores$mob == "sed", ][chull(data.scores[data.scores$mob == "sed", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
mob.b <- data.scores[data.scores$mob == "mob", ][chull(data.scores[data.scores$mob == "mob", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
mob.c <- data.scores[data.scores$mob == "vmob", ][chull(data.scores[data.scores$mob == "vmob", c("NMDS1", "NMDS2")]), ]  # hull values for grp C

lev.a <- data.scores[data.scores$level == "bottom", ][chull(data.scores[data.scores$level == "bottom", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
lev.b <- data.scores[data.scores$level == "low", ][chull(data.scores[data.scores$level == "low", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
lev.c <- data.scores[data.scores$level == "high", ][chull(data.scores[data.scores$level == "high", c("NMDS1", "NMDS2")]), ]  # hull values for grp C

act.a <- data.scores[data.scores$act == "day", ][chull(data.scores[data.scores$act == "day", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
act.b <- data.scores[data.scores$act == "both", ][chull(data.scores[data.scores$act == "both", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
act.c <- data.scores[data.scores$act == "night", ][chull(data.scores[data.scores$act == "night", c("NMDS1", "NMDS2")]), ]  # hull values for grp A

size.b <- data.scores[data.scores$size == "b", ][chull(data.scores[data.scores$size == "b", c("NMDS1", "NMDS2")]), ]
size.c <- data.scores[data.scores$size == "c", ][chull(data.scores[data.scores$size == "c", c("NMDS1", "NMDS2")]), ]
size.d <- data.scores[data.scores$size == "d", ][chull(data.scores[data.scores$size == "d", c("NMDS1", "NMDS2")]), ]
size.e <- data.scores[data.scores$size == "e", ][chull(data.scores[data.scores$size == "e", c("NMDS1", "NMDS2")]), ]
size.f <- data.scores[data.scores$size == "f", ][chull(data.scores[data.scores$size == "f", c("NMDS1", "NMDS2")]), ]

diet.h <- data.scores[data.scores$diet == "herb", ][chull(data.scores[data.scores$diet == "herb", c("NMDS1", "NMDS2")]), ]
diet.c <- data.scores[data.scores$diet == "inver", ][chull(data.scores[data.scores$diet == "inver", c("NMDS1", "NMDS2")]), ]
diet.d <- data.scores[data.scores$diet == "det", ][chull(data.scores[data.scores$diet == "det", c("NMDS1", "NMDS2")]), ]
diet.p <- data.scores[data.scores$diet == "nekton", ][chull(data.scores[data.scores$diet == "nekton", c("NMDS1", "NMDS2")]), ]
diet.z <- data.scores[data.scores$diet == "plank", ][chull(data.scores[data.scores$diet == "plank", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(school.a, school.b, school.c, school.d)  #combine grp.a and grp.b
hull.data <- rbind(mob.a, mob.b, mob.c)
hull.data <- rbind(lev.a, lev.b, lev.c)
hull.data <- rbind(act.a, act.b)
hull.data <- rbind(size.b, size.c, size.d, size.e, size.f)
hull.data <- rbind(diet.h, diet.c, diet.d, diet.p, diet.z)
hull.data

ggplot() + 
  geom_polygon(data = hull.data, aes(x = NMDS1, y = NMDS2, fill = diet, group = diet),
               alpha = 0.30) + # add the convex hulls
  geom_text(data = species.scores,aes(x = NMDS1, y = NMDS2, label = species),
            alpha = 0.5) +  # add the species labels
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, shape = diet,
                                     colour = diet), size = 4) + # add the point markers
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size = 18), # remove x-axis labels
        axis.title.y = element_text(size = 18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


