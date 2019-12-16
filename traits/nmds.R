library(vegan)
library(MASS)
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

nmds_traits <- metaMDS(comm = traits_data, distance = "gower")
nmds_traits <- isoMDS(d = traits_data)
is.list(traits_data)
is.na(traits_data)

myFun <- function(data) {
  temp1 <- sapply(data, is.list)
  temp2 <- do.call(
    cbind, lapply(data[temp1], function(x) 
      data.frame(do.call(rbind, x), check.names = FALSE)))
  cbind(data[!temp1], temp2)
}

myFun(traits_data)
