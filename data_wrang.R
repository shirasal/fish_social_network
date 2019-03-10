library(tidyverse)
library(plotly)

medata <- read.csv("all_uvc_data_07.03.19_2.csv")
medata$X.1 <- NULL
medata$X <- NULL

locations_all <- medata[, 4:6]
locations <- locations_all[!duplicated(locations_all),] %>% 
  na.omit() %>% 
  arrange()
write.csv(x = locations, file = "locations.csv")

p_loc <- locations %>% 
  ggplot(aes(x = lon, y = lat)) + geom_point()

ggplotly(p_loc)

