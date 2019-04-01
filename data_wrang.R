# library(maps)
library(tidyverse)
# library(plotly)
library(revgeo)

# medata <- read.csv("all_uvc_data_07.03.19_2.csv")
# medata_full$X.1 <- NULL
# medata_full$X <- NULL

locations <- read.csv("locations.csv", blank.lines.skip = TRUE, skipNul = TRUE) # upload unique locations by lat and lon
View(locations)

# locations$Country <- map.where(database = "world", locations$lon, locations$lat) # add a country by lat and lon using 'maps' package
# locations <- locations %>%
#   mutate(point = paste0(lat ,", ", lon))
# write.csv(locations, "locations.csv")

x <- locations$lat
y <- locations$lon

countries <- revgeo(y, x, output = "frame")

locations_full <- full_join(locations, countries)

