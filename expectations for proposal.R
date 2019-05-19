library(MRFcov)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(plotly)
library(datapasta)

## Plot sites to see where is the gradient suppose to be ##
sites_data <- read.csv("sites_names_points.csv", header = TRUE) %>% 
  distinct(latloncheck, .keep_all = TRUE)
sites_data$id <- NULL
sites_data$latloncheck <- NULL

plot_sites <- sites_data %>% 
  ggplot(aes(lon, lat)) + geom_point(aes(color = origin))
ggplotly(plot_sites)

ex_spp_mat <- read.csv("sample_site_spp_matrix.csv", blank.lines.skip = FALSE) 
row.names(ex_spp_mat) <- ex_spp_mat$sites
ex_spp_mat$sites <- NULL
ex_spp_mat$max_temp <- NULL

