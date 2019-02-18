
library(MRFcov)
# Citation: Clark, NJ, Wells, K and Lindberg, O. 2018. Unravelling changing interspecific interactions across environmental gradients using Markov random fields. Ecology doi: 10.1002/ecy.2221
library("testthat")
library("tidyverse")
medata <- read.csv("all_uvc_data_Jan2019.csv")
medata$X <- NULL

medata_fish <- medata %>% 
  group_by(data.origin, trans, site, species) %>% # to summarise the amount of same species in each transect
  summarise(sum = n()) # count individuals from each group (defined by above variables) - this is to get a dataframe with species abundance for each transect

