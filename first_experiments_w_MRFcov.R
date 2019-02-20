# sub = subsample

library(MRFcov)
# Citation: Clark, NJ, Wells, K and Lindberg, O. 2018. Unravelling changing interspecific interactions across environmental gradients using Markov random fields. Ecology doi: 10.1002/ecy.2221
library("testthat")
library("tidyverse")
medata <- read.csv("all_uvc_data_Jan2019.csv")
medata$X <- NULL

medata_fish <- medata %>% 
  group_by(data.origin, trans, site, species) %>% # to summarise the amount of same species in each transect
  summarise(sum = n()) %>%  # count individuals from each group (defined by above variables) to create a df with species abundance for each transect
  ungroup()

sub_medata_fish <- sample_n(tbl = medata_fish, size = 30) %>%  # sample 30 observations randomly
  mutate(ID = paste(site, trans, sep = ".")) %>% 
  select(ID, species, sum) %>% 
  spread(key = species, value = sum)
sub_medata_fish[is.na(sub_medata_fish)] <- 0 # change NAs to 0s
sub_fish_pa <- sub_medata_fish
rownames(sub_fish_pa) <- sub_fish_pa$ID
sub_fish_pa <- sub_fish_pa[2:ncol(sub_fish_pa)]
sub_fish_pa[sub_fish_pa > 0] <- 1 # turn to presence-absence

# so now sub_fish_pa is a presence absence 

