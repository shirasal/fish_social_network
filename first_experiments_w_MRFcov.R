
library(MRFcov)
# Citation: Clark, NJ, Wells, K and Lindberg, O. 2018. Unravelling changing interspecific interactions across environmental gradients using Markov random fields. Ecology doi: 10.1002/ecy.2221
library("tidyverse")

##           Data Wrangling            ##
med_species <- medata %>%
  group_by(data.origin, site, trans, season, enforcement, species) %>% 
  summarise(sp.n = sum(sp.n)) %>% # group_by() and summarise() to avoid duplicates. I get abundance data
  spread(key = species, value = sp.n, fill = 0)

# set up the position of first and last species:
first_spp <- 7
last_spp <- 98

spp_data <- medata %>% 
  mutate(ID = paste(site, trans, season, sep = "_")) %>% 
  group_by(ID, data.origin, site, trans, season, enforcement, species) %>% 
  summarise(n = max(as.numeric(sp.n > 0))) %>%
  spread(key = species, value = n, fill = 0) %>% 
  as.data.frame()

# creating matrix for MRFcov
rownames(spp_data) <- spp_data$ID
spp_only <- spp_data %>% 
  select(first_spp:last_spp) %>% 
  as.matrix()

MRF <- MRFcov(example_data, family = "binomial")
MRF_mat <- MRF$graph
heatmap(x = MRF_mat, symm = TRUE, col = terrain.colors(256))


