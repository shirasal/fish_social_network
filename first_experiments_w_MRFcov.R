
library(MRFcov)
# Citation: Clark, NJ, Wells, K and Lindberg, O. 2018. Unravelling changing interspecific interactions across environmental gradients using Markov random fields. Ecology doi: 10.1002/ecy.2221
library("tidyverse")

##           Data Wrangling            ##
med_species <- medata %>%
  group_by(data.origin, site, trans, season, enforcement, species) %>% 
  summarise(sp.n = sum(sp.n)) %>% # group_by() and summarise() to avoid duplicates. I get abundance data
  spread(key = species, value = sp.n, fill = 0)

# set up the position of first and last species:
first_spp <- 5
last_spp <- 96

spp_data <- medata %>% 
  mutate(ID = paste(site, trans, sep = "_")) %>% 
  group_by(ID, data.origin, site, trans, species) %>% 
  summarise(n = max(as.numeric(sp.n > 0))) %>%
  spread(key = species, value = n, fill = 0) %>% 
  as.data.frame()

# creating matrix for MRFcov  
rownames(spp_data) <- spp_data$ID
spp_only <- spp_data %>% 
  select(first_spp:last_spp) %>% 
  as.matrix()

# Take a wee look at the data to make sure it's good for MRFcov
class(spp_only)
class(spp_only[1:ncol(spp_only)])
table(summary(spp_only == 1 | spp_only == 0))
# Looks like it's good to go. Now my only trouble is the rownames (samples/sites), which I'll have to sort out later
# Just a reminder: This matrix does not include spatial or environmental information.

MRF <- MRFcov(spp_only, family = "binomial")
# ... but for some reason it's not working and telling me there are non-binary variables. I couldn't find them though.
#MRF_mat <- MRF$graph
#heatmap(x = MRF_mat, symm = TRUE, col = terrain.colors(256))

# prepare abundance data for MRFcov:
med_abund <- med_species %>% 
  mutate(ID = paste(site, trans, sep = ".")) %>% 
  group_by(data.origin, site, trans, season, enforcement) %>% 
  summarise(max(.)) %>% 
  select(6:ncol(.))

rownames(med_abund) <- med_abund$ID
