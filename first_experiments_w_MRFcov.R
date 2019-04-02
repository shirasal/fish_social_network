library(MRFcov)
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
# But it's not working, telling me there are non-binary data (which I cannot find)

# Creating a subsample to test the function
spp_only_df <- as.data.frame(spp_only)
spp_data_sample <- as.matrix(sample_n(spp_only_df, size = 100))

# Creating a fake matrix to test the function

sites <- read.csv("sites.csv", row.names = NULL)
site_list <- sites %>% 
  unique(.)

spp <- read.csv("species.csv",row.names = NULL)

spp_list <- spp %>% 
  unique(.)

mat.names<-list(site_list,spp_list)
mat.names<-bind_rows(site_list,spp_list)
fake_spp_mat <- matrix(rbinom(7905, 1, 0.5), nrow = nrow(spp_list), ncol = nrow(site_list))

rownames(fake_spp_mat)<-site_list$site
colnames(fake_spp_mat)<-spp_list$Species

#fake_spp_mat <- rbinom(100, 1, 0.5)

# Just a reminder: This matrix does not include spatial or environmental information.
MRF <- MRFcov(fake_spp_only, family = "binomial")
#MRF <- MRFcov(spp_only, family = "binomial")
# ... but for some reason it's not working and telling me there are non-binary variables. I couldn't find them though.

# prepare abundance data for MRFcov:
med_abund <- med_species %>% 
  mutate(ID = paste(site, trans, sep = ".")) %>% 
  group_by(data.origin, site, trans, season, enforcement) %>% 
  summarise(max(.)) %>% 
  select(6:ncol(.))

rownames(med_abund) <- med_abund$ID

####### NEW DATA FRAME WITH TOP 25% of SPECIES BY OCCURENCE ########
medata_new <- read.csv("sites_spp_matrix.csv")
medata_new[is.na(medata_new)] <- 0
View(medata_new) # YAAAAAAS! Abundance matrix!!!
