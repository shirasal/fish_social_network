# NMDS plot to define guilds

# load required packages:
library(vegan)
library(tidyverse)

# upload the raw mediterranean data and create a species matrix
all_traits <- read_csv("species_traits.csv")

traits <- all_traits %>% 
  group_by(species) %>%
  summarise(max_size = max(size_1, size_2), mean_troph = mean(troph)) %>% 
  as.data.frame()

## NMDS - non metric multidimentional scaling
# http://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
# (1) Log-transform the data <<<<< IS THIS NECESSARY??
# log_trans_data <- decostand(med_mat[4:ncol(med_mat)], method = "log") # only the species data
rownames(traits) <- traits$species
traits$species <- NULL
View(traits)

######################## I'm having trouble here as I have NAs... Not sure what to do ##############
# log transformation for species data
nmds_data <- decostand(traits, method = "log")
head(nmds_data)
summary(nmds_data)

# Ordination
ord <- metaMDS(nmds_data, trace = FALSE) # Default dissimilarity index and standartisation

# Examine the stress plot: how 'good' is the NMDS?
stressplot(ord)

# plot NMDS:
plot(ord, type = "t")

# Tutorial by Christopher Chizinski https://chrischizinski.github.io/rstats/vegan-ggplot2/ #
# Using the scores function from vegan to extract the site scores and convert to a data.frame:
species.scores <- as.data.frame(scores(ord))
species.scores$site <- rownames(species.scores)
head(species.scores)




orditorp(ord,display="sites",cex=0.75,air=0.01)#add sites
orditorp(ord,display="species",col="red",air=0.01)#add sp
#we can add some info of the treatment:
treat <- data$Location#the site of sampling
ordiplot(ord,type="n")
ordihull(ord,groups=treat,draw="polygon",col="grey90",label=F)
col_site <- as.numeric(treat)#set color for each type of site
orditorp(ord,display="sites",col=col_site,air=0.01,cex=0.75)#add sites name

#now lets see if the enviromental factors had an effect:
output_env <- envfit(ord, data[,c(1:5)], permu = 999,na.rm = T) #data[,c(1,3)] is the site location and size
output_env




##Show example, but students do not need to do with their data

varpart(log_trans_data, ~ data[,1] + data[,2] + data[,3]+data[,4])