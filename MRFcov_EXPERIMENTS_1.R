library(MRFcov)

#birds_binomial <- Bird.parasites[, 1:4]
mrf_birds <- MRFcov(birds_binomial, family = "binomial")
plotMRF_hm(mrf_birds)

# spp <- read.csv(file = "species.csv", header = TRUE, check.names = TRUE)
# spp_unq <- unique(spp)
# write.csv(spp_unq, "species_unique.csv")
# 
# sites <- read.csv(file = "sites.csv", header = TRUE, check.names = TRUE)
# sites_unq <- unique(sites)
# write.csv(sites_unq, "sites_unique.csv")
#
# rand_mat <- matrix(data = rbinom((86*7), 1, 0.5), nrow = 86, ncol = 7, byrow = TRUE)
# write.csv(rand_mat, "binary_matrix.csv")
# 
# rm(spp, spp_unq, sites, sites_unq, rand_mat)

