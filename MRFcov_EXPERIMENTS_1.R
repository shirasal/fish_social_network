library(MRFcov)

vignette("CRF_data_prep")

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
# rand_mat <- matrix(data = rbinom((85*13), 1, 0.5), nrow = 85, ncol = 13, byrow = TRUE )
# write.csv(rand_mat, "binary_matrix.csv")
# 
# rm(spp, spp_unq, sites, sites_unq, rand_mat)

fish_samp_mat <- read.csv("spp_sites.csv")
# View(med_samp_mat)
rownames(fish_samp_mat) <- fish_samp_mat$X
fish_samp_mat$X <- NULL
mrf_fish <- MRFcov(fish_samp_mat, family = "binomial")
plotMRF_hm(mrf_fish, plot_observed_vals = TRUE, data = fish_samp_mat)