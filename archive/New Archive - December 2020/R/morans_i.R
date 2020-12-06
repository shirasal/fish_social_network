# Restart R if you were working in it before
gc()
library(ape)
networks_df <- readRDS("data/med_df.Rds")
gc()
memory.limit()

# maybe use 'geosphere' package for correct projection? Not sure it's very important here.

# ref: https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/

## Generate a distance matrix:
med_dists <- as.matrix(dist(cbind(networks_df$lon, networks_df$lat), diag = FALSE, upper = FALSE))

## Take inverse of the matrix values and replace the diagonal entries with zero:
med_dists_inv <- 1/med_dists
diag(med_dists_inv) <- 0

med_dists_inv[1:5, 1:5]

Moran.I(networks_df$tmean, med_dists_inv)

detach("package:ape", unload = TRUE)
