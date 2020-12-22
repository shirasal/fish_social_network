library(MRFcov)

# A standard CRF
n_nodes = 4
dat <- Bird.parasites
crf <- MRFcov(dat, n_nodes = n_nodes, family = 'binomial')
crf$graph
crf$direct_coefs

# To fit without interactions, the simplest way given the package constraints is to set these
# columns to zero so they will be regularised out of the model
# Cross-multiply the data with covariates manually
prepped_dat <- prep_MRF_covariates(dat, n_nodes = n_nodes)

# Find the covariate * species interaction columns in the prepped data and set to zero
cov_names_remove <- paste(colnames(dat[,-c(1:n_nodes)]), '_', colnames(dat[,c(1:n_nodes)]), sep = '')
prepped_dat[,cov_names_remove] <- 0

# Re-fit the model, setting prep_covariates to FALSE
no_interactions_crf <- MRFcov(prepped_dat, n_nodes = 4, prep_covariates = F, family = 'binomial')
no_interactions_crf$graph
no_interactions_crf$direct_coefs
