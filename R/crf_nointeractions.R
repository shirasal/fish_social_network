source("R/packages.R")
source("R/functions.R")
load("data/data_and_objects.RData")

n_nodes = 4

# Groupers ----------------------------------------------------------------

grps_dat <- grps_mat

# To fit without interactions, the simplest way given the package constraints is to set these
# columns to zero so they will be regularised out of the model
# Cross-multiply the data with covariates manually
grps_prepped_dat <- prep_MRF_covariates(grps_dat, n_nodes = n_nodes)

# Find the covariate * species interaction columns in the prepped data and set to zero
# cov_names_remove <- paste(colnames(dat[,-c(1:n_nodes)]), '_', colnames(dat[,c(1:n_nodes)]), sep = '')
# prepped_dat[,cov_names_remove] <- 0
grps_prepped_dat <- grps_prepped_dat %>% 
  mutate(across(.cols = 9:ncol(grps_prepped_dat), .fns = function(x) 0))

# Re-fit the model, setting prep_covariates to FALSE
grps_no_int <- MRFcov(grps_prepped_dat, n_nodes = 4, prep_covariates = F, family = 'poisson')
grps_no_int$graph
grps_no_int$direct_coefs

grps_no_int$key_coefs

grps_noint_relimp <- rel_imp_sum(grps_no_int)

# Seabreams ---------------------------------------------------------------

dip_dat <- dip_mat

# To fit without interactions, the simplest way given the package constraints is to set these
# columns to zero so they will be regularised out of the model
# Cross-multiply the data with covariates manually
dip_prepped_dat <- prep_MRF_covariates(dip_dat, n_nodes = n_nodes)

# Find the covariate * species interaction columns in the prepped data and set to zero
# cov_names_remove <- paste(colnames(dat[,-c(1:n_nodes)]), '_', colnames(dat[,c(1:n_nodes)]), sep = '')
# prepped_dat[,cov_names_remove] <- 0
dip_prepped_dat <- dip_prepped_dat %>% 
  mutate(across(.cols = 9:ncol(dip_prepped_dat), .fns = function(x) 0))

# Re-fit the model, setting prep_covariates to FALSE
dip_no_int <- MRFcov(dip_prepped_dat, n_nodes = 4, prep_covariates = F, family = 'poisson')
dip_no_int$graph
dip_no_int$direct_coefs

dip_no_int$key_coefs

dip_noint_relimp <- rel_imp_sum(dip_no_int)

# Herbivores --------------------------------------------------------------

herb_dat <- herb_mat

# To fit without interactions, the simplest way given the package constraints is to set these
# columns to zero so they will be regularised out of the model
# Cross-multiply the data with covariates manually
herb_prepped_dat <- prep_MRF_covariates(herb_dat, n_nodes = n_nodes)

# Find the covariate * species interaction columns in the prepped data and set to zero
# cov_names_remove <- paste(colnames(dat[,-c(1:n_nodes)]), '_', colnames(dat[,c(1:n_nodes)]), sep = '')
# prepped_dat[,cov_names_remove] <- 0
herb_prepped_dat <- herb_prepped_dat %>% 
  mutate(across(.cols = 9:ncol(herb_prepped_dat), .fns = function(x) 0))

# Re-fit the model, setting prep_covariates to FALSE
herb_no_int <- MRFcov(herb_prepped_dat, n_nodes = 4, prep_covariates = F, family = 'poisson')
herb_no_int$graph
herb_no_int$direct_coefs

herb_no_int$key_coefs

herb_noint_relimp <- rel_imp_sum(herb_no_int)

p_relimp_grps_noint <- plot_relimp(grps_noint_relimp, guild_colours$grps, "Groupers")
p_relimp_dip_noint <- plot_relimp(dip_noint_relimp, guild_colours$dip, "Seabreams")
p_relimp_herb_noint <- plot_relimp(herb_noint_relimp, guild_colours$herb, "Herbivores")
p_relimp_noint <- egg::ggarrange(p_relimp_grps_noint, p_relimp_dip_noint, p_relimp_herb_noint)
# ggsave(p_relimp_noint, filename = "rel_imp_noint.png", device = "png", path = "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")
# ggsave(p_relimp_noint, filename = "rel_imp_noint.pdf", device = "pdf", path = "figures/rel_imp/", dpi = 150, height = 10, width = 10, units = "in")

