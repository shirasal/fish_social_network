source("R/run_models.R")

# Based on:
vignette("Gaussian_Poisson_CRFs")
# https://cran.r-project.org/web/packages/MRFcov/vignettes/Gaussian_Poisson_CRFs.html

apply(grps_mat[, c(1:5)], 2, max)
apply(dip_mat[, c(1:5)], 2, range)
apply(herb_mat[, c(1:4)], 2, range)

# symmetrising to the mean coefficient (when there's more than one)
sp.1.vs.sp.2 <- coef(glm(Epinephelus.costae ~ Epinephelus.marginatus, family = 'poisson', data = grps_mat))[2]
sp.2.vs.sp.1 <- coef(glm(Epinephelus.marginatus ~ Epinephelus.costae, family = 'poisson', data = grps_mat))[2]
mean.coef <- mean(sp.1.vs.sp.2, sp.2.vs.sp.1)

# Exponentiate, due to the log link
mean.coef <- exp(mean.coef)

# Make plots of predicted vs observed
# First, predict sp.1 (the common species) abundances
plot(grps_mat$Epinephelus.costae, grps_mat$Epinephelus.marginatus * mean.coef, 
     xlab = 'Observed',
     ylab = 'Predicted')

# Second (more rare) species
plot(grps_mat$Epinephelus.marginatus, grps_mat$Epinephelus.costae * mean.coef, 
     xlab = 'Observed',
     ylab = 'Predicted')

poiss.crf <- MRFcov(data = grps_mat, n_nodes = 5, family = 'poisson')

poiss.preds <- predict_MRF(data = grps_mat, MRF_mod = poiss.crf, n_cores = 1)
plot(grps_mat$Epinephelus.costae, poiss.preds[,1], 
     xlab = 'Observed',
     ylab = 'Predicted')

plot(grps_mat$Epinephelus.marginatus, poiss.preds[,2], 
     xlab = 'Observed',
     ylab = 'Predicted')

