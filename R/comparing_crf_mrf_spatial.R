
# Groupers ----------------------------------------------------------------

comparison_spatial <- MRFcov::cv_MRF_diag_rep_spatial(data = grps_mat, 
                                                      coords = grps_coords, 
                                                      compare_null = TRUE, 
                                                      family = "poisson", 
                                                      n_nodes = 4, 
                                                      plot = FALSE)

Spatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], 0.5)
Nonspatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], 0.5)

if (which.min(c(Spatial.mse, Nonspatial.mse)) == 1) {
  best.mod <- "Spatial"
} else {
  best.mod <- "Nonspatial"
}
str_glue("Best model for groupers: {best.mod}")

# Explore fit metrics in more detail using 95% quantiles
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], c(0.025, 0.5, 0.975))
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], c(0.025, 0.5, 0.975))

# Plot it
comparison_spatial %>% 
  ggplot() + aes(x = model, y = MSE) + geom_boxplot() + 
  xlab("Model") + ggtitle("Model comparison for grouper guild")

# Check significance
spat.mse <- comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")]
nonspat.mse <- comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")]
t.test(spat.mse, nonspat.mse, paired = TRUE, alternative = "greater")
# Not sure this is valid..

# CRF-MRF
comparison_crf_mrf <- MRFcov::cv_MRF_diag_rep(data = grps_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 4, plot = FALSE)
MRFcov::cv_MRF_diag_rep(data = grps_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 4, plot = TRUE)

MRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], 0.5)
CRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.min(c(MRF.mse, CRF.mse)) == 1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod

# Diplodus ----------------------------------------------------------------

comparison_spatial <- MRFcov::cv_MRF_diag_rep_spatial(data = dip_mat, coords = dip_coords, compare_null = TRUE, family = "poisson", n_nodes = 4, plot = FALSE)

Spatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], 0.5)
Nonspatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], 0.5)

if (which.min(c(Spatial.mse, Nonspatial.mse)) == 1) {
  best.mod <- "Spatial"
} else {
  best.mod <- "Nonspatial"
}
str_glue("Best model for seabreams: {best.mod}")

# Explore fit metrics in more detail using 95% quantiles
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], c(0.025, 0.5, 0.975))
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], c(0.025, 0.5, 0.975))

# Plot
comparison_spatial %>% 
  ggplot() + aes(x = model, y = MSE) + geom_boxplot() + 
  xlab("Model") + ggtitle("Model comparison for seabream guild")

# Check significance
spat.mse <- comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")]
nonspat.mse <- comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")]
t.test(spat.mse, nonspat.mse, paired = TRUE, alternative = "greater")

# Not sure this is valid..

# CRF-MRF
comparison_crf_mrf <- MRFcov::cv_MRF_diag_rep(data = dip_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 4, plot = FALSE)

MRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], 0.5)
CRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.min(c(MRF.mse, CRF.mse)) == 1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod


# Herbivores --------------------------------------------------------------

comparison_spatial <- MRFcov::cv_MRF_diag_rep_spatial(data = herb_mat, coords = herb_coords, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

Spatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], 0.5)
Nonspatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], 0.5)

if (which.min(c(Spatial.mse, Nonspatial.mse)) == 1) {
  best.mod <- "Spatial"
} else {
  best.mod <- "Nonspatial"
}
best.mod

comparison_crf_mrf <- MRFcov::cv_MRF_diag_rep(data = herb_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

MRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], 0.5)
CRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.min(c(MRF.mse, CRF.mse)) == 1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod



