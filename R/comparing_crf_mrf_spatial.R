source("R/run_models_spatial.R")


# Groupers ----------------------------------------------------------------

comparison_spatial <- MRFcov::cv_MRF_diag_rep_spatial(data = grps_mat, coords = grps_coords, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

Spatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], 0.5)
Nonspatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], 0.5)

if (which.min(c(Spatial.mse, Nonspatial.mse)) == 1) {
  best.mod <- "Spatial"
} else {
  best.mod <- "Nonspatial"
}
best.mod

# Explore fit metrics in more detail using 95% quantiles
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], c(0.025, 0.5, 0.975))
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], c(0.025, 0.5, 0.975))

comparison_crf_mrf <- MRFcov::cv_MRF_diag_rep(data = grps_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

MRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], 0.5)
CRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.min(c(MRF.mse, CRF.mse)) == 1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod

quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], c(0.025, 0.5, 0.975))
quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], c(0.025, 0.5, 0.975))

# Diplodus ----------------------------------------------------------------

comparison_spatial <- MRFcov::cv_MRF_diag_rep_spatial(data = dip_mat, coords = dip_coords, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

Spatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], 0.5)
Nonspatial.mse <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], 0.5)

if (which.min(c(Spatial.mse, Nonspatial.mse)) == 1) {
  best.mod <- "Spatial"
} else {
  best.mod <- "Nonspatial"
}
best.mod

# Explore fit metrics in more detail using 95% quantiles
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], c(0.025, 0.5, 0.975))
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], c(0.025, 0.5, 0.975))

comparison_crf_mrf <- MRFcov::cv_MRF_diag_rep(data = dip_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

MRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], 0.5)
CRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.min(c(MRF.mse, CRF.mse)) == 1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod

quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], c(0.025, 0.5, 0.975))
quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], c(0.025, 0.5, 0.975))


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

# Explore fit metrics in more detail using 95% quantiles
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], c(0.025, 0.5, 0.975))
quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], c(0.025, 0.5, 0.975))

comparison_crf_mrf <- MRFcov::cv_MRF_diag_rep(data = herb_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

MRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], 0.5)
CRF.mse <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.min(c(MRF.mse, CRF.mse)) == 1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod

quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF (no covariates)")], c(0.025, 0.5, 0.975))
quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], c(0.025, 0.5, 0.975))


# All turn out to be better models with CRF than MRF and spatial rather than nonspatial

