source("R/run_models_spatial.R")


# Groupers ----------------------------------------------------------------

comparison_spatial <- MRFcov::cv_MRF_diag_rep_spatial(data = grps_mat, coords = grps_coords, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

Spatial.true.prop <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], 0.5)
Nonspatial.true.prop <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], 0.5)

if (which.max(c(Spatial.true.prop, Nonspatial.true.prop)) ==
    1) {
  best.mod <- "Spatial"
} else {
  best.mod <- "Nonspatial"
}
best.mod

comparison_crf_mrf <- MRFcov::cv_MRF_diag(data = grps_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

MRF.true.prop <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF")], 0.5)
CRF.true.prop <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.max(c(MRF.true.prop, CRF.true.prop)) ==
    1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod



# Diplodus ----------------------------------------------------------------

comparison_spatial <- MRFcov::cv_MRF_diag_rep_spatial(data = dip_mat, coords = dip_coords, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

Spatial.true.prop <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], 0.5)
Nonspatial.true.prop <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], 0.5)

if (which.max(c(Spatial.true.prop, Nonspatial.true.prop)) ==
    1) {
  best.mod <- "Spatial"
} else {
  best.mod <- "Nonspatial"
}
best.mod

comparison_crf_mrf <- MRFcov::cv_MRF_diag(data = dip_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

MRF.true.prop <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF")], 0.5)
CRF.true.prop <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.max(c(MRF.true.prop, CRF.true.prop)) ==
    1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod

# Herbivores --------------------------------------------------------------

comparison_spatial <- MRFcov::cv_MRF_diag_rep_spatial(data = herb_mat, coords = dip_coords, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

Spatial.true.prop <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Spatial MRF")], 0.5)
Nonspatial.true.prop <- quantile(comparison_spatial$MSE[which(comparison_spatial$model == "Non-spatial MRF")], 0.5)

if (which.max(c(Spatial.true.prop, Nonspatial.true.prop)) ==
    1) {
  best.mod <- "Spatial"
} else {
  best.mod <- "Nonspatial"
}
best.mod

comparison_crf_mrf <- MRFcov::cv_MRF_diag(data = herb_mat, n_covariates = 4, compare_null = TRUE, family = "poisson", n_nodes = 5, plot = FALSE)

MRF.true.prop <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "MRF")], 0.5)
CRF.true.prop <- quantile(comparison_crf_mrf$MSE[which(comparison_crf_mrf$model == "CRF")], 0.5)

if (which.max(c(MRF.true.prop, CRF.true.prop)) ==
    1) {
  best.mod <- "MRF"
} else {
  best.mod <- "CRF"
}
best.mod

# All turn out to be better models with CRF than MRF and nonspatial rather than spatial

