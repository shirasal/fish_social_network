# source("R/packages.R")
# load("data/all_objects.RData")
# source("R/run_models_spatial.R")


# ATTEMPT to write a function ---------------------------------------------

prediction_data <- function(covariate, spp_mat, species_group){
  # if (covariate != c("temp" | "mpa")) stop("Select 'temp' or 'mpa' as covariate")
  if (covariate == "temp")
    abs_temp_mat <- spp_mat %>%
      mutate(depth = median(depth),
             prod = median(prod),
             mpa = TRUE,
             across(all_of(species_group), function(x) 0)) %>%
      group_by(temp = round(temp, digits = 1)) %>%
      sample_n(1)

  max_temp_mat <- spp_mat %>%
    mutate(depth = median(depth),
           prod = median(prod),
           mpa = TRUE,
           across(all_of(species_group), .fns = max)) %>%
    group_by(temp = round(temp, digits = 1)) %>%
    sample_n(1)

  pred_mat <- bind_rows(abs_temp_mat, max_temp_mat)

  if (covariate == "mpa"){
    abs_mpa_mat <- spp_mat %>%
      mutate(temp = median(temp),
             depth = median(depth),
             prod = median(prod),
             across(all_of(species_group), function(x) 0)) %>%
      group_by(mpa) %>%
      sample_n(1)

    if (covariate == "mpa")
      max_mpa_mat <- spp_mat %>%
        mutate(temp = median(temp),
               depth = median(depth),
               prod = median(prod),
               across(all_of(species_group), .fns = max)) %>%
        group_by(mpa) %>%
        sample_n(1)

    pred_mat <- bind_rows(abs_mpa_mat, max_mpa_mat)

    return(pred_mat)

  }
}

grps_temp_pred_mat <- prediction_data(covariate = "temp", spp_mat = grps_mat, species_group = groupers)


