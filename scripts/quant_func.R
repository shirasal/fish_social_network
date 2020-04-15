
# Function to calculate the sum of effect (relative importance) of species in the model
rel_imp <- function(i, cov){
  i %>% 
    filter(str_detect(string = .$Variable,
                      pattern = cov, negate = TRUE)) %>% 
    .$Rel_importance %>% 
    sum()
}

# Calculate the sum of mean_coefficient, see the general direction
mean_coef <- function(j){
  j %>% 
    .$Mean_coef %>% 
    sum()
}

# Sum separately negative coefficients and positive coefficients
coef_dir <- function(j){
  i %>% 
    if_else(.$Mean_coef > 0, mutate(pos = sum(), mutate(neg = sun())))
}

# model_temp_grps$boot$mean_key_coefs$Epinephelus.costae %>%
#   as.data.frame() %>% 
#   ifelse(Mean_coef > 0,
#           mutate(pos = sum(), mutate(neg = sum())))


