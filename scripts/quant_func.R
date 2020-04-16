
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



## Sum separately negative coefficients and positive coefficients
# coef_dir <- function(j){
#   j %>% 
#     mutate(pos = if_else(.$Mean_coef > 0, sum(), 0),
#            neg = if_else(.$Mean_coef < 0, sum(), 0)) %>% 
#   print()
# }
## Condition here is problematic 
## Test above on this:
# model_temp_grps$boot$mean_key_coefs$Epinephelus.costae %>%
#   as.data.frame() %>% 
#   ifelse(Mean_coef > 0,
#           mutate(pos = sum(), mutate(neg = sum())))


