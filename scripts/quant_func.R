
# Function to calculate the sum of effect (relative importance) of species in the model
rel_imp <- function(i, cov){
  i %>% 
    filter(str_detect(string = .$Variable,
                      pattern = cov, negate = TRUE)) %>% 
    .$Rel_importance %>% 
    sum()
}

# # Calculate the sum of mean_coefficient, see the general direction
# mean_coef <- function(j){
#   j %>% 
#     .$Mean_coef %>% 
#     sum()
# }
# # Condition here is problematic



