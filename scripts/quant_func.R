
# Function to calculate the sum of effect (relative importance) of species in the model
rel_imp <- function(i){
  i %>% 
    filter(str_detect(string = .$Variable,
                      pattern = "tmean", negate = TRUE)) %>% 
    .$Rel_importance %>% 
    sum()
}