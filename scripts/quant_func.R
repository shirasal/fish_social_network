
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


# Get the connectance value
connectance <- function(x, group){
  edgelist <- x %>%
    as.data.frame() %>% 
    rownames_to_column() %>%
    pivot_longer(cols = c(contains("."), contains("."))) %>%
    rename("spp_1" = rowname, "spp_2" = name, "edge_value" = value) %>% 
    filter(spp_1 != spp_2)
  
  L <- edgelist %>%
    select(edge_value) %>% 
    sum()
  M <- (length(group))^2
  connect <- (L/2)/M # L devided to 2 because it is calculated with the whole data, including diag
  print(connect)
  
}
