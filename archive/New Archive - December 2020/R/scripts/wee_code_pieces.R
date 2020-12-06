# Check how many double zeros:
new_grps_temp_dep %>% as.data.frame() %>% 
  filter(Epinephelus.costae == 0 & Epinephelus.marginatus == 0 & Mycteroperca.rubra == 0 &
           Serranus.cabrilla == 0 & Serranus.scriba == 0) %>% count()

# Fix for listed columns in a dataframe
as.data.frame(lapply(med_raw,unlist))

# Check if any infinite values in object:
grps_mat %>% 
  select_if(.predicate = is.numeric) %>% 
  as.matrix() %>% 
  is.finite() %>%
  any()