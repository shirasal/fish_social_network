# Check how many double zeros:
new_grps_temp_dep %>% as.data.frame() %>% 
  filter(Epinephelus.costae == 0 & Epinephelus.marginatus == 0 & Mycteroperca.rubra == 0 &
           Serranus.cabrilla == 0 & Serranus.scriba == 0) %>% count()
