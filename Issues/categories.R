str(spp_mat)
categorised_data <- spp_mat
str(categories)
categorised_data <- categories

if_else(condition  = "category" %in% colnames(categorised_data), 
        true = categorised_data %>%
          arrange(category) %>%
          group_by(category) %>%
          nest(),
        false = categorised_data %>%
          mutate(category = as.factor(.[, ncol(.)])) %>%
          arrange(category) %>%
          group_by(category) %>%
          nest())
