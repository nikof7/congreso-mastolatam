a <- generalized_density_data$density_tr_cfam_sys[[sp]][[1]] 

a %>% summarise(n = n()) %>% 
  mutate(text = glue("{cat}={n}")) %>%
  pull(text) %>%
  paste(collapse = ", ") %>% 
  paste("n: ", .)
