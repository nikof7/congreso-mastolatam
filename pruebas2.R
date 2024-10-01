# Este codigo es para calcular los dhats para todas las especies y variables

load("datos_procesados_v4.RData")

# Categorizar los datos
data_categorized <- data %>% 
  mutate(
    cat_n_tech_sys = case_when(n_tech_sys == 0 ~ "0", n_tech_sys > 0 ~ "1"),
    cat_tr_cfam_sys = case_when(tr_cfam_sys == 0 ~ "0", tr_cfam_sys > 0 ~ "1"),
    cat_tr_btau_sys = case_when(tr_btau_sys == 0 ~ "0", tr_btau_sys > 0 ~ "1")
  ) %>% 
  select(site, system, camera, solar, sp, cat_n_tech_sys, cat_tr_cfam_sys, cat_tr_btau_sys)

# Función para calcular superposición
calculate_overlap <- function(data, category) {
  cat0 <- data$solar[data[[category]] == "0"]
  cat1 <- data$solar[data[[category]] == "1"]
  
  if (length(cat0) > 0 && length(cat1) > 0) {
    overlap <- overlapEst(cat0, cat1)
    return(overlap[c("Dhat1", "Dhat4", "Dhat5")])
  } else {
    return(c(Dhat1 = NA, Dhat4 = NA, Dhat5 = NA))
  }
}

# Calcular la superposición para cada especie y cada variable
results <- data_categorized %>%
  group_by(sp) %>%
  summarise(
    overlap_n_tech = list(calculate_overlap(cur_data(), "cat_n_tech_sys")),
    overlap_tr_cfam = list(calculate_overlap(cur_data(), "cat_tr_cfam_sys")),
    overlap_tr_btau = list(calculate_overlap(cur_data(), "cat_tr_btau_sys"))
  ) %>%
  unnest_wider(overlap_n_tech, names_sep = "_") %>%
  unnest_wider(overlap_tr_cfam, names_sep = "_") %>%
  unnest_wider(overlap_tr_btau, names_sep = "_")

# Ver los resultados
print(results)

clipr::write_clip(results)
