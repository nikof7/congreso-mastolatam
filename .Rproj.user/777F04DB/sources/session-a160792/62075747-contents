librarya(patchwork)

# Función para crear el gráfico
create_plot <- function(data, title, subtitle) {
  ggplot(data[[1]], aes(x = angle)) +
    geom_line(aes(y = density, color = category), size = 1) +
    scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), 
                       labels = c("00", "06", "12", "18", "24")) +
      labs(title = title, subtitle = subtitle,
         x = "Solar Time", y = "Density") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "top") +
    scale_color_manual(values=c("#CC6666", "#9999CC", "#FFB90F"))
    
}

# Lista para almacenar los gráficos combinados
plots <- list()

for (sp in 1:nrow(generalized_density_data)) {
  # Datos y títulos
  datasets <- list(
    list(generalized_density_data$density_tr_cfam_sys[[sp]], 
         "TR perros domésticos por sistema"),
    list(generalized_density_data$density_tr_btau_sys[[sp]], 
         "TR vacuno por sistema"),
    list(generalized_density_data$density_n_tech_sys[[sp]], 
         "Techos por sistema")
  )
  
  # Generar los gráficos individuales
  individual_plots <- lapply(datasets, function(data) {
    n_registros <- data[[1]][[2]] %>% 
      group_by(cat) %>% 
      summarise(n = n()) %>% 
      mutate(text = glue("{cat}={n}")) %>%
      pull(text) %>%
      paste(collapse = ", ") %>% 
      paste("n: ", .)
    
    create_plot(data[[1]], data[[2]], n_registros)
  })
  
  # Combinar los gráficos en un solo gráfico con 3 columnas
  combined_plot <- wrap_plots(individual_plots, ncol = 3) +
    plot_annotation(title = generalized_density_data[[1]][sp],
                    theme = theme(plot.title = element_text(size = 16, face = "bold"))) + plot_layout(axes = "collect")
  
  # Almacenar el gráfico combinado en la lista
  plots[[length(plots) + 1]] <- combined_plot
}
combined_plot


pdf("combined_plots.pdf", width = 20, height = 8)
for (i in seq_along(plots)) {
  print(plots[[i]])
}
dev.off()
