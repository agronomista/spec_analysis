#lectura de firmas espectrales borrador app
library(pacman)
p_load(tidyverse)

#selección de la ruta del archivo
ruta <- "C:/Users/janto/OneDrive/Escritorio/poaceae_ixophorus.csv"

#creación de una variable
test_open <- read.csv(ruta, header = FALSE, sep = ";")

#cambiar de comas a puntos
test_open[, 4] <- gsub(",", ".", test_open[, 4])
#pasar de chr a numerico
test_open[, 4] <- as.numeric(test_open[, 4])


#cambiar el prefijo de los ID´s
test_open[, 1] <- gsub("^IS-RSC.*", "IS_RESISTENTE", test_open[, 1])
test_open[, 1] <- gsub("^IS-SSC.*", "IS_SUSCEPTIBLE", test_open[, 1])

#cambiar a factor la columna ID 
test_open[, 1] <- as.factor(test_open[, 1])


spectro_analysis <- function(df) {
  # Tomar las primeras cuatro columnas y renombrarlas
  df_subset <- df[, 1:4]
  colnames(df_subset) <- c("id", "registro", "nm", "reflec")
  
  # Definir los rangos espectrales revisados
  AzulRW1 <- seq(451, 496, by = 1)
  VerdeRW1 <- seq(496, 571, by = 1)
  AmarilloRW1 <- seq(571, 590, by = 1)  # Añadido el rango Amarillo
  RojoRW1 <- seq(590, 700, by = 1)  # Ajustado para evitar superposición
  RED_EDGERW1 <- seq(700, 751, by = 1)  # Ajustado para iniciar después del Rojo
  NIRRW1 <- seq(751, 901, by = 1)
  
  # Crear una lista para almacenar los resultados
  segmented_data <- list()
  
  # Segmentar por Azul y añadir la columna 'banda'
  segmented_data$Azul <- df_subset[df_subset$nm %in% AzulRW1, ]
  segmented_data$Azul$banda <- "Azul"
  
  # Segmentar por Verde y añadir la columna 'banda'
  segmented_data$Verde <- df_subset[df_subset$nm %in% VerdeRW1, ]
  segmented_data$Verde$banda <- "Verde"
  
  # Segmentar por Amarillo y añadir la columna 'banda'
  segmented_data$Amarillo <- df_subset[df_subset$nm %in% AmarilloRW1, ]
  segmented_data$Amarillo$banda <- "Amarillo"
  
  # Segmentar por Rojo y añadir la columna 'banda'
  segmented_data$Rojo <- df_subset[df_subset$nm %in% RojoRW1, ]
  segmented_data$Rojo$banda <- "Rojo"
  
  # Segmentar por Red Edge y añadir la columna 'banda'
  segmented_data$RedEdge <- df_subset[df_subset$nm %in% RED_EDGERW1, ]
  segmented_data$RedEdge$banda <- "RedEdge"
  
  # Segmentar por NIR y añadir la columna 'banda'
  segmented_data$NIR <- df_subset[df_subset$nm %in% NIRRW1, ]
  segmented_data$NIR$banda <- "NIR"
  
  # Combinar todos los segmentos en un solo data frame
  output <- do.call(rbind, segmented_data)
  
  # Crear la columna 'rep' que cuenta las repeticiones por cada combinación de 'id', 'registro', 'banda', y 'nm'
  output$rep <- ave(output$registro, output$id, output$registro, output$banda, output$nm, FUN = seq_along)
  
  # Reordenar las columnas para que 'banda' aparezca en el lugar deseado
  output <- output[, c("id", "registro", "nm", "banda", "reflec", "rep")]
  
  # Retornar el data frame segmentado
  return(output)
}

# Aplicar la función al data frame test_open
a <- spectro_analysis(test_open)

# Ver el resultado
print(a)

#write_excel_csv(a, "output2.csv")


#grafico de datos crudos (es posible ver las repeticiones)
plot <- ggplot(data = a, aes(x = nm, y = reflec)) +
  geom_line(aes(group = interaction(id, rep), color = id)) +
  labs(title = "Espectro de Reflectancia por Repetición",
       x = "Longitud de Onda (nm)",
       y = "Reflectancia") +
  theme_minimal()

print(plot)

library(ggplot2)

spectro_analysis_resume <- function(df) {
  # Calcular el resumen estadístico
  summary_results <- df %>%
    group_by(banda, id, nm) %>%
    summarise(
      mean_reflec = mean(reflec, na.rm = TRUE),
      sd_reflec = sd(reflec, na.rm = TRUE),
      n = n(),  # Número de observaciones para calcular el error estándar
      se_reflec = sd(reflec, na.rm = TRUE) / sqrt(n()),  # Error estándar
      max_reflec = max(reflec, na.rm = TRUE),
      min_reflec = min(reflec, na.rm = TRUE),
      median_reflec = median(reflec, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Crear el gráfico completo
  plot_complete <- ggplot(summary_results, aes(x = nm, y = mean_reflec, color = id, group = interaction(id, banda))) +
    geom_line() +  
    geom_errorbar(aes(ymin = mean_reflec - se_reflec, ymax = mean_reflec + se_reflec), width = 0.2) +  
    labs(title = "Espectro de Reflectancia Completo",
         x = "Longitud de Onda (nm)",
         y = "Reflectancia Media") +
    theme_minimal() +
    geom_vline(xintercept = 496, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 571, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 621, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 700, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 750, linetype = "dashed", color = "gray") +
    annotate("text", x = 473.5, y = 0.85, label = "Azul", angle = 0, vjust = 1, hjust = 0.5, color = "black") +
    annotate("text", x = 533.5, y = 0.85, label = "Verde", angle = 0, vjust = 1, hjust = 0.5, color = "black") +
    annotate("text", x = 596, y = 0.85, label = "Amarillo", angle = 0, vjust = 1, hjust = 0.5, color = "black") +
    annotate("text", x = 660.5, y = 0.85, label = "Rojo", angle = 0, vjust = 1, hjust = 0.5, color = "black") +
    annotate("text", x = 725, y = 0.85, label = "Red Edge", angle = 0, vjust = 1, hjust = 0.5, color = "black") +
    annotate("text", x = 825, y = 0.85, label = "NIR", angle = 0, vjust = 1, hjust = 0.5, color = "black")
  
  # Imprimir el gráfico completo
  print(plot_complete)
  
  # Crear gráficos por banda específica
  bandas <- list(
    Azul = c(451, 496),
    Verde = c(496, 571),
    Amarillo = c(571, 621),  # Añadido el rango Amarillo
    Rojo = c(621, 700),
    RedEdge = c(700, 750),
    NIR = c(750, 900)
  )
  
  for (banda in names(bandas)) {
    rango <- bandas[[banda]]
    
    plot_band <- ggplot(subset(summary_results, nm >= rango[1] & nm <= rango[2]), 
                        aes(x = nm, y = mean_reflec, color = id, group = interaction(id, banda))) +
      geom_line() +  
      geom_errorbar(aes(ymin = mean_reflec - se_reflec, ymax = mean_reflec + se_reflec), width = 0.2) +  
      labs(title = paste("Banda", banda, paste0("(", rango[1], "-", rango[2], " nm)")),
           x = "Longitud de Onda (nm)",
           y = "Reflectancia Media") +
      theme_minimal() +
      xlim(rango[1], rango[2])
    
    # Imprimir el gráfico de la banda
    print(plot_band)
  }
  
  # Retornar los resultados resumidos
  return(summary_results)
}

# Uso de la función con un data frame de ejemplo
summary_results <- spectro_analysis_resume(a)