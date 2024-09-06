# Cargar las librerías necesarias
library(shiny)
library(ggplot2)
library(plotly)
library(pacman)
p_load(tidyverse)

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Análisis de Firmas espectrales"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Seleccionar archivo CSV", accept = c(".csv")),
      
      # Selección del número de prefijos a modificar
      selectInput("num_prefix", "Cantidad de especies a analizar:", 
                  choices = 1:5, selected = 1),
      
      # Generar dinámicamente los campos de entrada para los prefijos
      uiOutput("dynamic_prefix_inputs"),
      
      actionButton("process", "Procesar Datos")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datos Crudos", plotlyOutput("plot_raw")),
        tabPanel("Espectro Completo", plotlyOutput("plot_complete")),
        tabPanel("Azul", plotlyOutput("plot_band_azul")),
        tabPanel("Verde", plotlyOutput("plot_band_verde")),
        tabPanel("Amarillo", plotlyOutput("plot_band_amarillo")),
        tabPanel("Rojo", plotlyOutput("plot_band_rojo")),
        tabPanel("RedEdge", plotlyOutput("plot_band_rededge")),
        tabPanel("NIR", plotlyOutput("plot_band_nir"))
      )
    )
  )
)

# Definir la lógica del servidor (Server)
server <- function(input, output, session) {
  
  # Generar dinámicamente los campos de prefijos según la selección del usuario
  output$dynamic_prefix_inputs <- renderUI({
    num <- as.numeric(input$num_prefix)
    prefix_ui <- list()
    
    for (i in 1:num) {
      # Establecer valores por defecto para el primer prefijo
      if (i == 1) {
        old_default <- ""
        new_default <- ""
      } else if (i == 2) {
        old_default <- ""
        new_default <- ""
      } else {
        old_default <- paste0("", i)
        new_default <- paste0("", i)
      }
      
      prefix_ui[[i]] <- tagList(
        br(),
        h4(paste("Especie", i)),
        helpText(paste("Ingrese el prefijo original", i, ":")),
        textInput(paste0("prefix_old_", i), 
                  label = paste("Prefijo original ", i, ":"), 
                  value = old_default),
        
        helpText(paste("Ingrese el nuevo prefijo con el que desea reemplazar el original en el ID", i, ":")),
        textInput(paste0("prefix_new_", i), 
                  label = paste("Nuevo prefijo ", i, ":"), 
                  value = new_default)
      )
    }
    return(prefix_ui)
  })
  
  # Función para leer y procesar los datos al presionar el botón "Procesar Datos"
  observeEvent(input$process, {
    req(input$file)
    
    # Leer el archivo CSV
    test_open <- read.csv(input$file$datapath, header = FALSE, sep = ";")
    
    # Cambiar de comas a puntos y convertir a numérico en la cuarta columna
    test_open[, 4] <- gsub(",", ".", test_open[, 4])
    test_open[, 4] <- as.numeric(test_open[, 4])
    
    # Aplicar la modificación de prefijos según la cantidad seleccionada
    num <- as.numeric(input$num_prefix)
    for (i in 1:num) {
      old_prefix <- input[[paste0("prefix_old_", i)]]
      new_prefix <- input[[paste0("prefix_new_", i)]]
      test_open[, 1] <- gsub(paste0("^", old_prefix, ".*"), new_prefix, test_open[, 1])
    }
    test_open[, 1] <- as.factor(test_open[, 1])
    
    # Aplicar la función de análisis espectral
    a <- spectro_analysis(test_open)
    
    # Generar y renderizar los gráficos
    
    # Gráfico de datos crudos
    output$plot_raw <- renderPlotly({
      p <- ggplot(data = a, aes(x = nm, y = reflec)) +
        geom_line(aes(group = interaction(id, rep), color = id)) +
        labs(title = "Espectro de Reflectancia por Repetición",
             x = "Longitud de Onda (nm)",
             y = "Reflectancia") +
        theme_minimal()
      ggplotly(p)
    })
    
    # Resumen de datos y generación de gráficos
    summary_results <- spectro_analysis_resume(a)
    
    # Gráfico completo
    output$plot_complete <- renderPlotly({
      p <- ggplot(summary_results, aes(x = nm, y = mean_reflec, color = id, group = interaction(id, banda))) +
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
      ggplotly(p)
    })
    
    # Gráficos por banda
    output$plot_band_azul <- renderPlotly({
      plotly::ggplotly(plot_band_by_range(summary_results, "Azul", 451, 496))
    })
    
    output$plot_band_verde <- renderPlotly({
      plotly::ggplotly(plot_band_by_range(summary_results, "Verde", 496, 571))
    })
    
    output$plot_band_amarillo <- renderPlotly({
      plotly::ggplotly(plot_band_by_range(summary_results, "Amarillo", 571, 621))
    })
    
    output$plot_band_rojo <- renderPlotly({
      plotly::ggplotly(plot_band_by_range(summary_results, "Rojo", 621, 700))
    })
    
    output$plot_band_rededge <- renderPlotly({
      plotly::ggplotly(plot_band_by_range(summary_results, "RedEdge", 700, 750))
    })
    
    output$plot_band_nir <- renderPlotly({
      plotly::ggplotly(plot_band_by_range(summary_results, "NIR", 750, 900))
    })
    
  })
}

# Función para generar gráficos por banda
plot_band_by_range <- function(summary_results, banda_name, nm_start, nm_end) {
  p <- ggplot(subset(summary_results, nm >= nm_start & nm <= nm_end), 
              aes(x = nm, y = mean_reflec, color = id, group = interaction(id, banda))) +
    geom_line() +  
    geom_errorbar(aes(ymin = mean_reflec - se_reflec, ymax = mean_reflec + se_reflec), width = 0.2) +  
    labs(title = paste("Banda", banda_name, paste0("(", nm_start, "-", nm_end, " nm)")),
         x = "Longitud de Onda (nm)",
         y = "Reflectancia Media") +
    theme_minimal() +
    xlim(nm_start, nm_end)
  return(p)
}

# Definir la función spectro_analysis
spectro_analysis <- function(df) {
  df_subset <- df[, 1:4]
  colnames(df_subset) <- c("id", "registro", "nm", "reflec")
  
  # Definir los rangos espectrales revisados
  AzulRW1 <- seq(451, 496, by = 1)
  VerdeRW1 <- seq(496, 571, by = 1)
  AmarilloRW1 <- seq(571, 590, by = 1)
  RojoRW1 <- seq(590, 700, by = 1)
  RED_EDGERW1 <- seq(700, 751, by = 1)
  NIRRW1 <- seq(751, 901, by = 1)
  
  segmented_data <- list()
  
  segmented_data$Azul <- df_subset[df_subset$nm %in% AzulRW1, ]
  segmented_data$Azul$banda <- "Azul"
  
  segmented_data$Verde <- df_subset[df_subset$nm %in% VerdeRW1, ]
  segmented_data$Verde$banda <- "Verde"
  
  segmented_data$Amarillo <- df_subset[df_subset$nm %in% AmarilloRW1, ]
  segmented_data$Amarillo$banda <- "Amarillo"
  
  segmented_data$Rojo <- df_subset[df_subset$nm %in% RojoRW1, ]
  segmented_data$Rojo$banda <- "Rojo"
  
  segmented_data$RedEdge <- df_subset[df_subset$nm %in% RED_EDGERW1, ]
  segmented_data$RedEdge$banda <- "RedEdge"
  
  segmented_data$NIR <- df_subset[df_subset$nm %in% NIRRW1, ]
  segmented_data$NIR$banda <- "NIR"
  
  output <- do.call(rbind, segmented_data)
  output$rep <- ave(output$registro, output$id, output$registro, output$banda, output$nm, FUN = seq_along)
  output <- output[, c("id", "registro", "nm", "banda", "reflec", "rep")]
  
  return(output)
}

# Definir la función spectro_analysis_resume
spectro_analysis_resume <- function(df) {
  summary_results <- df %>%
    group_by(banda, id, nm) %>%
    summarise(
      mean_reflec = mean(reflec, na.rm = TRUE),
      sd_reflec = sd(reflec, na.rm = TRUE),
      n = n(),
      se_reflec = sd_reflec / sqrt(n),
      max_reflec = max(reflec, na.rm = TRUE),
      min_reflec = min(reflec, na.rm = TRUE),
      median_reflec = median(reflec, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(summary_results)
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
