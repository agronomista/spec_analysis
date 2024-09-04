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
      
      actionButton("process", "Procesar Datos"),
      
      hr(),  # Separador visual
      
      # Para la unión de archivos CSV
      fileInput("multi_files", "Seleccionar varios archivos CSV para combinar", 
                accept = c(".csv"), multiple = TRUE),
      actionButton("combine", "Unir Archivos")
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
        tabPanel("NIR", plotlyOutput("plot_band_nir")),
        tabPanel("Unir Archivos CSV", tableOutput("combined_data"))
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
      prefix_ui[[i]] <- tagList(
        br(),
        h4(paste("Especie", i)),
        helpText(paste("Ingrese el prefijo original", i, ":")),
        textInput(paste0("prefix_old_", i), 
                  label = paste("Prefijo original ", i, ":"), 
                  value = ""),
        
        helpText(paste("Ingrese el nuevo prefijo con el que desea reemplazar el original en el ID", i, ":")),
        textInput(paste0("prefix_new_", i), 
                  label = paste("Nuevo prefijo ", i, ":"), 
                  value = "")
      )
    }
    return(prefix_ui)
  })
  
  # Procesar un archivo CSV al presionar el botón "Procesar Datos"
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
        theme_minimal()
      ggplotly(p)
    })
  })
  
  # Unir múltiples archivos CSV
  observeEvent(input$combine, {
    req(input$multi_files)
    
    # Leer y combinar los archivos CSV
    combined_data <- lapply(input$multi_files$datapath, read.csv, header = FALSE, sep = ";")
    combined_data <- bind_rows(combined_data)  # Unir los archivos
    
    # Mostrar los datos combinados en la pestaña correspondiente
    output$combined_data <- renderTable({
      combined_data
    })
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
