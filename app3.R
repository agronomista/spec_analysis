library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)  # Para usar sample_n()

# Función para realizar el análisis espectral
spectro_analysis <- function(df) {
  df_subset <- df[, 1:4]
  colnames(df_subset) <- c("id", "registro", "nm", "reflec")
  
  segmented_data <- list(
    UV = df_subset[df_subset$nm %in% 350:400, ],   # Rango UV
    Violeta = df_subset[df_subset$nm %in% 400:450, ],  # Rango Violeta (400-450 nm)
    Azul = df_subset[df_subset$nm %in% 451:496, ],  # Rango Azul
    Verde = df_subset[df_subset$nm %in% 496:571, ],  # Rango Verde
    Amarillo = df_subset[df_subset$nm %in% 571:621, ],  # Rango Amarillo
    Rojo = df_subset[df_subset$nm %in% 621:700, ],  # Rango Rojo
    RedEdge = df_subset[df_subset$nm %in% 700:750, ],  # Rango RedEdge
    NIR = df_subset[df_subset$nm %in% 750:900, ]   # Rango NIR
  )
  
  for (name in names(segmented_data)) {
    segmented_data[[name]]$banda <- name
  }
  
  output <- do.call(rbind, segmented_data)
  output$rep <- ave(output$registro, output$id, output$registro, output$banda, output$nm, FUN = seq_along)
  return(output)
}

# Función para generar resumen de análisis espectral
spectro_analysis_resume <- function(df) {
  summary_results <- df %>%
    group_by(banda, id, nm) %>%
    summarise(
      mean_reflec = mean(reflec, na.rm = TRUE),
      sd_reflec = sd(reflec, na.rm = TRUE),
      n = n(),
      se_reflec = sd_reflec / sqrt(n)
    ) %>%
    ungroup()
  return(summary_results)
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

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("SpectraCSV con UV y Violeta"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Examinar CSV",
                 fileInput("exam_csv", "Seleccionar archivo CSV para examinar", accept = c(".csv")),
                 tableOutput("examined_data")
        ),
        tabPanel("Unir CSV",
                 fileInput("csvs", "Selecciona los archivos CSV", multiple = TRUE, accept = ".csv"),
                 tableOutput("contents"),
                 downloadButton("downloadData", "Descargar CSV combinado")
        ),
        tabPanel("Visualizador",
                 fileInput("file", "Seleccionar archivo CSV", accept = c(".csv")),
                 selectInput("num_prefix", "Cantidad de especies a analizar:", choices = 1:5, selected = 1),
                 uiOutput("dynamic_prefix_inputs"),
                 actionButton("process", "Procesar Datos")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datos Crudos", plotlyOutput("plot_raw")),
        tabPanel("Espectro Completo", plotlyOutput("plot_complete")),
        tabPanel("UV", plotlyOutput("plot_band_uv")),  # Pestaña para UV
        tabPanel("Violeta", plotlyOutput("plot_band_violeta")),  # Pestaña para Violeta
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
  
  # --- Funcionalidad para Unir CSV ---
  combined_data <- reactive({
    req(input$csvs)
    
    if (is.null(input$csvs$datapath)) {
      return(NULL)
    }
    
    # Lee y combina los archivos CSV por filas
    archivos <- lapply(input$csvs$datapath, function(path) {
      read.csv(path, header = FALSE, sep = ";")
    })
    
    # Combina los archivos por filas
    combined <- do.call(rbind, archivos)
    
    return(combined)
  })
  
  # Muestra una muestra de 5 registros aleatorios combinados en la interfaz
  output$contents <- renderTable({
    data <- combined_data()
    if (!is.null(data)) {
      sample_n(data, size = min(5, nrow(data)))  # Muestra 5 registros al azar o menos si hay menos de 5
    }
  })
  
  # Permite descargar el archivo combinado
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("union_combined", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- combined_data()
      if (is.null(data)) {
        return(NULL)
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # --- Funcionalidad para Examinar CSV ---
  output$examined_data <- renderTable({
    req(input$exam_csv)
    data <- read.csv(input$exam_csv$datapath, header = FALSE, sep = ";")
    sample_n(data, size = min(5, nrow(data)))
  })
  
  # --- Funcionalidad para Análisis de Firmas Espectrales ---
  output$dynamic_prefix_inputs <- renderUI({
    num <- as.numeric(input$num_prefix)
    prefix_ui <- list()
    
    for (i in 1:num) {
      prefix_ui[[i]] <- tagList(
        br(),
        h4(paste("Especie", i)),
        helpText(paste("Ingrese el prefijo original", i, ":")),
        textInput(paste0("prefix_old_", i), label = paste("Prefijo original ", i, ":"), value = ""),
        helpText(paste("Ingrese el nuevo prefijo con el que desea reemplazar el original en el ID", i, ":")),
        textInput(paste0("prefix_new_", i), label = paste("Nuevo prefijo ", i, ":"), value = "")
      )
    }
    return(prefix_ui)
  })
  
  observeEvent(input$process, {
    req(input$file)
    
    test_open <- read.csv(input$file$datapath, header = FALSE, sep = ";")
    test_open[, 4] <- gsub(",", ".", test_open[, 4])
    test_open[, 4] <- as.numeric(test_open[, 4])
    
    num <- as.numeric(input$num_prefix)
    for (i in 1:num) {
      old_prefix <- input[[paste0("prefix_old_", i)]]
      new_prefix <- input[[paste0("prefix_new_", i)]]
      test_open[, 1] <- gsub(paste0("^", old_prefix, ".*"), new_prefix, test_open[, 1])
    }
    test_open[, 1] <- as.factor(test_open[, 1])
    
    a <- spectro_analysis(test_open)
    
    output$plot_raw <- renderPlotly({
      p <- ggplot(data = a, aes(x = nm, y = reflec)) +
        geom_line(aes(group = interaction(id, rep), color = id)) +
        labs(title = "Espectro de Reflectancia por Repetición",
             x = "Longitud de Onda (nm)",
             y = "Reflectancia") +
        theme_minimal()
      ggplotly(p)
    })
    
    summary_results <- spectro_analysis_resume(a)
    
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
    
    output$plot_band_uv <- renderPlotly({
      plotly::ggplotly(plot_band_by_range(summary_results, "UV", 350, 400))
    })
    
    output$plot_band_violeta <- renderPlotly({
      plotly::ggplotly(plot_band_by_range(summary_results, "Violeta", 400, 450))
    })
    
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

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
