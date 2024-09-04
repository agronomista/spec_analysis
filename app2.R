library(shiny)
library(dplyr)  # Para usar sample_n()

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Unir Múltiples Archivos CSV por Filas con Progreso"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("csvs", "Selecciona los archivos CSV", multiple = TRUE, accept = ".csv"),
      downloadButton("downloadData", "Descargar CSV combinado")
    ),
    
    mainPanel(
      tableOutput("contents"),  # Muestra la tabla de registros aleatorios
      textOutput("progressText")
    )
  )
)

# Define el servidor
server <- function(input, output, session) {
  
  # Función para cargar y combinar los archivos por filas con barra de progreso
  combined_data <- reactive({
    req(input$csvs)
    
    # Asegúrate de que se hayan seleccionado archivos
    if (is.null(input$csvs$datapath)) {
      return(NULL)
    }
    
    # Inicializa la barra de progreso
    total_files <- length(input$csvs$datapath)
    progress <- shiny::Progress$new()
    progress$set(message = "Uniendo archivos CSV...", value = 0)
    on.exit(progress$close())
    
    # Lee y combina los archivos CSV uno por uno con progreso
    archivos <- list()
    for (i in seq_along(input$csvs$datapath)) {
      archivos[[i]] <- read.csv(input$csvs$datapath[i], header = FALSE, sep = ";")
      # Actualiza la barra de progreso
      progress$inc(1 / total_files, detail = paste("Procesando archivo", i, "de", total_files))
    }
    
    # Combina los archivos por filas (unión vertical)
    combined <- do.call(rbind, archivos)
    
    # Retorna los datos combinados
    combined
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
      # Asegúrate de que los datos están disponibles antes de guardar
      data <- combined_data()
      if (is.null(data)) {
        return(NULL)
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Ejecuta la aplicación Shiny
shinyApp(ui = ui, server = server)
