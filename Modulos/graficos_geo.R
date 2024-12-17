graficosGeoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(
        ns("variable_seleccionada"),
        "Seleccione la variable para el gráfico:",
        choices = NULL  # Se llenará dinámicamente desde el servidor
      )),
      column(4, uiOutput(ns("selector_municipios"))),  # Selector dinámico de municipios
      column(4, checkboxGroupInput(
        ns("filtro_respuesta"),
        "Filtrar Respuesta:",
        choices = list("Sí" = 1, "No" = 0),
        selected = c(1, 0)  # Mostrar ambas opciones por defecto
      ))
    ),
    fluidRow(
      column(12, plotlyOutput(ns("grafico_dispersion"), height = "500px"))
    ),
    fluidRow(
      column(12, uiOutput(ns("leyenda_municipios")))  # Usar la leyenda generada por tu función
    ),
    fluidRow(
      column(12, actionButton(ns("guardar_grafico"), "Guardar Gráfico"))
    )
    
  )
}



graficosGeo <- function(input, output, session, datos_relevantes, carpeta_informe) {
  ns <- session$ns
  
  # Definir leyendas localmente
  variables_leyendas <- list(
    FamDiag = "Miembro de la familia ha sido diagnosticado",
    FamHosp = "Miembro de la familia ha sido hospitalizado",
    Caso_6m = "Caso registrado en los últimos 6 meses",
    ZancViv = "Zancudos observados en el área de vivienda",
    LarvViv = "Larvas observadas en el área de vivienda",
    fam_entrev = "Familias entrevistadas para el estudio",
    H_Aeg = "Hembras de Aedes aegypti",
    M_Aeg = "Machos de Aedes aegypti",
    H_albo = "Hembras de Aedes albopictus",
    M_albo = "Machos de Aedes albopictus"
  )
  
  # UI para el selector de municipios
  output$selector_municipios <- renderUI({
    req(datos_relevantes())
    municipios <- unique(datos_relevantes()$Municipio)
    checkboxGroupInput(
      ns("municipios_filtro"),
      "Seleccione Municipios:",
      choices = municipios,
      selected = municipios  # Seleccionar todos por defecto
    )
  })
  
  # Actualizar opciones del selector de variables
  observe({
    updateSelectInput(
      session,
      "variable_seleccionada",
      choices = setNames(names(variables_leyendas), variables_leyendas),  # Clave: nombre técnico, Valor: leyenda
      selected = names(variables_leyendas)[1]
    )
  })
  
  
  # Generar el gráfico dinámico
  grafico_actual <- reactive({
    req(datos_relevantes(), input$variable_seleccionada, input$municipios_filtro)
    
    datos <- datos_relevantes()
    variable <- input$variable_seleccionada
    leyenda <- variables_leyendas[[variable]]   # Usar la leyenda para el título
    
    # Filtrar datos
    datos_filtrados <- datos %>%
      filter(Municipio %in% input$municipios_filtro) %>%
      filter(.data[[variable]] %in% as.numeric(input$filtro_respuesta))
    
    req(nrow(datos_filtrados) > 0)
    
    # Crear gráfico de dispersión interactivo con plotly
    plot_ly(
      data = datos_filtrados,
      x = ~Coor_Long,
      y = ~Coor_Lat,
      color = ~factor(.data[[variable]], levels = c(1, 0), labels = c("Sí", "No")),
      colors = c("blue", "red"),  # Especificar colores: azul para "Sí", rojo para "No"
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10)
    ) %>%
      layout(
        title = paste("Localización de familias por:", leyenda),  # Título con leyenda
        xaxis = list(title = "Longitud"),
        yaxis = list(title = "Latitud"),
        showlegend = TRUE  # Ocultar la leyenda
      )
  })
  
  
  
  # Mostrar el gráfico en la UI
  output$grafico_dispersion <- renderPlotly({
    plotly::config(
      grafico_actual(),
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "lasso2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "toImage"
      ),
      modeBarButtonsToAdd = c("zoom2d", "pan2d", "autoScale2d", "resetScale2d")
    )
  })
  
  # Función auxiliar para generar un nombre único
  generar_nombre_unico <- function(base_path, nombre_base) {
    contador <- 1
    nombre <- paste0(nombre_base, "_", contador, ".png")
    
    # Incrementar el contador si el archivo ya existe
    while (file.exists(file.path(base_path, nombre))) {
      contador <- contador + 1
      nombre <- paste0(nombre_base, "_", contador, ".png")
    }
    
    return(nombre)
  }
  
  # Guardar el gráfico cuando se presiona el botón
  observeEvent(input$guardar_grafico, {
    req(datos_relevantes(), input$variable_seleccionada, carpeta_informe())
    
    # Evaluar y verificar la carpeta de informe
    carpeta_informe_evaluada <- carpeta_informe()
    
    # Crear la subcarpeta si no existe
    subcarpeta <- file.path(carpeta_informe_evaluada, "graficos_dispersion")
    if (!dir.exists(subcarpeta)) {
      dir.create(subcarpeta, recursive = TRUE)
    }
    
    # Obtener los datos relevantes y la variable seleccionada
    datos <- datos_relevantes()
    variable <- input$variable_seleccionada
    
    # Generar un nombre base basado en los filtros seleccionados
    filtros <- paste(input$filtros_seleccionados, collapse = "_")
    nombre_base <- paste0("grafico_", variable, "_", filtros)
    
    # Generar un nombre único
    nombre_archivo <- generar_nombre_unico(subcarpeta, nombre_base)
    
    # Crear el gráfico interactivo
    grafico <- grafico_actual()  # Genera el gráfico interactivo
    
    # Guardar como HTML (sin necesidad de agregar número aquí, solo como referencia)
    archivo_html <- guardar_mapa_html(grafico, subcarpeta, tools::file_path_sans_ext(nombre_archivo))
    
    # Guardar como PNG
    guardar_mapa_png(archivo_html, subcarpeta, tools::file_path_sans_ext(nombre_archivo))
    
    # Notificar éxito
    showNotification(paste("Gráfico guardado como:", nombre_archivo), type = "message")
  })
  
  
} 
