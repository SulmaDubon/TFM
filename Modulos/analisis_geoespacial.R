source("modulos/Funciones/funciones_geoespaciales.R")  

analisisGeoespacialUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, selectInput(
        ns("variable_mapa"),
        "Seleccione una variable para graficar:",
        choices = NULL  # Se llenará dinámicamente desde el servidor
      )),
      column(6, checkboxGroupInput(
        ns("municipios_filtro"),
        "Seleccione municipios:",
        choices = NULL  # Se llenará dinámicamente desde el servidor
      ))
    ),
    fluidRow(
      column(12, leafletOutput(ns("mapa_geoespacial"), height = "500px"))
    ),
    fluidRow(
      column(12, actionButton(ns("guardar_grafico"), "Guardar Gráfico"))
    )
  )
}


analisisGeoespacial <- function(input, output, session, datos_relevantes, carpeta_informe) {
  ns <- session$ns
  
  # Leyendas para las variables
  variables_leyendas <- list(
    FamDiag = "Familiar diagnosticado",
    FamHosp = "Familiar hospitalizado",
    Caso_6m = "Casos en últimos 6 meses",
    ZancViv = "Zancudos observados en la vivienda",
    LarvViv = "Larvas observadas en la vivienda",
    fam_entrev = "Familias entrevistadas",
    H_Aeg = "Hembras de Aedes aegypti",
    M_Aeg = "Machos de Aedes aegypti",
    H_albo = "Hembras de Aedes albopictus",
    M_albo = "Machos de Aedes albopictus"
  )
  
  # Actualizar selectores dinámicos
  observe({
    req(datos_relevantes())
    updateSelectInput(
      session,
      "variable_mapa",
      choices = setNames(names(variables_leyendas), variables_leyendas),
      selected = "fam_entrev"
    )
    
    municipios <- unique(datos_relevantes()$Municipio)
    updateCheckboxGroupInput(
      session,
      "municipios_filtro",
      choices = municipios,
      selected = municipios
    )
  })
  
  # Renderizar el mapa interactivo
  output$mapa_geoespacial <- renderLeaflet({
    req(datos_relevantes(), input$variable_mapa, input$municipios_filtro)
    
    datos <- datos_relevantes()
    
    # Filtrar por municipios seleccionados
    if (!is.null(input$municipios_filtro) && length(input$municipios_filtro) > 0) {
      datos <- datos %>% filter(Municipio %in% input$municipios_filtro)
    }
    
    # Crear el mapa
    mapa <- crear_mapa(
      data = datos,
      variable = input$variable_mapa,
      leyenda = variables_leyendas[[input$variable_mapa]],
      municipios_seleccionados = input$municipios_filtro
    )
    
    return(mapa)
  })
  
  # Guardar el gráfico cuando se presiona el botón
  observeEvent(input$guardar_grafico, {
    req(datos_relevantes(), input$variable_mapa, carpeta_informe())
    
    # Obtener los datos relevantes
    datos <- datos_relevantes()
    
    # Filtrar por municipios seleccionados
    municipios_seleccionados <- input$municipios_filtro
    if (is.null(municipios_seleccionados) || length(municipios_seleccionados) == 0) {
      municipios_seleccionados <- unique(datos$Municipio)
    }
    datos_filtrados <- datos %>% filter(Municipio %in% municipios_seleccionados)
    
    # Crear el mapa interactivo para los municipios seleccionados
    mapa <- crear_mapa(
      data = datos_filtrados,
      variable = input$variable_mapa,
      leyenda = variables_leyendas[[input$variable_mapa]],
      municipios_seleccionados = municipios_seleccionados
    )
    
    # Crear subcarpeta para guardar los gráficos
    carpeta_guardado <- file.path(carpeta_informe(), "mapas_interactivos")
    if (!dir.exists(carpeta_guardado)) {
      dir.create(carpeta_guardado, recursive = TRUE)
    }
    
    # Función auxiliar para generar un nombre único
    generar_nombre_unico <- function(base_path, nombre_base) {
      contador <- 1
      nombre <- paste0(nombre_base, "_", contador, ".png")
      while (file.exists(file.path(base_path, nombre))) {
        contador <- contador + 1
        nombre <- paste0(nombre_base, "_", contador, ".png")
      }
      return(nombre)
    }
    
    # Generar un nombre base basado únicamente en la variable seleccionada
    titulo_base <- paste("Mapa_de", variables_leyendas[[input$variable_mapa]])
    titulo_base <- gsub(" ", "_", titulo_base)
    
    # Generar un nombre único para el archivo PNG
    nombre_archivo <- generar_nombre_unico(carpeta_guardado, titulo_base)
    
    # Guardar como HTML
    archivo_html <- guardar_mapa_html(mapa, carpeta_guardado, tools::file_path_sans_ext(nombre_archivo))
    
    # Guardar como PNG con nombre único
    guardar_mapa_png(archivo_html, carpeta_guardado, tools::file_path_sans_ext(nombre_archivo))
    
    # Notificar éxito
    showNotification(paste("Gráfico guardado correctamente como:", nombre_archivo), type = "message")
  })
}

