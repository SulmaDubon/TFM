inferenciaEstadisticaUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Selección de variables en la parte superior
    fluidRow(
      column(3,
             selectInput(
               ns("variable_dependiente"),
               "Seleccione variable dependiente:",
               choices = NULL
             )
      ),
      column(3,
             selectInput(
               ns("categoria_seleccionada"),
               "Seleccione Categoría:",
               choices = NULL
             )
      ),
      column(6,
             uiOutput(ns("radio_variables"))  # Variables independientes dinámicas
      )
    ),
    hr(),
    # Resultados en la parte inferior
    fluidRow(
      column(12,
             navlistPanel(
               tabPanel("Prueba de Fisher", 
                        tableOutput(ns("fisher_resultados")),
                        actionButton(ns("agregar_a_lista"), "Agregar a Lista")
               ),
               tabPanel("Intervalos de Confianza", tableOutput(ns("intervalos_resultados")))
             )
      )
    ),
    fluidRow(
      column(12,
             tableOutput(ns("tabla_resultados")), 
             downloadButton(ns("guardar_tabla"), "Guardar Tabla")
      )
    )
  )
}


inferenciaEstadistica <- function(input, output, session, datos_completos, categorias, carpeta_informe) {
  ns <- session$ns
  
  # Leyendas de variables dependientes
  variables_leyendas <- list(
    FamDiag = "Miembro de la familia ha sido diagnosticado",
    FamHosp = "Miembro de la familia ha sido hospitalizado",
    Caso_6m = "Caso registrado en los últimos 6 meses",
    ZancViv = "Zancudos observados en el área de vivienda",
    LarvViv = "Larvas observadas en el área de vivienda"
  )
  
  # Actualizar opciones de selectores
  observe({
    req(categorias)
    updateSelectInput(session, "variable_dependiente", choices = setNames(names(variables_leyendas), unlist(variables_leyendas)))
    categorias_con_municipio <- c("Municipio" = "Municipio", categorias)
    updateSelectInput(session, "categoria_seleccionada", choices = names(categorias_con_municipio))
  })
  
  output$radio_variables <- renderUI({
    req(input$categoria_seleccionada, categorias)
    variables <- if (input$categoria_seleccionada == "Municipio") "Municipio" else categorias[[input$categoria_seleccionada]]
    tags$div(
      style = "column-count: 2; column-gap: 20px;",
      radioButtons(
        ns("variables_independientes"),
        label = "variable independiete",
        choices = variables,
        selected = NULL
      )
    )
  })
  
  # Función  para calcular la prueba de Fisher
  calcular_fisher <- function(datos, variable_dep, variable_indep) {
    tabla <- table(datos[[variable_dep]], datos[[variable_indep]])
    
    # Validar que la tabla sea 2x2
    if (nrow(tabla) != 2 || ncol(tabla) != 2) {
      showNotification("La tabla de contingencia no es 2x2. La prueba de Fisher no puede ser realizada.", type = "error")
      return(NULL)
    }
    
    # Realizar la prueba si la tabla es válida
    fisher_result <- fisher.test(tabla)
    fisher_tidy <- broom::tidy(fisher_result)
    
    data.frame(
      "Prueba" = "Prueba exacta de Fisher",
      "Valor p" = fisher_tidy$p.value,
      "Odds ratio" = fisher_tidy$estimate,
      "IC Inferior" = fisher_tidy$conf.low,
      "IC Superior" = fisher_tidy$conf.high
    )
  }
  
  
  calcular_intervalos_confianza <- function(datos, variable) {
    tabla <- datos %>%
      group_by(!!sym(variable)) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(
        p = n / sum(n),
        IC_Lower = p - qnorm(0.975) * sqrt((p * (1 - p)) / sum(n)),
        IC_Upper = p + qnorm(0.975) * sqrt((p * (1 - p)) / sum(n)),
        Tamaño_muestra = ceiling((qnorm(0.975)^2 * p * (1 - p)) / 0.05^2)
      )
    return(tabla)
  }
  
  # Salida para Fisher
  output$fisher_resultados <- renderTable({
    req(input$variable_dependiente, input$variables_independientes, datos_completos())
    fisher_result <- calcular_fisher(datos_completos(), input$variable_dependiente, input$variables_independientes)
    
    if (is.null(fisher_result)) {
      return(data.frame("Error" = "No se puede calcular la prueba: la tabla no es 2x2."))
    }
    fisher_result
  })
  
  
  # Salida para Intervalos de Confianza
  output$intervalos_resultados <- renderTable({
    req(input$variable_dependiente, datos_completos())
    calcular_intervalos_confianza(datos_completos(), input$variable_dependiente)
  })
  
  # Tabla reactiva para almacenar resultados
  resultados_lista <- reactiveVal(data.frame())
  
  # Evento para agregar resultados a la lista
  observeEvent(input$agregar_a_lista, {
    req(input$variable_dependiente, input$variables_independientes, datos_completos())
    
    fisher_result <- calcular_fisher(datos_completos(), input$variable_dependiente, input$variables_independientes)
    
    # Si no se puede realizar la prueba, no agregar resultados
    if (is.null(fisher_result)) {
      showNotification("No se puede agregar a la lista porque la tabla no es 2x2.", type = "error")
      return()
    }
    
    # Agregar resultado si la prueba es válida
    nuevo_resultado <- data.frame(
      "Variable Dependiente" =  input$variable_dependiente,
      "Variable Independiente" = input$variables_independientes,
      fisher_result[2:5]
    )
    resultados_lista(rbind(resultados_lista(), nuevo_resultado))
  })
  
  
  output$tabla_resultados <- renderTable({
    req(nrow(resultados_lista()) > 0)
    resultados_lista()
  })
  
  output$guardar_tabla <- downloadHandler(
    filename = function() paste("resultados_fisher", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(resultados_lista(), file, row.names = FALSE)
  )
}

