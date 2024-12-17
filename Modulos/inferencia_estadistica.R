# Parte del UI
inferenciaEstadisticaUI <- function(id) {
  ns <- NS(id)
  tagList(
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
             uiOutput(ns("radio_variables"))  
      )
    ),
    hr(),
    fluidRow(
      column(12,
             navlistPanel(
               tabPanel("Prueba de Fisher", 
                        tableOutput(ns("fisher_resultados")),
                        actionButton(ns("agregar_a_lista"), "Agregar a Lista")
               ),
               tabPanel("Intervalos de Confianza", 
                        tableOutput(ns("intervalos_resultados")),
                        actionButton(ns("agregar_intervalos"), "Agregar Intervalos a Lista")
               )
             )
      )
    ),
    hr(),
    fluidRow(
      column(12,
             tableOutput(ns("tabla_resultados")), 
             downloadButton(ns("guardar_tabla"), "Guardar Tabla")
      )
    )
  )
}

# Parte del Server
inferenciaEstadistica <- function(input, output, session, datos_completos, categorias, carpeta_informe) {
  ns <- session$ns
  
  # Estados reactivos para Fisher e Intervalos
  resultados_fisher_lista <- reactiveVal(data.frame())
  resultados_intervalos_lista <- reactiveVal(data.frame())
  
  # Prueba de Fisher
  calcular_fisher <- function(datos, variable_dep, variable_indep) {
    tabla <- table(datos[[variable_dep]], datos[[variable_indep]])
    if (nrow(tabla) != 2 || ncol(tabla) != 2) return(NULL)
    fisher_result <- broom::tidy(fisher.test(tabla))
    data.frame(
      "Prueba" = "Prueba exacta de Fisher",
      "Valor p" = fisher_result$p.value,
      "Odds ratio" = fisher_result$estimate,
      "IC Inferior" = fisher_result$conf.low,
      "IC Superior" = fisher_result$conf.high
    )
  }
  
  # Intervalos de Confianza
  calcular_intervalos_confianza <- function(datos, variable) {
    datos %>%
      group_by(!!sym(variable)) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(
        p = n / sum(n),
        IC_Lower = p - qnorm(0.975) * sqrt((p * (1 - p)) / sum(n)),
        IC_Upper = p + qnorm(0.975) * sqrt((p * (1 - p)) / sum(n))
      )
  }
  
  # Render para Fisher
  output$fisher_resultados <- renderTable({
    req(input$variable_dependiente, input$variables_independientes, datos_completos())
    calcular_fisher(datos_completos(), input$variable_dependiente, input$variables_independientes)
  })
  
  # Render para Intervalos de Confianza
  output$intervalos_resultados <- renderTable({
    req(input$variable_dependiente, datos_completos())
    calcular_intervalos_confianza(datos_completos(), input$variable_dependiente)
  })
  
  # Evento para agregar resultados de Fisher
  observeEvent(input$agregar_a_lista, {
    req(input$variable_dependiente, input$variables_independientes, datos_completos())
    fisher_result <- calcular_fisher(datos_completos(), input$variable_dependiente, input$variables_independientes)
    if (is.null(fisher_result)) return()
    nuevo_resultado <- data.frame(
      "Variable Dependiente" = input$variable_dependiente,
      "Variable Independiente" = input$variables_independientes,
      fisher_result[2:5]
    )
    resultados_fisher_lista(rbind(resultados_fisher_lista(), nuevo_resultado))
  })
  
  # Evento para agregar resultados de Intervalos
  observeEvent(input$agregar_intervalos, {
    req(input$variable_dependiente, datos_completos())
    intervalos_result <- calcular_intervalos_confianza(datos_completos(), input$variable_dependiente)
    if (is.null(intervalos_result)) return()
    intervalos_result$Variable <- input$variable_dependiente
    resultados_intervalos_lista(rbind(resultados_intervalos_lista(), intervalos_result))
  })
  
  # Mostrar tabla dinámica según la pestaña activa
  tabla_activa <- reactive({
    if (input$agregar_a_lista || input$guardar_tabla) {
      resultados_fisher_lista()
    } else if (input$agregar_intervalos) {
      resultados_intervalos_lista()
    }
  })
  
  output$tabla_resultados <- renderTable({
    req(tabla_activa())
    tabla_activa()
  })
  
  output$guardar_tabla <- downloadHandler(
    filename = function() paste("resultados_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      write.csv(tabla_activa(), file, row.names = FALSE)
    }
  )
}
