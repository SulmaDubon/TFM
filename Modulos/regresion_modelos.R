regresionModelosUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("variable_dependiente"),
                         "Seleccione variable dependiente:",
                         choices = NULL)
      ),
      column(3,
             selectizeInput(ns("variables_independientes"),
                            "Seleccione variables independientes:",
                            choices = NULL, multiple = TRUE)
      ),
      column(3,
             actionButton(ns("calcular_regresion"), "Calcular Modelo")
      )
    ),
    hr(),
    fluidRow(
      column(12, 
             h4("Resultados del Modelo de Regresión Logística"),
             tableOutput(ns("resultados_logistica"))
      )
    ),
    fluidRow(
      column(12, 
             downloadButton(ns("guardar_resultados"), "Guardar Resultados")
      )
    )
  )
}


regresionModelos <- function(input, output, session, datos_completos) {
  ns <- session$ns
  
  # Actualizar opciones en los selectores
  observe({
    req(datos_completos())
    updateSelectInput(session, "variable_dependiente", 
                      choices = names(datos_completos()))
    updateSelectizeInput(session, "variables_independientes", 
                         choices = names(datos_completos()))
  })
  
  # Modelo reactivo para almacenar los resultados
  modelo_logistico <- eventReactive(input$calcular_regresion, {
    req(input$variable_dependiente, input$variables_independientes, datos_completos())
    
    # Crear la fórmula dinámica
    formula <- as.formula(
      paste(input$variable_dependiente, "~", 
            paste(input$variables_independientes, collapse = " + "))
    )
    
    # Convertir variables independientes a factores si son categóricas
    datos <- datos_completos()
    for (var in input$variables_independientes) {
      if (is.character(datos[[var]]) || is.factor(datos[[var]])) {
        datos[[var]] <- as.factor(datos[[var]])
      }
    }
    
    # Calcular el modelo de regresión logística
    modelo <- glm(formula, data = datos, family = binomial)
    broom::tidy(modelo, exponentiate = TRUE, conf.int = TRUE)
  })
  
  # Mostrar resultados en la tabla
  output$resultados_logistica <- renderTable({
    req(modelo_logistico())
    resultados <- modelo_logistico()
    resultados <- resultados %>%
      select(term, estimate, conf.low, conf.high, p.value) %>%
      rename(
        "Variable" = term,
        "Odds Ratio" = estimate,
        "IC Inferior" = conf.low,
        "IC Superior" = conf.high,
        "Valor p" = p.value
      )
    return(resultados)
  })
  
  # Descargar los resultados
  output$guardar_resultados <- downloadHandler(
    filename = function() { paste("resultados_logisticos_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(modelo_logistico(), file, row.names = FALSE)
    }
  )
}

