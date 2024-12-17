#------------------------------------------------------
# Función para convertir coordenadas a grados decimales
#------------------------------------------------------
convertir_a_decimal <- function(coordenada) {
  if (is.na(coordenada) || coordenada == "") return(NA)
  
  partes <- as.numeric(strsplit(coordenada, "[°'\"]")[[1]])
  if (length(partes) < 2) return(NA)
  
  decimal <- partes[1] + (partes[2] / 60)
  if (grepl("S|W", coordenada)) return(-decimal)
  
  return(decimal)
}

#-------------------------------------------------------
# Función para limpiar coordenadas
#-------------------------------------------------------
limpiar_coordenadas <- function(datos, coord_vars) {
  stopifnot(all(coord_vars %in% names(datos)))
  
  datos <- datos %>%
    mutate(
      Coor_Lat = sapply(.data[[coord_vars[1]]], convertir_a_decimal),
      Coor_Long = sapply(.data[[coord_vars[2]]], convertir_a_decimal)
    ) %>%
    filter(!is.na(Coor_Lat) & !is.na(Coor_Long))
  
  return(datos)
}

#-------------------------------------------------------
# Función para crear datos relevantes
#-------------------------------------------------------
crear_datos_relevantes <- function(datos) {
  columnas_requeridas <- c("ID", "Municipio", "Coor_Lat", "Coor_Long", 
                           "FamDiag", "FamHosp", "Caso_6m", "ZancViv", 
                           "LarvViv", "H_Aeg", "M_Aeg", "H_albo", "M_albo")
  
  stopifnot(all(columnas_requeridas %in% names(datos)))
  
  datos <- datos %>%
    limpiar_coordenadas(c("Coor_Lat", "Coor_Long")) %>%
    mutate(Coor_Long = ifelse(Coor_Long > 0, -Coor_Long, Coor_Long),
           fam_entrev = 1) %>%
    select(all_of(columnas_requeridas), fam_entrev)
  
  return(datos)
}

#----------------------------------------------
# Función para crear el mapa interactivo
#----------------------------------------------
crear_mapa <- function(data, variable, leyenda, municipios_seleccionados = NULL) {
  data <- data %>%
    filter(Municipio %in% (municipios_seleccionados %||% unique(data$Municipio)),
           .data[[variable]] == 1)
  
  leaflet(data) %>%
    addTiles() %>%
    addMarkers(lng = ~Coor_Long, lat = ~Coor_Lat, 
               popup = ~paste("<strong>ID:</strong>", ID,
                              "<br><strong>Municipio:</strong>", Municipio,
                              "<br><strong>Latitud:</strong>", round(Coor_Lat, 3),
                              "<br><strong>Longitud:</strong>", round(Coor_Long, 3))) %>%
    addLegend(position = "bottomright", colors = "blue", labels = leyenda, title = "Leyenda") %>%
    addControl(html = paste("Municipios seleccionados:", 
                            paste(municipios_seleccionados, collapse = ", ")),
               position = "topright")
}


#----------------------------------------------
# Limpiar nombre del archivo
#----------------------------------------------
limpiar_nombre_archivo <- function(nombre) {
  # Remover acentos y caracteres especiales
  nombre <- iconv(nombre, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Reemplazar espacios por guiones bajos
  nombre <- gsub(" ", "_", nombre)
  # Eliminar caracteres no alfanuméricos ni guiones bajos
  nombre <- gsub("[^[:alnum:]_]", "", nombre)
  return(nombre)
}

#----------------------------------------------
# Función para guardar el mapa como HTML
#----------------------------------------------
guardar_mapa_html <- function(mapa, carpeta_guardado, nombre_archivo) {
  # Limpiar el nombre del archivo
  nombre_archivo <- limpiar_nombre_archivo(nombre_archivo)
  
  if (!dir.exists(carpeta_guardado)) {
    dir.create(carpeta_guardado, recursive = TRUE)  # Crear la carpeta si no existe
    cat("La carpeta '", carpeta_guardado, "' ha sido creada.\n", sep = "")
  }
  
  archivo_html <- file.path(carpeta_guardado, paste0(nombre_archivo, ".html"))
  htmlwidgets::saveWidget(mapa, archivo_html, selfcontained = TRUE)
  cat("Mapa guardado como HTML en: ", archivo_html, "\n")
  return(archivo_html)
}

#------------------------------------------------
# Función para guardar el mapa como PNG
#------------------------------------------------
guardar_mapa_png <- function(archivo_html, carpeta_guardado, nombre_archivo) {
  archivo_png <- file.path(carpeta_guardado, paste0(nombre_archivo, ".png"))
  if (!webshot::is_phantomjs_installed()) {
    webshot::install_phantomjs()
  }
  webshot::webshot(archivo_html, file = archivo_png)
  cat("Mapa guardado como PNG en: ", archivo_png, "\n")
  return(archivo_png)
}


#------------------------------------------------
# Funcion leyendas personalizadas mapas especies
#-------------------------------------------------
addLegendCustom <- function(map, variables_seleccionadas) {
  # Generar contenido de la leyenda basado en las variables seleccionadas
  legend_html <- paste0(
    "<div style='background-color: white; padding: 5px; border-radius: 5px; font-size: 12px;'>",
    "<strong>Leyenda</strong><br>",
    paste(
      lapply(variables_seleccionadas, function(variable) {
        if (grepl("H_", variable)) {
          # Hembras: círculo con borde amarillo
          paste0(
            "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
            "<div style='width: 15px; height: 15px; background-color: ",
            ifelse(variable == "H_Aeg", "red", "green"),
            "; border: 2px solid yellow; border-radius: 50%; margin-right: 5px;'></div>",
            variable,
            "</div>"
          )
        } else {
          # Machos: triángulos
          color <- ifelse(variable == "M_Aeg", "red", "green")
          paste0(
            "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
            "<div style='width: 0; height: 0; border-left: 7px solid transparent; ",
            "border-right: 7px solid transparent; border-bottom: 14px solid ", color, "; ",
            "margin-right: 5px;'></div>",
            variable,
            "</div>"
          )
        }
      }) %>% unlist() %>% paste(collapse = ""),
      "</div>"
    )
  )
  addControl(map, html = legend_html, position = "bottomright")
}

#-------------------------------------------------------
# Funcion para añadir leyenda municipio a mapas especie
#-------------------------------------------------------
addMunicipiosLegend <- function(map, municipios_seleccionados) {
  # Generar contenido de la leyenda para los municipios seleccionados
  legend_html <- paste0(
    "<div style='background-color: white; padding: 5px; border-radius: 5px; font-size: 12px;'>",
    "<strong>Municipios Seleccionados:</strong><br>",
    paste(municipios_seleccionados, collapse = "<br>"),
    "</div>"
  )
  addControl(map, html = legend_html, position = "bottomleft")
}

#-------------------------------------------------
# Funcion para generar mapas para especie
#-------------------------------------------------
generar_mapa_especies <- function(datos, variables_especies, municipios_seleccionados, variables_leyendas, crear_icono) {
  mapa <- leaflet() %>% addTiles()
  
  for (variable in variables_especies) {
    datos_variable <- datos %>%
      filter(!is.na(.data[[variable]]) & .data[[variable]] == 1 & Municipio %in% municipios_seleccionados)
    
    if (nrow(datos_variable) == 0) {
      cat("Advertencia: No hay datos para la variable:", variable, "\n")
      next
    }
    
    if (grepl("H_", variable)) {
      # Hembras
      mapa <- mapa %>%
        addCircleMarkers(
          lng = datos_variable$Coor_Long,
          lat = datos_variable$Coor_Lat,
          color = "yellow",
          fillColor = ifelse(variable == "H_Aeg", "red", "green"),
          fillOpacity = 0.6,
          radius = 8,
          stroke = TRUE,
          weight = 4,
          group = variable,
          popup = paste(
            "<strong>Variable:</strong>", variables_leyendas[[variable]],
            "<br><strong>Municipio:</strong>", datos_variable$Municipio
          )
        )
    } else {
      # Machos
      icono <- crear_icono(variable)
      mapa <- mapa %>%
        addMarkers(
          lng = datos_variable$Coor_Long,
          lat = datos_variable$Coor_Lat,
          icon = icono,
          group = variable,
          popup = paste(
            "<strong>Variable:</strong>", variables_leyendas[[variable]],
            "<br><strong>Municipio:</strong>", datos_variable$Municipio
          )
        )
    }
  }
  
  # Agregar leyendas
  mapa <- addLegendCustom(mapa, variables_especies)
  mapa <- addMunicipiosLegend(mapa, municipios_seleccionados)
  
  return(mapa)
}



