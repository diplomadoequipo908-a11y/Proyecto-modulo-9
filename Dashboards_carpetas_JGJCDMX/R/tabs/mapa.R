# ============================================================
# MAPA TAB
# ============================================================


# ============================================================
# LOAD GEOGRAPHIC DATA
# ============================================================

alcaldias_mapa <- st_read(
  "data/alcaldias.gpkg",
  quiet = TRUE
)

sectores_mapa <- st_read(
  "data/sectores.gpkg",
  quiet = TRUE
)

cuadrantes_mapa <- st_read(
  "data/cuadranes.gpkg",
  quiet = TRUE
)

cdmx_mapa <- st_read(
  "data/CDMX.gpkg",
  quiet = TRUE
)


# ============================================================
# MAPA TAB UI
# ============================================================

mapa_ui <- function() {
  
  div(
    
    style = "
        display: flex;
        gap: 20px;
        align-items: flex-start;
        width: 100%;
      ",
    
    # ========================================================
    # MAP
    # ========================================================
    
    div(
      
      style = "
          width: 45%;
          flex-shrink: 0;
        ",
      
      h4(
        "Mapa de incidencia delictiva"
      ),
      
      div(
        style = "display: flex; gap: 10px; flex-wrap: wrap; align-items: flex-end; margin-bottom: 12px;",

        div(
          style = "flex: 1 1 30%; min-width: 150px;",
          selectInput(
            "mapa_crime_type",
            "Delito:",
            choices = categorias_dashboard,
            selected = "Total",
            width = "100%"
          )
        ),

        div(
          style = "flex: 1 1 30%; min-width: 170px;",
          selectInput(
            "mapa_value_type",
            "Valor:",
            choices = c(
              "Absoluto",
              "Por cada 100 k habitantes"
            ),
            selected = "Absoluto",
            width = "100%"
          )
        ),

        div(
          style = "flex: 1 1 30%; min-width: 170px;",
          selectInput(
            "map_level",
            "División del mapa:",
            choices = c(
              "Alcaldías" = "Alcaldía",
              "Sectores" = "Sector",
              "Cuadrantes" = "Cuadrante"
            ),
            selected = "Alcaldía",
            width = "100%"
          )
        )
      ),
      
      leafletOutput(
        "mapa",
        height = "610px"
      )
      
    ),
    
    # ========================================================
    # CHARTS
    # ========================================================
    
    div(
      
      style = "
          width: 55%;
          flex-shrink: 0;
        ",
      
      h4(
        "Distribución de delitos por categoría",
        style = "margin-top: 15px;"
      ),

      plotlyOutput(
        "distribucion_delitos",
        height = "240px"
      ),

      h4(
        "Top regiones",
        style = "margin-top: 15px;"
      ),
      
      sliderInput(
        "top_n",
        "Rango de regiones:",
        min = 1,
        max = 10,
        value = c(
          1,
          10
        ),
        step = 1,
        width = "100%"
      ),
      
      checkboxInput(
        "show_reference_line",
        "Mostrar línea de referencia",
        value = TRUE
      ),
      
      plotlyOutput(
        "ranking_regiones",
        height = "380px"
      )
      
    )
    
  )
  
}


# ============================================================
# MAPA TAB SERVER
# ============================================================

mapa_server <- function(
    input,
    output,
    session,
    carpetas_geografia
) {
  
  
  # ==========================================================
  # AVAILABLE MAP LEVELS
  # ==========================================================
  
  observeEvent(
    input$geo_level,
    {
      
      if (
        input$geo_level == "CDMX"
      ) {
        
        opciones <- c(
          "Alcaldías" = "Alcaldía",
          "Sectores" = "Sector",
          "Cuadrantes" = "Cuadrante"
        )
        
      } else if (
        input$geo_level == "Alcaldía"
      ) {
        
        opciones <- c(
          "Sectores" = "Sector",
          "Cuadrantes" = "Cuadrante"
        )
        
      } else if (
        input$geo_level == "Sector"
      ) {
        
        opciones <- c(
          "Cuadrantes" = "Cuadrante"
        )
        
      } else {
        
        opciones <- c(
          "Cuadrante" = "Cuadrante"
        )
        
      }
      
      updateSelectInput(
        session,
        "map_level",
        choices = opciones,
        selected = unname(
          opciones[1]
        )
      )
      
      if (
        length(opciones) == 1
      ) {
        disable(
          "map_level"
        )
      } else {
        enable(
          "map_level"
        )
      }
      
    },
    ignoreInit = FALSE
  )
  
  
  # ==========================================================
  # SINGLE-REGION SELECTION
  # ==========================================================

  region_seleccionada <- reactiveVal(NULL)


  # ==========================================================
  # RESET MAP SELECTION
  # Geography/map division changes clear the selected region.
  # Crime, value, and time filters do not.
  # ==========================================================

  observeEvent(
    list(
      input$geo_level,
      input$geo_selection,
      input$map_level
    ),
    {
      region_seleccionada(NULL)
    },
    ignoreInit = TRUE
  )


  # ==========================================================
  # TIME RANGE
  # Internal convention: [inicio, fin), so custom end dates
  # are converted to end + 1 day to make the user-facing range
  # inclusive: [start_date, end_date].
  # ==========================================================
  
  rango_temporal <- reactive({
    
    req(
      input$time_level,
      input$time_selection
    )
    
    if (
      input$time_level == "Año"
    ) {
      
      año <- as.integer(
        input$time_selection
      )
      
      inicio <- as.Date(
        paste0(
          año,
          "-01-01"
        )
      )
      
      fin <- as.Date(
        paste0(
          año + 1,
          "-01-01"
        )
      )
      
    } else if (
      input$time_level == "Semestre"
    ) {
      
      partes <- strsplit(
        input$time_selection,
        " - S"
      )[[1]]
      
      año <- as.integer(
        partes[1]
      )
      
      semestre <- as.integer(
        partes[2]
      )
      
      if (
        semestre == 1
      ) {
        inicio <- as.Date(
          paste0(
            año,
            "-01-01"
          )
        )
        
        fin <- as.Date(
          paste0(
            año,
            "-07-01"
          )
        )
        
      } else {
        inicio <- as.Date(
          paste0(
            año,
            "-07-01"
          )
        )
        
        fin <- as.Date(
          paste0(
            año + 1,
            "-01-01"
          )
        )
      }
      
    } else if (
      input$time_level == "Trimestre"
    ) {
      
      partes <- strsplit(
        input$time_selection,
        " - T"
      )[[1]]
      
      año <- as.integer(
        partes[1]
      )
      
      trimestre <- as.integer(
        partes[2]
      )
      
      mes_inicio <- (
        (trimestre - 1) * 3
      ) + 1
      
      inicio <- as.Date(
        paste0(
          año,
          "-",
          sprintf(
            "%02d",
            mes_inicio
          ),
          "-01"
        )
      )
      
      if (
        trimestre == 4
      ) {
        fin <- as.Date(
          paste0(
            año + 1,
            "-01-01"
          )
        )
      } else {
        mes_fin <- mes_inicio + 3
        fin <- as.Date(
          paste0(
            año,
            "-",
            sprintf(
              "%02d",
              mes_fin
            ),
            "-01"
          )
        )
      }
      
    } else if (
      input$time_level == "Mes"
    ) {
      
      fecha <- as.Date(
        input$time_selection
      )
      
      inicio <- as.Date(
        paste0(
          format(
            fecha,
            "%Y-%m"
          ),
          "-01"
        )
      )
      
      fin <- seq.Date(
        inicio,
        by = "month",
        length.out = 2
      )[2]
      
    } else if (
      input$time_level == "Día"
    ) {
      
      inicio <- as.Date(
        input$time_selection
      )
      
      fin <- inicio + 1
      
    } else if (
      input$time_level == "Personalizado"
    ) {
      
      rango <- as.Date(
        input$time_selection
      )
      
      req(
        length(rango) == 2,
        !any(is.na(rango)),
        rango[1] <= rango[2]
      )
      
      inicio <- rango[1]
      fin <- rango[2] + 1
      
    }
    
    list(
      inicio = inicio,
      fin = fin
    )
    
  })
  
  
  # ==========================================================
  # CRIME DATA FOR MAP
  # ==========================================================
  
  datos_mapa <- reactive({
    
    datos <- carpetas_geografia()
    rango <- rango_temporal()
    
    fechas <- as.Date(
      datos$fecha_hecho
    )
    
    datos <- datos[
      !is.na(fechas) &
        fechas >= rango$inicio &
        fechas < rango$fin,
      ,
      drop = FALSE
    ]
    
    crime_col <- columna_delito(input$mapa_crime_type)

    datos <- datos |>
      mutate(
        delitos = as.numeric(.data[[crime_col]])
      )

    datos
    
  })
  
  
  # ==========================================================
  # MAP REGIONS
  # ==========================================================
  
  regiones_mapa <- reactive({
    
    req(
      input$geo_level,
      input$geo_selection,
      input$map_level
    )
    
    if (
      input$map_level == "Alcaldía"
    ) {
      
      resultado <- alcaldias_mapa |>
        mutate(
          subregion_id = alcaldi,
          subregion_name = NOMGEO
        )
      
    } else if (
      input$map_level == "Sector"
    ) {
      
      resultado <- sectores_mapa |>
        left_join(
          alcaldias_mapa |>
            st_drop_geometry() |>
            select(
              alcaldi,
              NOMGEO
            ),
          by = "alcaldi"
        ) |>
        mutate(
          subregion_id = sector_id,
          subregion_name = sector
        )
      
    } else {
      
      resultado <- cuadrantes_mapa |>
        left_join(
          alcaldias_mapa |>
            st_drop_geometry() |>
            select(
              alcaldi,
              NOMGEO
            ),
          by = "alcaldi"
        ) |>
        mutate(
          subregion_id = cuadrante_id,
          subregion_name = cuadrante_id
        )
      
    }
    
    if (
      input$geo_level == "CDMX"
    ) {
      return(resultado)
    }
    
    if (
      input$geo_level == "Alcaldía"
    ) {
      return(
        resultado |>
          filter(
            alcaldi == input$geo_selection
          )
      )
    }
    
    if (
      input$geo_level == "Sector"
    ) {
      
      sector_id_actual <- sector_lookup |>
        filter(
          sector == input$geo_selection
        ) |>
        pull(
          sector_id
        )
      
      return(
        resultado |>
          filter(
            sector_id %in% sector_id_actual
          )
      )
    }
    
    resultado |>
      filter(
        cuadrante_id == input$geo_selection
      )
    
  })
  
  
  # ==========================================================
  # UPDATE DOUBLE SLIDER
  # ==========================================================
  
  observeEvent(
    regiones_mapa(),
    {
      
      numero_regiones <- nrow(
        regiones_mapa()
      )
      
      maximo <- max(
        1,
        numero_regiones
      )
      
      updateSliderInput(
        session,
        "top_n",
        min = 1,
        max = maximo,
        value = c(
          1,
          min(
            10,
            maximo
          )
        ),
        step = 1
      )
      
      if (
        maximo == 1
      ) {
        disable("top_n")
      } else {
        enable("top_n")
      }
      
    },
    ignoreInit = TRUE
  )
  
  
  # ==========================================================
  # POPULATION FOR MAP REGIONS
  # ==========================================================
  
  poblacion_regiones_mapa <- reactive({
    
    regiones <- regiones_mapa()
    
    if (
      input$map_level == "Alcaldía"
    ) {
      
      regiones <- regiones |>
        left_join(
          pob_alcaldia |>
            select(
              alcaldi,
              POBTOT
            ),
          by = "alcaldi"
        ) |>
        mutate(
          poblacion = POBTOT
        )
      
    } else if (
      input$map_level == "Sector"
    ) {
      
      regiones <- regiones |>
        left_join(
          pob_sector |>
            select(
              sector_id,
              POBTOT
            ),
          by = "sector_id"
        ) |>
        mutate(
          poblacion = POBTOT
        )
      
    } else {
      
      regiones <- regiones |>
        left_join(
          pob_cuadrante |>
            select(
              cuadrante_id,
              POBTOT
            ),
          by = "cuadrante_id"
        ) |>
        mutate(
          poblacion = ifelse(
            is.na(POBTOT) |
              POBTOT <= 0,
            min_pob_cuadrante,
            POBTOT
          )
        )
      
    }
    
    regiones
    
  })
  
  
  # ==========================================================
  # MAP DATA
  # ==========================================================
  
  datos_para_mapa <- reactive({
    
    regiones <- poblacion_regiones_mapa()
    datos <- datos_mapa()
    datos_tabular <- datos
    
    if (
      input$map_level == "Alcaldía"
    ) {
      
      conteos <- datos_tabular |>
        group_by(alcaldi) |>
        summarise(
          delitos = sum(delitos, na.rm = TRUE),
          .groups = "drop"
        ) |>
        rename(
          subregion_id = alcaldi
        )
      
    } else if (
      input$map_level == "Sector"
    ) {
      
      conteos <- datos_tabular |>
        group_by(sector_id) |>
        summarise(
          delitos = sum(delitos, na.rm = TRUE),
          .groups = "drop"
        ) |>
        rename(
          subregion_id = sector_id
        )
      
    } else {
      
      conteos <- datos_tabular |>
        group_by(cuadrante_id) |>
        summarise(
          delitos = sum(delitos, na.rm = TRUE),
          .groups = "drop"
        ) |>
        rename(
          subregion_id = cuadrante_id
        )
      
    }
    
    resultado <- regiones |>
      left_join(
        conteos,
        by = "subregion_id"
      ) |>
      mutate(
        delitos = ifelse(
          is.na(delitos),
          0,
          delitos
        ),
        tasa = ifelse(
          !is.na(poblacion) &
            poblacion > 0,
          (
            delitos /
              poblacion
          ) * 100000,
          NA_real_
        ),
        valor = if (
          input$mapa_value_type == "Por cada 100 k habitantes"
        ) tasa else delitos,
        delitos_label = format(
          delitos,
          big.mark = ",",
          scientific = FALSE
        ),
        tasa_label = ifelse(
          is.na(tasa),
          "NA",
          format(
            round(
              tasa,
              1
            ),
            nsmall = 1,
            big.mark = ",",
            scientific = FALSE
          )
        ),
        poblacion_label = ifelse(
          is.na(poblacion),
          "NA",
          format(
            poblacion,
            big.mark = ",",
            scientific = FALSE
          )
        )
      )
    
    # ----------------------------------------------------------
    # TOOLTIP
    # ----------------------------------------------------------
    
    categoria_texto <- if (
      input$mapa_crime_type == "Total"
    ) {
      ""
    } else {
      paste0(
        "<br><br><b>",
        input$mapa_crime_type,
        "</b>"
      )
    }
    
    if (
      input$map_level == "Alcaldía"
    ) {
      
      resultado <- resultado |>
        mutate(
          tooltip = paste0(
            "<b>", subregion_name, "</b>",
            categoria_texto,
            "<br><br>",
            "<b>Delitos: ", delitos_label, "</b>",
            "<br>",
            "<b>Por cada 100 mil habitantes: ", tasa_label, "</b>",
            "<br><br>",
            "Población: ", poblacion_label
          )
        )
      
    } else if (
      input$map_level == "Sector"
    ) {
      
      resultado <- resultado |>
        mutate(
          tooltip = paste0(
            "<b>", subregion_name, "</b>",
            "<br>",
            "Alcaldía: ", NOMGEO,
            categoria_texto,
            "<br><br>",
            "<b>Delitos: ", delitos_label, "</b>",
            "<br>",
            "<b>Por cada 100 mil habitantes: ", tasa_label, "</b>",
            "<br><br>",
            "Población: ", poblacion_label
          )
        )
      
    } else {
      
      resultado <- resultado |>
        mutate(
          tooltip = paste0(
            "<b>", subregion_name, "</b>",
            "<br>",
            "Alcaldía: ", NOMGEO,
            "<br>",
            "Sector: ", sector,
            categoria_texto,
            "<br><br>",
            "<b>Delitos: ", delitos_label, "</b>",
            "<br>",
            "<b>Por cada 100 mil habitantes: ", tasa_label, "</b>",
            "<br><br>",
            "Población: ", poblacion_label
          )
        )
      
    }
    
    resultado
    
  })
  
  
  # ==========================================================
  # MAIN REGION GEOMETRY
  # ==========================================================
  
  geometria_principal <- reactive({
    
    if (
      input$geo_level == "CDMX"
    ) {
      
      cdmx_mapa
      
    } else if (
      input$geo_level == "Alcaldía"
    ) {
      
      alcaldias_mapa |>
        filter(
          alcaldi == input$geo_selection
        )
      
    } else if (
      input$geo_level == "Sector"
    ) {
      
      sectores_mapa |>
        filter(
          sector == input$geo_selection
        )
      
    } else {
      
      cuadrantes_mapa |>
        filter(
          cuadrante_id == input$geo_selection
        )
      
    }
    
  })
  
  
  # ==========================================================
  # MAP
  # ==========================================================
  
  output$mapa <- renderLeaflet({

    datos <- datos_para_mapa()
    principal <- geometria_principal()

    datos <- st_transform(datos, 4326)
    principal <- st_transform(principal, 4326)

    datos$map_layer_id <- as.character(datos$subregion_id)

    valores_validos <- datos$valor[is.finite(datos$valor)]

    if (length(valores_validos) == 0) {
      dominio <- c(0, 1)
    } else {
      dominio <- range(valores_validos, na.rm = TRUE)
      if (dominio[1] == dominio[2]) {
        dominio <- c(dominio[1], dominio[1] + 1)
      }
    }

    pal <- colorNumeric(
      palette = choropleth_red_palette,
      domain = dominio,
      na.color = gray
    )

    selected_id <- region_seleccionada()
    if (is.null(selected_id)) selected_id <- ""

    datos$border_color <- ifelse(
      datos$map_layer_id == selected_id,
      gold,
      "#ffffff"
    )
    datos$border_weight <- ifelse(
      datos$map_layer_id == selected_id,
      3,
      1
    )

    titulo_leyenda <- if (
      input$mapa_value_type == "Por cada 100 k habitantes"
    ) {
      "Por cada 100 mil habitantes"
    } else {
      "Número de delitos"
    }

    leaflet() |>
      addPolygons(
        data = principal,
        fill = TRUE,
        fillColor = "#ffffff",
        fillOpacity = 0.02,
        color = dark_blue,
        weight = 3,
        opacity = 1,
        layerId = "__parent__"
      ) |>
      addPolygons(
        data = datos,
        fillColor = ~pal(valor),
        fillOpacity = 0.75,
        color = ~border_color,
        weight = ~border_weight,
        opacity = 1,
        layerId = ~map_layer_id,
        label = lapply(datos$tooltip, htmltools::HTML),
        labelOptions = labelOptions(direction = "auto"),
        highlightOptions = highlightOptions(
          weight = 3,
          color = dark_blue,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = unname(datos$valor),
        title = titulo_leyenda,
        opacity = 0.8
      )
  })


  # ==========================================================
  # MAP SHAPE CLICK / SINGLE SELECTED REGION
  # ==========================================================
  
  observeEvent(
    input$mapa_shape_click,
    {
      click <- input$mapa_shape_click
      req(click$id)

      if (click$id == "__parent__") return()

      id_click <- trimws(
        sub("^click_", "", as.character(click$id))
      )

      regiones <- datos_para_mapa() |>
        st_drop_geometry() |>
        mutate(map_layer_id = trimws(as.character(subregion_id)))

      region_clic <- regiones |>
        filter(map_layer_id == .env$id_click) |>
        slice_head(n = 1)

      if (nrow(region_clic) == 0) return()

      if (identical(region_seleccionada(), id_click)) {
        region_seleccionada(NULL)
      } else {
        region_seleccionada(id_click)
      }
    },
    ignoreInit = TRUE
  )


  # ==========================================================
  # REGION USED BY LINE CHART
  # ==========================================================
  
  titulo_global <- reactive({
    
    if (
      input$geo_level == "CDMX"
    ) {
      return("CDMX")
    }
    
    if (
      input$geo_level == "Alcaldía"
    ) {
      
      nombre <- alcaldias_mapa |>
        filter(
          alcaldi == input$geo_selection
        ) |>
        pull(
          NOMGEO
        )
      
      return(
        nombre[1]
      )
    }
    
    input$geo_selection
    
  })
  
  
  # ==========================================================
  # CRIME COMPOSITION BAR CHART (LOG SCALE)
  # ==========================================================

  output$distribucion_delitos <- renderPlotly({

    req(input$time_selection)

    rango <- rango_temporal()

    datos <- carpetas_geografia() |>
      filter(
        !is.na(fecha_hecho),
        fecha_hecho >= rango$inicio,
        fecha_hecho < rango$fin
      )

    id_seleccionado <- region_seleccionada()

    if (!is.null(id_seleccionado)) {
      if (input$map_level == "Alcaldía") {
        datos <- datos |> filter(alcaldi == id_seleccionado)
      } else if (input$map_level == "Sector") {
        datos <- datos |> filter(sector_id == id_seleccionado)
      } else {
        datos <- datos |> filter(cuadrante_id == id_seleccionado)
      }
    }

    if (nrow(datos) == 0) return(NULL)

    composicion <- datos |>
      summarise(
        across(
          all_of(delitos_disponibles),
          ~ sum(.x, na.rm = TRUE)
        )
      ) |>
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "categoria_delito",
        values_to = "delitos"
      ) |>
      filter(delitos > 0) |>
      arrange(desc(delitos))

    # Calculate the percentage explicitly after sorting so each
    # category receives its own share of the total.
    total_composicion <- sum(
      composicion$delitos,
      na.rm = TRUE
    )

    composicion <- composicion |>
      mutate(
        porcentaje = if (
          total_composicion > 0
        ) {
          delitos / total_composicion * 100
        } else {
          0
        }
      )

    if (nrow(composicion) == 0) return(NULL)

    # Keep the exact descending order in Plotly.
    orden_categorias <- composicion$categoria_delito

    plot_ly(
      data = composicion,
      x = ~categoria_delito,
      y = ~delitos,
      type = "bar",
      marker = list(
        color = red
      ),
      text = ~paste0(round(porcentaje, 1), "%"),
      textposition = "outside",
      cliponaxis = FALSE,
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Delitos: %{y:,.0f}<br>",
        "Participación: %{text}<extra></extra>"
      )
    ) |>
      layout(
        xaxis = list(
          title = NULL,
          tickangle = -45,
          categoryorder = "array",
          categoryarray = orden_categorias
        ),
        yaxis = list(
          title = "Número de delitos (escala logarítmica)",
          type = "log"
        ),
        margin = list(
          t = 50,
          r = 20,
          b = 120,
          l = 70
        )
      )
  })


  # ==========================================================
  # TOP N / RANGE BAR CHART
  # ==========================================================
  
  output$ranking_regiones <- renderPlotly({
    
    datos <- datos_para_mapa()
    req(
      nrow(datos) > 0
    )
    
    ranking <- datos |>
      select(
        subregion_id,
        subregion_name,
        delitos,
        poblacion,
        tasa,
        valor
      ) |>
      st_drop_geometry() |>
      mutate(
        subregion_id = as.character(subregion_id)
      ) |>
      arrange(
        desc(valor),
        subregion_name
      ) |>
      mutate(
        ranking = row_number()
      )
    
    rango <- if (
      is.null(input$top_n)
    ) {
      c(
        1,
        min(
          10,
          nrow(ranking)
        )
      )
    } else {
      sort(
        as.integer(input$top_n)
      )
    }
    
    rango[1] <- max(
      1,
      min(
        rango[1],
        nrow(ranking)
      )
    )
    
    rango[2] <- max(
      rango[1],
      min(
        rango[2],
        nrow(ranking)
      )
    )
    
    ranking_top <- ranking |>
      filter(
        ranking >= rango[1] &
          ranking <= rango[2]
      )
    
    id_seleccionado <- region_seleccionada()

    colores_barras <- ifelse(
      !is.null(id_seleccionado) &
        ranking_top$subregion_id == id_seleccionado,
      gold,
      red
    )
    
    # ----------------------------------------------------------
    # GLOBAL REFERENCE VALUE FOR THE BAR CHART
    # ----------------------------------------------------------
    
    if (
      input$mapa_value_type == "Por cada 100 k habitantes"
    ) {
      
      datos_globales <- carpetas_geografia()
      crime_col <- columna_delito(input$mapa_crime_type)
      datos_globales <- datos_globales |>
        mutate(
          delitos = as.numeric(.data[[crime_col]])
        )
      rango_global <- rango_temporal()
      fechas_global <- as.Date(
        datos_globales$fecha_hecho
      )
      
      datos_globales <- datos_globales[
        !is.na(fechas_global) &
          fechas_global >= rango_global$inicio &
          fechas_global < rango_global$fin,
        ,
        drop = FALSE
      ]
      
      # The crime selection is already represented in `delitos`.
      
      delitos_global <- sum(
        datos_globales$delitos,
        na.rm = TRUE
      )
      
      poblacion_global <- poblacion_actual(
        nivel_geografico = input$geo_level,
        seleccion = input$geo_selection
      )
      
      referencia <- if (
        !is.na(poblacion_global) &&
        poblacion_global > 0
      ) {
        (
          delitos_global /
            poblacion_global
        ) * 100000
      } else {
        NA_real_
      }
      
      titulo_referencia <- "Territorio global"
      
    } else {
      
      referencia <- mean(
        ranking$delitos,
        na.rm = TRUE
      )
      
      titulo_referencia <- "Promedio por región"
      
    }
    
    grafica <- plot_ly(
      data = ranking_top,
      x = ~valor,
      y = ~subregion_name,
      type = "bar",
      orientation = "h",
      marker = list(
        color = colores_barras
      ),
      hovertemplate = if (
        input$mapa_value_type == "Absoluto"
      ) {
        paste0(
          "<b>%{y}</b>",
          "<br>",
          "Delitos: %{x:,.0f}",
          "<extra></extra>"
        )
      } else {
        paste0(
          "<b>%{y}</b>",
          "<br>",
          "Delitos por cada 100 mil: %{x:.1f}",
          "<extra></extra>"
        )
      }
    ) |>
      layout(
        xaxis = list(
          title = if (
            input$mapa_value_type == "Por cada 100 k habitantes"
          ) {
            "Delitos por cada 100 mil habitantes"
          } else {
            "Número de delitos"
          },
          zeroline = TRUE
        ),
        yaxis = list(
          title = "",
          categoryorder = "total ascending"
        ),
        margin = list(
          l = 110,
          r = 80,
          t = 70,
          b = 40
        )
      )
    
    # ----------------------------------------------------------
    # BAR CHART REFERENCE LINE
    # ----------------------------------------------------------
    
    if (
      isTRUE(input$show_reference_line) &&
      is.finite(referencia)
    ) {
      
      etiqueta_referencia <- if (
        input$mapa_value_type == "Por cada 100 k habitantes"
      ) {
        paste0(
          titulo_referencia,
          ": ",
          format(
            round(
              referencia,
              1
            ),
            nsmall = 1,
            big.mark = ","
          )
        )
      } else {
        paste0(
          titulo_referencia,
          ": ",
          format(
            round(
              referencia,
              0
            ),
            big.mark = ",",
            scientific = FALSE
          )
        )
      }
      
      grafica <- grafica |>
        layout(
          shapes = list(
            list(
              type = "line",
              xref = "x",
              yref = "paper",
              x0 = referencia,
              x1 = referencia,
              y0 = 0,
              y1 = 1,
              line = list(
                color = black,
                width = 2,
                dash = "dash"
              )
            )
          ),
          annotations = list(
            list(
              x = referencia,
              xref = "x",
              y = 1,
              yref = "paper",
              text = etiqueta_referencia,
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "bottom",
              font = list(
                size = 10
              )
            )
          )
        )
    }
    
    # ----------------------------------------------------------
    # TITLES
    # ----------------------------------------------------------
    
    nombre_division <- switch(
      input$map_level,
      "Alcaldía" = "Alcaldías",
      "Sector" = "Sectores",
      "Cuadrante" = "Cuadrantes"
    )
    
    grafica |>
      layout(
        title = list(
          text = paste(
            "Regiones",
            rango[1],
            "a",
            rango[2],
            "-",
            nombre_division,
            "-",
            titulo_global()
          ),
          y = 0.98
        )
      )
    
  })
  
  
  # ==========================================================
  # KEEP RANKING OUTPUT ACTIVE
  # ==========================================================
  
  outputOptions(
    output,
    "ranking_regiones",
    suspendWhenHidden = FALSE
  )
  
}