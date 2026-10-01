# ============================================================
# FILTER LOGIC
# ============================================================

filters_server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # GEOGRAPHIC SELECTION
  # ==========================================================
  
  observeEvent(
    input$geo_level,
    {
      
      if (
        input$geo_level == "CDMX"
      ) {
        
        updateSelectInput(
          session,
          "geo_selection",
          choices = "CDMX",
          selected = "CDMX"
        )
        
        disable(
          "geo_selection"
        )
        
      } else if (
        input$geo_level == "Alcaldía"
      ) {
        
        valores <- sort(
          unique(
            cuadrantes$alcaldi
          )
        )
        
        updateSelectInput(
          session,
          "geo_selection",
          choices = valores,
          selected = valores[1]
        )
        
        enable(
          "geo_selection"
        )
        
      } else if (
        input$geo_level == "Sector"
      ) {
        
        valores <- sort(
          unique(
            cuadrantes$sector
          )
        )
        
        updateSelectInput(
          session,
          "geo_selection",
          choices = valores,
          selected = valores[1]
        )
        
        enable(
          "geo_selection"
        )
        
      } else if (
        input$geo_level == "Cuadrante"
      ) {
        
        valores <- sort(
          unique(
            cuadrantes$cuadrante_id
          )
        )
        
        updateSelectInput(
          session,
          "geo_selection",
          choices = valores,
          selected = valores[1]
        )
        
        enable(
          "geo_selection"
        )
        
      }
      
    },
    ignoreInit = FALSE
  )
  
  
  # ==========================================================
  # TAB-SPECIFIC GLOBAL FILTERS
  # ==========================================================
  # Temporal comparison uses only geography + the custom date range.
  # Its crime selector is local to the tab.
  
  observeEvent(
    input$main_tab,
    {
      
      if (
        input$main_tab == "Comparación temporal"
      ) {
        
        updateSelectInput(
          session,
          "time_level",
          selected = "Personalizado"
        )
        
        disable("time_level")
        
        enable("geo_level")
        
        session$onFlushed(
          function() {
            updateDateRangeInput(
              session,
              "time_selection",
              start = fecha_inicio,
              end = fecha_fin,
              min = fecha_inicio,
              max = fecha_fin
            )
          },
          once = TRUE
        )
        
      } else if (
        input$main_tab == "Clusters por sector"
      ) {
        
        updateSelectInput(
          session,
          "geo_level",
          selected = "CDMX"
        )
        
        updateSelectInput(
          session,
          "geo_selection",
          choices = "CDMX",
          selected = "CDMX"
        )
        
        disable("geo_level")
        disable("geo_selection")
        enable("time_level")
        
      } else {
        
        enable("geo_level")
        enable("time_level")
        
      }
    },
    ignoreInit = FALSE
  )
  
  
  # ==========================================================
  # TEMPORAL SELECTION UI
  # ==========================================================
  
  output$time_selection_ui <- renderUI({
    
    req(
      input$time_level
    )
    
    fechas <- resumen_delitos$fecha_hecho
    
    # --------------------------------------------------------
    # YEAR
    # Only fully completed years are selectable.
    # --------------------------------------------------------
    
    if (
      input$time_level == "Año"
    ) {
      
      años <- sort(
        unique(
          as.integer(
            format(
              fechas,
              "%Y"
            )
          )
        )
      )
      años <- años[!is.na(años)]
      
      años_completos <- años[
        as.Date(
          paste0(
            años,
            "-12-31"
          )
        ) <= fecha_fin
      ]
      
      req(
        length(años_completos) > 0
      )
      
      selectInput(
        "time_selection",
        "Selección:",
        choices = años_completos,
        selected = if (2024 %in% años_completos) {
          2024
        } else {
          max(
            años_completos
          )
        }
      )
      
      
      # --------------------------------------------------------
      # SEMESTER
      # Only fully completed semesters are selectable.
      # --------------------------------------------------------
      
    } else if (
      input$time_level == "Semestre"
    ) {
      
      años <- sort(
        unique(
          as.integer(
            format(
              fechas,
              "%Y"
            )
          )
        )
      )
      años <- años[!is.na(años)]
      
      semestres_df <- do.call(
        rbind,
        lapply(
          años,
          function(año) {
            data.frame(
              label = c(
                paste0(año, " - S1"),
                paste0(año, " - S2")
              ),
              fin = as.Date(
                c(
                  paste0(año, "-06-30"),
                  paste0(año, "-12-31")
                )
              ),
              stringsAsFactors = FALSE
            )
          }
        )
      )
      
      semestres_df <- semestres_df[
        semestres_df$fin <= fecha_fin,
        ,
        drop = FALSE
      ]
      
      req(
        nrow(semestres_df) > 0
      )
      
      choices <- semestres_df$label
      
      selectInput(
        "time_selection",
        "Selección:",
        choices = choices,
        selected = tail(
          choices,
          1
        )
      )
      
      
      # --------------------------------------------------------
      # QUARTER
      # Only fully completed quarters are selectable.
      # --------------------------------------------------------
      
    } else if (
      input$time_level == "Trimestre"
    ) {
      
      años <- sort(
        unique(
          as.integer(
            format(
              fechas,
              "%Y"
            )
          )
        )
      )
      años <- años[!is.na(años)]
      
      trimestres_df <- do.call(
        rbind,
        lapply(
          años,
          function(año) {
            data.frame(
              label = paste0(
                año,
                " - T",
                1:4
              ),
              fin = as.Date(
                c(
                  paste0(año, "-03-31"),
                  paste0(año, "-06-30"),
                  paste0(año, "-09-30"),
                  paste0(año, "-12-31")
                )
              ),
              stringsAsFactors = FALSE
            )
          }
        )
      )
      
      trimestres_df <- trimestres_df[
        trimestres_df$fin <= fecha_fin,
        ,
        drop = FALSE
      ]
      
      req(
        nrow(trimestres_df) > 0
      )
      
      choices <- trimestres_df$label
      
      selectInput(
        "time_selection",
        "Selección:",
        choices = choices,
        selected = tail(
          choices,
          1
        )
      )
      
      
      # --------------------------------------------------------
      # MONTH
      # Only fully completed months are selectable.
      # --------------------------------------------------------
      
    } else if (
      input$time_level == "Mes"
    ) {
      
      primer_mes <- as.Date(
        format(
          fecha_inicio,
          "%Y-%m-01"
        )
      )
      
      ultimo_mes <- as.Date(
        format(
          fecha_fin,
          "%Y-%m-01"
        )
      )
      
      meses <- seq.Date(
        primer_mes,
        ultimo_mes,
        by = "month"
      )
      
      siguiente_mes <- seq.Date(
        ultimo_mes,
        by = "month",
        length.out = 2
      )[2]
      
      inicio_siguiente_mes <- c(
        meses[-1],
        siguiente_mes
      )
      
      fin_mes <- inicio_siguiente_mes - 1
      
      meses_completos <- meses[
        fin_mes <= fecha_fin
      ]
      
      req(
        length(meses_completos) > 0
      )
      
      valores <- as.character(
        meses_completos
      )
      
      nombres <- format(
        meses_completos,
        "%m/%Y"
      )
      
      selectInput(
        "time_selection",
        "Selección:",
        choices = setNames(
          valores,
          nombres
        ),
        selected = tail(
          valores,
          1
        )
      )
      
      
      # --------------------------------------------------------
      # DAY
      # All dates up to and including the latest available date.
      # --------------------------------------------------------
      
    } else if (
      input$time_level == "Día"
    ) {
      
      dateInput(
        "time_selection",
        "Selección:",
        value = fecha_fin,
        min = fecha_inicio,
        max = fecha_fin,
        format = "dd/mm/yyyy",
        language = "es"
      )
      
      
      # --------------------------------------------------------
      # CUSTOM
      # Inclusive range [start_date, end_date].
      # --------------------------------------------------------
      
    } else if (
      input$time_level == "Personalizado"
    ) {
      
      dateRangeInput(
        "time_selection",
        "Selección:",
        start = fecha_inicio,
        end = fecha_fin,
        min = fecha_inicio,
        max = fecha_fin,
        format = "dd/mm/yyyy",
        language = "es",
        separator = " a "
      )
      
    }
    
  })
  
}
