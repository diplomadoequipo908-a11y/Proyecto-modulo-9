library(shiny)
library(bslib)
library(shinyjs)
library(leaflet)
library(plotly)

source("R/colors.R")
source("R/global_data.R")
source("R/helpers.R")
source("R/filters.R")
source("R/tabs/mapa.R")
source("R/tabs/comparacion_temporal.R")
source("R/tabs/clusters.R")


# ============================================================
# USER INTERFACE
# ============================================================

ui <- navbarPage(
  
  title = "Incidencia delictiva en CDMX",
  
  theme = bs_theme(
    primary = dark_blue,
    secondary = light_blue
  ),
  
  useShinyjs(),
  
  
  # ==========================================================
  # CUSTOM CSS
  # ==========================================================
  
  header = tagList(
    
    tags$head(
      
      tags$style(HTML(paste0("
        
        /* ==========================================
           GLOBAL
           ========================================== */
        
        body {
          overflow-x: hidden;
        }
        
        /* ==========================================
           FILTER SIDEBAR
           ========================================== */
        
        .filter-sidebar {
          position: fixed;
          top: 105px;
          left: 0;
          bottom: 0;
          width: 280px;
          padding: 20px;
          background-color: #f8f9fa;
          border-right: 1px solid #dee2e6;
          overflow-y: auto;
          z-index: 1000;
        }
        
        .filter-title {
          font-size: 1.2rem;
          font-weight: 600;
          margin-bottom: 20px;
          color: ", dark_blue, ";
        }
        
        /* ==========================================
           MAIN CONTENT
           ========================================== */
        
        .dashboard-content {
          margin-left: 280px;
          padding: 20px 25px 40px 25px;
        }
        
        /* ==========================================
           RESPONSIVE
           ========================================== */
        
        @media (max-width: 992px) {
          
          .filter-sidebar {
            position: relative;
            top: auto;
            bottom: auto;
            left: auto;
            width: 100%;
            height: auto;
            border-right: none;
            border-bottom: 1px solid #dee2e6;
          }
          
          .dashboard-content {
            margin-left: 0;
          }
          
          .kpi-row {
            flex-wrap: wrap;
          }
          
          .kpi-column {
            flex: 0 0 calc(50% - 8px);
          }
          
        }
        
        @media (max-width: 576px) {
          
          .kpi-column {
            flex: 0 0 100%;
          }
          
        }
        
      ")))
      
    ),
    
    
    # ========================================================
    # FILTER SIDEBAR
    # ========================================================
    
    div(
      class = "filter-sidebar",
      
      div(
        class = "filter-title",
        "Filtros"
      ),
      
      
      # ------------------------------------------------------
      # Geographic level
      # ------------------------------------------------------
      
      selectInput(
        "geo_level",
        "Nivel geográfico:",
        choices = c(
          "CDMX",
          "Alcaldía",
          "Sector",
          "Cuadrante"
        ),
        selected = "CDMX"
      ),
      
      
      # ------------------------------------------------------
      # Geographic selection
      # ------------------------------------------------------
      
      selectInput(
        "geo_selection",
        "Selección:",
        choices = NULL
      ),
      
      
      # ------------------------------------------------------
      # Temporal level
      # ------------------------------------------------------
      
      selectInput(
        "time_level",
        "Nivel temporal:",
        choices = c(
          "Año",
          "Semestre",
          "Trimestre",
          "Mes",
          "Día",
          "Personalizado"
        ),
        selected = "Año"
      ),
      
      
      # ------------------------------------------------------
      # Temporal selection
      # ------------------------------------------------------
      
      uiOutput(
        "time_selection_ui"
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # TABS
  # ==========================================================
  
  div(
    
    class = "dashboard-content",
    
    tabsetPanel(
      id = "main_tab",
      
      tabPanel(
        "Mapa",
        mapa_ui()
      ),
      
      tabPanel(
        "Comparación temporal",
        comparacion_temporal_ui()
      ),
      
      tabPanel(
        "Clusters por sector",
        clusters_ui()
      )
      
    )
    
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # FILTERS
  # ==========================================================
  
  filters_server(
    input = input,
    output = output,
    session = session
  )
  
  
  # ==========================================================
  # SHARED GEOGRAPHIC FILTER
  # ==========================================================

  carpetas_geografia <- reactive({

    req(
      input$geo_level,
      input$geo_selection
    )

    datos <- resumen_delitos

    if (
      input$geo_level == "Alcaldía"
    ) {

      datos <- datos |>
        filter(
          alcaldi == input$geo_selection
        )

    } else if (
      input$geo_level == "Sector"
    ) {

      sector_id_actual <- sector_lookup |>
        filter(
          sector == input$geo_selection
        ) |>
        pull(
          sector_id
        )

      datos <- datos |>
        filter(
          sector_id %in% sector_id_actual
        )

    } else if (
      input$geo_level == "Cuadrante"
    ) {

      datos <- datos |>
        filter(
          cuadrante_id == input$geo_selection
        )
    }

    datos

  })


  # ==========================================================
  # MAPA TAB
  # ==========================================================
  
  mapa_server(
    input = input,
    output = output,
    session = session,
    carpetas_geografia = carpetas_geografia
  )
  
  # ==========================================================
  # TEMPORAL COMPARISON TAB
  # ==========================================================
  
  comparacion_temporal_server(
    input = input,
    output = output,
    session = session,
    carpetas_geografia = carpetas_geografia
  )

  # ==========================================================
  # CLUSTERS TAB
  # ==========================================================
  
  clusters_server(
    input = input,
    output = output
  )
  
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)