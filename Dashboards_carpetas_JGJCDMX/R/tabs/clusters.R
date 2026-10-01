# ============================================================
# CLUSTERS TAB
# ============================================================

# Two sector classifications are supplied by the user:
#   - sectores_cluster_delitos.csv
#   - sectores_cluster_dias.csv
# The tab lets the user switch between them.

cluster_delitos <- read.csv(
  "data/sectores_cluster_delitos.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

cluster_dias <- read.csv(
  "data/sectores_cluster_dias.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

validar_clusters <- function(
    datos,
    nombre_archivo
) {

  req_cols <- c(
    "sector_id",
    "cluster"
  )

  if (!all(req_cols %in% names(datos))) {
    stop(
      paste0(
        nombre_archivo,
        " debe contener las columnas: sector_id y cluster."
      )
    )
  }

  datos |>
    dplyr::transmute(
      sector_id = trimws(as.character(sector_id)),
      cluster = trimws(as.character(cluster))
    ) |>
    dplyr::filter(
      !is.na(sector_id),
      sector_id != "",
      !is.na(cluster),
      cluster != ""
    ) |>
    dplyr::distinct(
      sector_id,
      .keep_all = TRUE
    )
}

cluster_delitos <- validar_clusters(
  cluster_delitos,
  "sectores_cluster_delitos.csv"
)

cluster_dias <- validar_clusters(
  cluster_dias,
  "sectores_cluster_dias.csv"
)

clusters_disponibles <- sort(
  unique(
    c(
      cluster_delitos$cluster,
      cluster_dias$cluster
    )
  )
)

if (length(clusters_disponibles) != 4) {
  warning(
    paste0(
      "Se esperaban 4 clusters y se encontraron ",
      length(clusters_disponibles),
      "."
    )
  )
}

cluster_label_default <- function(x) {
  sub(
    "_",
    " ",
    x,
    fixed = TRUE
  )
}

# Display names for each source classification.
cluster_labels_delitos <- c(
  "Cluster_1" = "Incidencia alta, robo patrimonial moderado",
  "Cluster_2" = "Incidencia alta, especialmente robo patrimonial",
  "Cluster_3" = "Incidencia promedio",
  "Cluster_4" = "Incidencia baja"
)

cluster_labels_dias <- c(
  "Cluster_1" = "Incidencia muy alta, especialmente en transporte y vía pública",
  "Cluster_2" = "Incidencia promedio, más alta en transporte público",
  "Cluster_3" = "Incidencia promedio",
  "Cluster_4" = "Incidencia alta, especialmente en transporte y vía pública"
)

cluster_labels <- function(x, source) {
  mapa <- if (identical(source, "dias")) {
    cluster_labels_dias
  } else {
    cluster_labels_delitos
  }

  out <- unname(mapa[as.character(x)])
  faltan <- is.na(out)
  if (any(faltan)) {
    out[faltan] <- cluster_label_default(x[faltan])
  }

  out
}

# Colors communicate the incidence profile of each cluster.
# The assignments differ by classification because the meaning of
# each cluster number differs between the two source files.
cluster_colors_delitos <- c(
  "Cluster_1" = gold,
  "Cluster_2" = red,
  "Cluster_3" = light_blue,
  "Cluster_4" = green
)

cluster_colors_dias <- c(
  "Cluster_1" = red,
  "Cluster_2" = gold,
  "Cluster_3" = light_blue,
  "Cluster_4" = orange
)

cluster_colors_for_source <- function(source) {
  if (identical(source, "dias")) {
    return(cluster_colors_dias)
  }

  cluster_colors_delitos
}

# ------------------------------------------------------------
# TIME RANGE HELPER
# ------------------------------------------------------------

rango_temporal_clusters <- function(input) {

  req(
    input$time_level,
    input$time_selection
  )

  if (input$time_level == "Personalizado") {

    rango <- as.Date(
      input$time_selection
    )

    req(
      length(rango) == 2,
      !any(is.na(rango)),
      rango[1] <= rango[2]
    )

    return(
      list(
        inicio = rango[1],
        fin = rango[2]
      )
    )
  }

  if (input$time_level == "Año") {

    año <- as.integer(
      input$time_selection
    )

    return(
      list(
        inicio = as.Date(
          paste0(año, "-01-01")
        ),
        fin = as.Date(
          paste0(año, "-12-31")
        )
      )
    )
  }

  if (input$time_level == "Semestre") {

    partes <- strsplit(
      input$time_selection,
      " - S",
      fixed = TRUE
    )[[1]]

    año <- as.integer(partes[1])
    semestre <- as.integer(partes[2])

    if (semestre == 1) {
      return(
        list(
          inicio = as.Date(paste0(año, "-01-01")),
          fin = as.Date(paste0(año, "-06-30"))
        )
      )
    }

    return(
      list(
        inicio = as.Date(paste0(año, "-07-01")),
        fin = as.Date(paste0(año, "-12-31"))
      )
    )
  }

  if (input$time_level == "Trimestre") {

    partes <- strsplit(
      input$time_selection,
      " - T",
      fixed = TRUE
    )[[1]]

    año <- as.integer(partes[1])
    trimestre <- as.integer(partes[2])

    inicio_mes <- ((trimestre - 1) * 3) + 1
    inicio <- as.Date(
      paste0(
        año,
        "-",
        sprintf("%02d", inicio_mes),
        "-01"
      )
    )

    fin <- seq.Date(
      inicio,
      by = "month",
      length.out = 4
    )[4] - 1

    return(
      list(
        inicio = inicio,
        fin = fin
      )
    )
  }

  if (input$time_level == "Mes") {

    inicio <- as.Date(
      paste0(
        format(
          as.Date(input$time_selection),
          "%Y-%m"
        ),
        "-01"
      )
    )

    fin <- seq.Date(
      inicio,
      by = "month",
      length.out = 2
    )[2] - 1

    return(
      list(
        inicio = inicio,
        fin = fin
      )
    )
  }

  inicio <- as.Date(
    input$time_selection
  )

  list(
    inicio = inicio,
    fin = inicio
  )
}

# ============================================================
# UI
# ============================================================

clusters_ui <- function() {

  div(
    style = "width: 100%;",

    h4("Clusters por sector"),

    div(
      style = "
        display: flex;
        gap: 20px;
        align-items: flex-start;
        width: 100%;
        flex-wrap: wrap;
      ",

      div(
        style = "
          flex: 1 1 45%;
          min-width: 420px;
        ",

        div(
          style = "display: flex; gap: 12px; flex-wrap: wrap; align-items: flex-end;",

          div(
            style = "flex: 1 1 220px; min-width: 220px;",
            selectInput(
              "cluster_source",
              "Clasificación:",
              choices = c(
                "Clusters por delitos" = "delitos",
                "Clusters por días" = "dias"
              ),
              selected = "delitos"
            )
          ),

          div(
            style = "flex: 1 1 220px; min-width: 220px;",
            selectInput(
              "cluster_value_type",
              "Valor:",
              choices = c(
                "Por cada 100 mil habitantes" = "tasa",
                "Total" = "total"
              ),
              selected = "tasa"
            )
          )
        ),

        leafletOutput(
          "mapa_clusters",
          height = "620px"
        )
      ),

      div(
        style = "
          flex: 1 1 50%;
          min-width: 520px;
        ",

        h4(
          "Incidencia por cluster"
        ),

        p(
          style = "color: #898b8c; margin-bottom: 10px;",
          "En modo por 100 mil habitantes, 1.0 = incidencia de CDMX.
          En modo total, se muestran los delitos acumulados del periodo."
        ),

        plotlyOutput(
          "cluster_rates",
          height = "700px"
        )
      )
    ),

    # Small reference note: the grouped categories used by the cluster chart.
    div(
      style = "
        width: 100%;
        margin-top: 8px;
        color: #898b8c;
        font-size: 0.78rem;
        line-height: 1.45;
      ",
      tags$strong("Grupos de delitos:"),
      tags$span(
        " Violentos y sexuales: Homicidio doloso, Feminicidio, Secuestro, Violación y Lesiones dolosas disparo. "
      ),
      tags$span(
        "Robo transporte público: Robo metro, Robo taxi CV y Robo microbus. "
      ),
      tags$span(
        "Robo en vía pública: Robo transeunte y Robo cuentahabiente CV. "
      ),
      tags$span(
        "Robos patrimoniales: Robo negocio CV, Robo casa habitación CV, Robo de vehículo, Robo transportista y Robo repartidor. "
      ),
      tags$span(
        "Total: suma de todas las categorías."
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================

clusters_server <- function(
    input,
    output
) {

  cluster_table <- reactive({

    req(input$cluster_source)

    if (input$cluster_source == "dias") {
      return(cluster_dias)
    }

    cluster_delitos
  })

  # ----------------------------------------------------------
  # MAP
  # ----------------------------------------------------------

  output$mapa_clusters <- renderLeaflet({

    req(
      input$cluster_source
    )

    # Cluster tab always shows all CDMX sectors. The global
    # geographic filters are intentionally ignored here.
    sectores <- sectores_mapa |>
      dplyr::left_join(
        cluster_table(),
        by = "sector_id"
      ) |>
      dplyr::mutate(
        cluster_label = cluster_labels(cluster, input$cluster_source),
        cluster_color = unname(
          cluster_colors_for_source(input$cluster_source)[cluster]
        )
      )

    sectores <- sf::st_transform(
      sectores,
      4326
    )

    mapa <- leaflet::leaflet() |>
      leaflet::addPolygons(
        data = sectores,
        fillColor = ~cluster_color,
        fillOpacity = 0.72,
        color = "#ffffff",
        weight = 1,
        opacity = 1,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = dark_blue,
          bringToFront = TRUE
        )
      )

    mapa
  })

  # ----------------------------------------------------------
  # NORMALIZED RATE CHART
  # ----------------------------------------------------------

  output$cluster_rates <- renderPlotly({

    req(
      input$time_level,
      input$time_selection,
      input$cluster_source,
      input$cluster_value_type
    )

    rango <- rango_temporal_clusters(input)
    clusters <- cluster_table()
    source_actual <- input$cluster_source

    # ----------------------------------------------------------
    # CATEGORY GROUPS USED ONLY IN THE CLUSTERS TAB
    # ----------------------------------------------------------
    # Every source category is placed into one of the requested groups.
    # `Total` is kept as the sum of all dashboard crime categories.
    grupos_cluster <- list(
      "Violentos y sexuales" = c(
        "Homicidio doloso",
        "Feminicidio",
        "Secuestro",
        "Violación",
        "Lesiones dolosas disparo"
      ),
      "Robo transporte público" = c(
        "Robo metro",
        "Robo taxi CV",
        "Robo microbus"
      ),
      "Robo en vía pública" = c(
        "Robo transeunte",
        "Robo cuentahabiente CV"
      ),
      "Robos patrimoniales" = c(
        "Robo negocio CV",
        "Robo casa habitación CV",
        "Robo de vehículo",
        "Robo transportista",
        "Robo repartidor"
      )
    )

    categorias_cluster <- c(
      names(grupos_cluster),
      "Total"
    )

    # ----------------------------------------------------------
    # CLUSTERS + POPULATION
    # ----------------------------------------------------------

    sectores_base <- sectores_mapa |>
      sf::st_drop_geometry() |>
      dplyr::select(
        sector_id,
        alcaldi
      ) |>
      dplyr::inner_join(
        clusters,
        by = "sector_id"
      ) |>
      dplyr::distinct(
        sector_id,
        cluster,
        alcaldi
      )

    req(
      nrow(sectores_base) > 0
    )

    poblacion_cluster <- sectores_base |>
      dplyr::left_join(
        pob_sector |>
          dplyr::select(
            sector_id,
            POBTOT
          ),
        by = "sector_id"
      ) |>
      dplyr::group_by(
        cluster
      ) |>
      dplyr::summarise(
        poblacion = sum(
          POBTOT,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    # ----------------------------------------------------------
    # CRIME COUNTS FOR CLUSTERS
    # ----------------------------------------------------------

    datos <- resumen_delitos |>
      dplyr::filter(
        !is.na(fecha_hecho),
        fecha_hecho >= rango$inicio,
        fecha_hecho <= rango$fin
      ) |>
      dplyr::inner_join(
        clusters,
        by = "sector_id"
      )

    columnas <- c(
      "total_delitos",
      delitos_disponibles
    )

    conteos <- datos |>
      dplyr::group_by(cluster) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(columnas),
          ~ sum(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      )

    # Build grouped counts explicitly so categories remain readable and stable.
    datos_grupos <- lapply(
      names(grupos_cluster),
      function(nombre_grupo) {
        categorias <- intersect(
          grupos_cluster[[nombre_grupo]],
          delitos_disponibles
        )

        if (length(categorias) == 0) {
          return(
            tibble::tibble(
              cluster = character(0),
              categoria = character(0),
              delitos = numeric(0)
            )
          )
        }

        conteos |>
          dplyr::select(
            cluster,
            dplyr::all_of(categorias)
          ) |>
          dplyr::mutate(
            categoria = nombre_grupo,
            delitos = rowSums(
              dplyr::across(
                dplyr::all_of(categorias)
              ),
              na.rm = TRUE
            )
          ) |>
          dplyr::select(
            cluster,
            categoria,
            delitos
          )
      }
    ) |>
      dplyr::bind_rows()

    total_grupo <- conteos |>
      dplyr::transmute(
        cluster,
        categoria = "Total",
        delitos = total_delitos
      )

    tasas <- dplyr::bind_rows(
      datos_grupos,
      total_grupo
    ) |>
      dplyr::right_join(
        tidyr::expand_grid(
          cluster = unique(sectores_base$cluster),
          categoria = categorias_cluster
        ),
        by = c("cluster", "categoria")
      ) |>
      dplyr::mutate(
        delitos = tidyr::replace_na(delitos, 0)
      ) |>
      dplyr::left_join(
        poblacion_cluster,
        by = "cluster"
      ) |>
      dplyr::mutate(
        tasa_cluster = dplyr::if_else(
          !is.na(poblacion) & poblacion > 0,
          delitos / poblacion * 100000,
          NA_real_
        )
      )

    # ----------------------------------------------------------
    # CDMX BASELINE FOR THE SAME GROUPS
    # ----------------------------------------------------------

    poblacion_cdmx <- sum(
      pob_alcaldia$POBTOT,
      na.rm = TRUE
    )

    datos_cdmx <- resumen_delitos |>
      dplyr::filter(
        !is.na(fecha_hecho),
        fecha_hecho >= rango$inicio,
        fecha_hecho <= rango$fin
      )

    conteos_cdmx <- datos_cdmx |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(columnas),
          ~ sum(.x, na.rm = TRUE)
        )
      )

    cdmx_grupos <- lapply(
      names(grupos_cluster),
      function(nombre_grupo) {
        categorias <- intersect(
          grupos_cluster[[nombre_grupo]],
          delitos_disponibles
        )

        total <- if (length(categorias) == 0) {
          0
        } else {
          sum(
            vapply(
              categorias,
              function(cat) {
                as.numeric(conteos_cdmx[[cat]][1])
              },
              numeric(1)
            )
          )
        }

        tibble::tibble(
          categoria = nombre_grupo,
          delitos_cdmx = total
        )
      }
    ) |>
      dplyr::bind_rows()

    cdmx_total <- tibble::tibble(
      categoria = "Total",
      delitos_cdmx = as.numeric(
        conteos_cdmx$total_delitos[1]
      )
    )

    tasas_cdmx <- dplyr::bind_rows(
      cdmx_grupos,
      cdmx_total
    ) |>
      dplyr::mutate(
        tasa_cdmx = if (
          isTRUE(poblacion_cdmx > 0)
        ) {
          delitos_cdmx / poblacion_cdmx * 100000
        } else {
          NA_real_
        }
      ) |>
      dplyr::select(
        categoria,
        tasa_cdmx
      )

    tasas <- tasas |>
      dplyr::left_join(
        tasas_cdmx,
        by = "categoria"
      ) |>
      dplyr::mutate(
        indice_cdmx = dplyr::if_else(
          !is.na(tasa_cluster) &
            !is.na(tasa_cdmx) &
            tasa_cdmx > 0,
          tasa_cluster / tasa_cdmx,
          NA_real_
        ),
        cluster_label = cluster_labels(
          cluster,
          source_actual
        ),
        categoria = factor(
          categoria,
          levels = categorias_cluster
        )
      )

    colores_fuente <- cluster_colors_for_source(
      source_actual
    )

    colores <- stats::setNames(
      unname(colores_fuente),
      cluster_labels(
        names(colores_fuente),
        source_actual
      )
    )

    usa_total <- identical(input$cluster_value_type, "total")

    datos_grafica <- tasas |>
      dplyr::mutate(
        valor_grafica = if (usa_total) delitos else indice_cdmx
      )

    plot <- plotly::plot_ly(
      data = datos_grafica,
      x = ~categoria,
      y = ~valor_grafica,
      color = ~cluster_label,
      colors = colores,
      type = "bar",
      hovertemplate = if (usa_total) {
        paste0(
          "<b>%{x}</b><br>",
          "%{fullData.name}<br>",
          "Total de delitos: %{y:,.0f}",
          "<extra></extra>"
        )
      } else {
        paste0(
          "<b>%{x}</b><br>",
          "%{fullData.name}<br>",
          "Índice vs CDMX: %{y:.2f}<br>",
          "1.00 = CDMX",
          "<extra></extra>"
        )
      },
      marker = list(
        line = list(
          width = 0
        )
      )
    ) |>
      plotly::layout(
        barmode = "group",
        xaxis = list(
          title = NULL,
          tickangle = -35
        ),
        yaxis = list(
          title = if (usa_total) {
            "Total de delitos"
          } else {
            "Índice de incidencia vs CDMX"
          },
          rangemode = "tozero"
        ),
        shapes = if (usa_total) {
          list()
        } else {
          list(
            list(
              type = "line",
              xref = "paper",
              yref = "y",
              x0 = 0,
              x1 = 1,
              y0 = 1,
              y1 = 1,
              line = list(
                color = black,
                width = 1.5,
                dash = "dash"
              )
            )
          )
        },
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.28
        ),
        margin = list(
          t = 20,
          r = 20,
          b = 120,
          l = 80
        )
      )

    plot
  })}
