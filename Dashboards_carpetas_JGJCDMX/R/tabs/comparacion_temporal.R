# ============================================================
# COMPARACIÓN TEMPORAL TAB
# ============================================================

# The temporal comparison uses Total + the detailed crime categories only.
categorias_comparacion <- c("Total", delitos_disponibles)


# ============================================================
# HELPERS
# ============================================================

periodos_temporales <- function(
    inicio,
    fin,
    agrupacion
) {

  if (agrupacion == "Año") {
    seq.Date(
      as.Date(paste0(format(inicio, "%Y"), "-01-01")),
      as.Date(paste0(format(fin, "%Y"), "-01-01")),
      by = "year"
    )

  } else if (agrupacion == "Mes") {
    seq.Date(
      as.Date(paste0(format(inicio, "%Y-%m"), "-01")),
      as.Date(paste0(format(fin, "%Y-%m"), "-01")),
      by = "month"
    )

  } else {
    seq.Date(
      inicio,
      fin,
      by = "day"
    )
  }
}


# ============================================================
# UI
# ============================================================

comparacion_temporal_ui <- function() {

  div(
    style = "width: 100%;",

    h4("Incidencia delictiva a lo largo del tiempo"),

    p(
      "Cada línea muestra los delitos de la categoría como índice relativo a su promedio del periodo (1 = promedio)."
    ),

    div(
      style = "
        width: 100%;
        margin-bottom: 20px;
        display: flex;
        align-items: flex-end;
        gap: 24px;
        flex-wrap: wrap;
      ",

      div(
        style = "width: 260px;",
        selectInput(
          "agrupacion_temporal",
          "Agrupar por:",
          choices = c(
            "Año",
            "Mes",
            "Día"
          ),
          selected = "Mes",
          width = "260px"
        )
      ),

      div(
        style = "padding-bottom: 8px;",
        checkboxInput(
          "mostrar_covid",
          "Mostrar línea de COVID-19",
          value = FALSE
        )
      )
    ),

    plotlyOutput(
      "comparacion_temporal_plot",
      height = "560px"
    ),

    h4(
      "Patrones temporales",
      style = "margin-top: 30px;"
    ),

    div(
      style = "
        display: flex;
        flex-direction: column;
        gap: 24px;
        width: 100%;
      ",

      div(
        style = "width: 100%;",
        h5("Promedio de delitos por día de la semana"),
        plotlyOutput(
          "promedio_semana",
          height = "420px"
        )
      ),

      div(
        style = "width: 100%;",
        h5("Promedio de delitos por día del mes"),
        plotlyOutput(
          "promedio_dia_mes",
          height = "420px"
        )
      ),

      div(
        style = "width: 100%;",
        h5("Promedio de delitos por hora del día"),
        plotlyOutput(
          "promedio_hora",
          height = "420px"
        )
      ),

      div(
        style = "width: 100%;",
        h5("Promedio de delitos por mes del año"),
        plotlyOutput(
          "promedio_mes_anio",
          height = "420px"
        )
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

comparacion_temporal_server <- function(
    input,
    output,
    session,
    carpetas_geografia
) {

  # ----------------------------------------------------------
  # SHARED PREPARATION
  # ----------------------------------------------------------

  datos_temporales <- reactive({

    req(
      input$main_tab == "Comparación temporal",
      input$time_selection
    )

    rango <- as.Date(
      input$time_selection
    )

    req(
      length(rango) == 2,
      !any(is.na(rango)),
      rango[1] <= rango[2]
    )

    columnas <- c(
      "fecha_hecho",
      "hora_hecho",
      columnas_dashboard
    )

    datos <- carpetas_geografia() |>
      dplyr::select(
        all_of(columnas)
      ) |>
      filter(
        !is.na(fecha_hecho),
        fecha_hecho >= rango[1],
        fecha_hecho <= rango[2]
      ) |>
      group_by(
        fecha_hecho,
        hora_hecho
      ) |>
      summarise(
        across(
          all_of(
            columnas_dashboard
          ),
          ~ sum(
            .x,
            na.rm = TRUE
          )
        ),
        .groups = "drop"
      )

    list(
      inicio = rango[1],
      fin = rango[2],
      datos = datos
    )
  })


  # ==========================================================
  # LINE CHART
  # ==========================================================

  output$comparacion_temporal_plot <- renderPlotly({

    datos_info <- datos_temporales()
    datos <- datos_info$datos
    rango_inicio <- datos_info$inicio
    rango_fin <- datos_info$fin

    req(input$agrupacion_temporal)

    periodos <- periodos_temporales(
      rango_inicio,
      rango_fin,
      input$agrupacion_temporal
    )

    if (input$agrupacion_temporal == "Año") {

      datos_periodo <- datos |>
        mutate(
          periodo = as.Date(
            paste0(
              format(
                fecha_hecho,
                "%Y"
              ),
              "-01-01"
            )
          )
        )

      formato_hover <- "%Y"
      titulo_eje_x <- "Año"

    } else if (input$agrupacion_temporal == "Mes") {

      datos_periodo <- datos |>
        mutate(
          periodo = as.Date(
            paste0(
              format(
                fecha_hecho,
                "%Y-%m"
              ),
              "-01"
            )
          )
        )

      formato_hover <- "%m/%Y"
      titulo_eje_x <- "Mes"

    } else {

      datos_periodo <- datos |>
        mutate(
          periodo = fecha_hecho
        )

      formato_hover <- "%d/%m/%Y"
      titulo_eje_x <- "Día"
    }

    valores <- columnas_dashboard

    datos_agregados <- datos_periodo |>
      group_by(periodo) |>
      summarise(
        across(
          all_of(valores),
          ~ sum(
            .x,
            na.rm = TRUE
          )
        ),
        .groups = "drop"
      )

    base <- expand.grid(
      categoria_delito = categorias_comparacion,
      periodo = periodos,
      stringsAsFactors = FALSE
    ) |>
      as.data.frame()

    base_larga <- datos_agregados |>
      tidyr::pivot_longer(
        cols = all_of(valores),
        names_to = "categoria_columna",
        values_to = "delitos"
      ) |>
      mutate(
        categoria_delito = ifelse(
          categoria_columna == "total_delitos",
          "Total",
          categoria_columna
        )
      ) |>
      select(
        categoria_delito,
        periodo,
        delitos
      )

    base <- base |>
      left_join(
        base_larga,
        by = c(
          "categoria_delito",
          "periodo"
        )
      ) |>
      mutate(
        delitos = ifelse(
          is.na(delitos),
          0,
          delitos
        )
      ) |>
      arrange(
        categoria_delito,
        periodo
      ) |>
      group_by(categoria_delito) |>
      mutate(
        promedio_periodo = mean(
          delitos,
          na.rm = TRUE
        ),
        indice = ifelse(
          promedio_periodo > 0,
          delitos / promedio_periodo,
          0
        )
      ) |>
      ungroup()

    colores <- color_categorias(
      categorias_comparacion
    )

    grafica <- plot_ly()

    for (
      categoria_actual in categorias_comparacion
    ) {

      datos_categoria <- base |>
        filter(
          categoria_delito == categoria_actual
        )

      grafica <- grafica |>
        add_lines(
          data = datos_categoria,
          x = ~periodo,
          y = ~indice,
          name = categoria_actual,
          line = list(
            color = colores[[categoria_actual]],
            width = 2
          ),
          marker = list(
            color = colores[[categoria_actual]],
            size = 6,
            symbol = "circle"
          ),
          mode = "lines+markers",
          visible = if (identical(categoria_actual, "Total")) TRUE else "legendonly",
          hovertemplate = paste0(
            "<b>%{x|",
            formato_hover,
            "}</b><br>",
            "Índice vs. promedio: %{y:.2f}",
            "<extra>",
            categoria_actual,
            "</extra>"
          )
        )
    }

    covid_shapes <- list()
    covid_annotations <- list()
    covid_fecha <- as.Date("2020-03-01")

    if (
      isTRUE(input$mostrar_covid) &&
      covid_fecha >= rango_inicio &&
      covid_fecha <= rango_fin
    ) {
      covid_shapes <- list(
        list(
          type = "line",
          xref = "x",
          yref = "paper",
          x0 = covid_fecha,
          x1 = covid_fecha,
          y0 = 0,
          y1 = 1,
          line = list(
            color = red,
            width = 2,
            dash = "solid"
          )
        )
      )

      covid_annotations <- list(
        list(
          x = covid_fecha,
          xref = "x",
          y = 1,
          yref = "paper",
          text = "Inicio COVID-19",
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "bottom",
          font = list(
            color = red,
            size = 10
          )
        )
      )
    }

    grafica |>
      layout(
        xaxis = list(
          title = titulo_eje_x
        ),
        yaxis = list(
          title = "Índice relativo al promedio",
          zeroline = TRUE,
          rangemode = "tozero"
        ),
        shapes = covid_shapes,
        annotations = covid_annotations,
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.24
        ),
        margin = list(
          t = 20,
          r = 20,
          b = 105,
          l = 70
        )
      )
  })


  # ==========================================================
  # DAILY BASE FOR AVERAGE PATTERNS
  # ==========================================================

  datos_diarios <- reactive({

    datos_info <- datos_temporales()
    datos <- datos_info$datos

    fechas <- seq.Date(
      datos_info$inicio,
      datos_info$fin,
      by = "day"
    )

    valores <- columnas_dashboard

    diarios <- datos |>
      group_by(fecha_hecho) |>
      summarise(
        across(
          all_of(valores),
          ~ sum(
            .x,
            na.rm = TRUE
          )
        ),
        .groups = "drop"
      )

    base <- expand.grid(
      fecha_hecho = fechas,
      stringsAsFactors = FALSE
    ) |>
      as.data.frame() |>
      left_join(
        diarios,
        by = "fecha_hecho"
      )

    for (columna in valores) {
      base[[columna]] <- ifelse(
        is.na(base[[columna]]),
        0,
        base[[columna]]
      )
    }

    base
  })


  # ==========================================================
  # AVERAGE BY WEEKDAY
  # ==========================================================

  output$promedio_semana <- renderPlotly({

    datos <- datos_diarios()

    indice_dia <- ((
      as.POSIXlt(datos$fecha_hecho)$wday + 6
    ) %% 7) + 1

    etiquetas <- c(
      "Lunes",
      "Martes",
      "Miércoles",
      "Jueves",
      "Viernes",
      "Sábado",
      "Domingo"
    )

    datos$día_semana <- factor(
      etiquetas[indice_dia],
      levels = etiquetas,
      ordered = TRUE
    )

    valores <- columnas_dashboard

    promedios_diarios <- datos |>
      summarise(
        across(
          all_of(valores),
          ~ mean(
            .x,
            na.rm = TRUE
          )
        )
      )

    largos <- datos |>
      group_by(día_semana) |>
      summarise(
        across(
          all_of(valores),
          ~ mean(
            .x,
            na.rm = TRUE
          )
        ),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = all_of(valores),
        names_to = "categoria_columna",
        values_to = "promedio"
      ) |>
      mutate(
        categoria_delito = ifelse(
          categoria_columna == "total_delitos",
          "Total",
          categoria_columna
        ),
        promedio_base = vapply(
          categoria_columna,
          function(x) unname(promedios_diarios[[x]][1]),
          numeric(1)
        ),
        indice = ifelse(
          promedio_base > 0,
          promedio / promedio_base,
          0
        )
      )

    colores <- color_categorias(
      categorias_comparacion
    )

    grafica <- plotly::plot_ly()

    for (categoria_actual in categorias_comparacion) {

      datos_categoria <- largos |>
        dplyr::filter(categoria_delito == categoria_actual)

      grafica <- grafica |>
        plotly::add_bars(
          data = datos_categoria,
          x = ~día_semana,
          y = ~indice,
          name = categoria_actual,
          marker = list(
            color = colores[[categoria_actual]]
          ),
          visible = if (identical(categoria_actual, "Total")) TRUE else "legendonly",
          hovertemplate = paste0(
            "<b>%{x}</b><br>",
            "Índice vs. promedio diario: %{y:.2f}",
            "<extra>", categoria_actual, "</extra>"
          )
        )
    }

    grafica |>
      layout(
        barmode = "group",
        xaxis = list(
          title = NULL
        ),
        yaxis = list(
          title = "Índice relativo al promedio",
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.28
        ),
        margin = list(
          t = 15,
          r = 10,
          b = 95,
          l = 65
        )
      )
  })


  # ==========================================================
  # AVERAGE BY DAY OF MONTH
  # ==========================================================

  output$promedio_dia_mes <- renderPlotly({

    datos <- datos_diarios()

    datos$dia_mes <- as.integer(
      format(
        datos$fecha_hecho,
        "%d"
      )
    )

    valores <- columnas_dashboard

    promedios_diarios <- datos |>
      summarise(
        across(
          all_of(valores),
          ~ mean(
            .x,
            na.rm = TRUE
          )
        )
      )

    largos <- datos |>
      group_by(dia_mes) |>
      summarise(
        across(
          all_of(valores),
          ~ mean(
            .x,
            na.rm = TRUE
          )
        ),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = all_of(valores),
        names_to = "categoria_columna",
        values_to = "promedio"
      ) |>
      mutate(
        categoria_delito = ifelse(
          categoria_columna == "total_delitos",
          "Total",
          categoria_columna
        ),
        promedio_base = vapply(
          categoria_columna,
          function(x) unname(promedios_diarios[[x]][1]),
          numeric(1)
        ),
        indice = ifelse(
          promedio_base > 0,
          promedio / promedio_base,
          0
        )
      )

    colores <- color_categorias(
      categorias_comparacion
    )

    grafica <- plotly::plot_ly()

    for (categoria_actual in categorias_comparacion) {

      datos_categoria <- largos |>
        dplyr::filter(categoria_delito == categoria_actual)

      grafica <- grafica |>
        plotly::add_bars(
          data = datos_categoria,
          x = ~dia_mes,
          y = ~indice,
          name = categoria_actual,
          marker = list(
            color = colores[[categoria_actual]]
          ),
          visible = if (identical(categoria_actual, "Total")) TRUE else "legendonly",
          hovertemplate = paste0(
            "<b>Día %{x}</b><br>",
            "Índice vs. promedio diario: %{y:.2f}",
            "<extra>", categoria_actual, "</extra>"
          )
        )
    }

    grafica |>
      layout(
        barmode = "group",
        xaxis = list(
          title = NULL,
          dtick = 1
        ),
        yaxis = list(
          title = "Índice relativo al promedio",
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.28
        ),
        margin = list(
          t = 15,
          r = 10,
          b = 95,
          l = 65
        )
      )
  })


  # ==========================================================
  # AVERAGE BY HOUR OF DAY
  # ==========================================================

  output$promedio_hora <- renderPlotly({

    datos_info <- datos_temporales()
    datos <- datos_info$datos

    fechas <- seq.Date(
      datos_info$inicio,
      datos_info$fin,
      by = "day"
    )

    valores <- columnas_dashboard

    datos_hora <- datos |>
      filter(
        !is.na(hora_hecho),
        hora_hecho >= 0,
        hora_hecho <= 23
      ) |>
      group_by(
        fecha_hecho,
        hora_hecho
      ) |>
      summarise(
        across(
          all_of(valores),
          ~ sum(
            .x,
            na.rm = TRUE
          )
        ),
        .groups = "drop"
      )

    base_horas <- expand.grid(
      fecha_hecho = fechas,
      hora_hecho = 0:23,
      stringsAsFactors = FALSE
    ) |>
      as.data.frame() |>
      left_join(
        datos_hora,
        by = c(
          "fecha_hecho",
          "hora_hecho"
        )
      )

    for (columna in valores) {
      base_horas[[columna]] <- ifelse(
        is.na(base_horas[[columna]]),
        0,
        base_horas[[columna]]
      )
    }

    promedios_hora <- base_horas |>
      summarise(
        across(
          all_of(valores),
          ~ mean(
            .x,
            na.rm = TRUE
          )
        )
      )

    largos <- base_horas |>
      group_by(hora_hecho) |>
      summarise(
        across(
          all_of(valores),
          ~ mean(
            .x,
            na.rm = TRUE
          )
        ),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = all_of(valores),
        names_to = "categoria_columna",
        values_to = "promedio"
      ) |>
      mutate(
        categoria_delito = ifelse(
          categoria_columna == "total_delitos",
          "Total",
          categoria_columna
        ),
        promedio_base = vapply(
          categoria_columna,
          function(x) unname(promedios_hora[[x]][1]),
          numeric(1)
        ),
        indice = ifelse(
          promedio_base > 0,
          promedio / promedio_base,
          0
        )
      )

    colores <- color_categorias(
      categorias_comparacion
    )

    grafica <- plotly::plot_ly()

    for (categoria_actual in categorias_comparacion) {

      datos_categoria <- largos |>
        dplyr::filter(categoria_delito == categoria_actual)

      grafica <- grafica |>
        plotly::add_bars(
          data = datos_categoria,
          x = ~hora_hecho,
          y = ~indice,
          name = categoria_actual,
          marker = list(
            color = colores[[categoria_actual]]
          ),
          visible = if (identical(categoria_actual, "Total")) TRUE else "legendonly",
          hovertemplate = paste0(
            "<b>%{x}:00</b><br>",
            "Índice vs. promedio horario: %{y:.2f}",
            "<extra>", categoria_actual, "</extra>"
          )
        )
    }

    grafica |>
      layout(
        barmode = "group",
        xaxis = list(
          title = NULL,
          dtick = 1
        ),
        yaxis = list(
          title = "Índice relativo al promedio",
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.28
        ),
        margin = list(
          t = 15,
          r = 10,
          b = 95,
          l = 65
        )
      )
  })


  # ==========================================================
  # AVERAGE BY MONTH OF YEAR
  # ==========================================================

  output$promedio_mes_anio <- renderPlotly({

    datos <- datos_diarios()

    datos$mes_anio <- as.integer(
      format(
        datos$fecha_hecho,
        "%m"
      )
    )

    valores <- columnas_dashboard

    promedio_diario <- datos |>
      summarise(
        across(
          all_of(valores),
          ~ mean(
            .x,
            na.rm = TRUE
          )
        )
      )

    largos <- datos |>
      group_by(mes_anio) |>
      summarise(
        across(
          all_of(valores),
          ~ mean(
            .x,
            na.rm = TRUE
          )
        ),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = all_of(valores),
        names_to = "categoria_columna",
        values_to = "promedio"
      ) |>
      mutate(
        categoria_delito = ifelse(
          categoria_columna == "total_delitos",
          "Total",
          categoria_columna
        ),
        promedio_base = vapply(
          categoria_columna,
          function(x) unname(promedio_diario[[x]][1]),
          numeric(1)
        ),
        indice = ifelse(
          promedio_base > 0,
          promedio / promedio_base,
          0
        )
      )

    meses <- c(
      "Enero",
      "Febrero",
      "Marzo",
      "Abril",
      "Mayo",
      "Junio",
      "Julio",
      "Agosto",
      "Septiembre",
      "Octubre",
      "Noviembre",
      "Diciembre"
    )

    largos$mes_anio <- factor(
      largos$mes_anio,
      levels = 1:12,
      labels = meses,
      ordered = TRUE
    )

    colores <- color_categorias(
      categorias_comparacion
    )

    grafica <- plotly::plot_ly()

    for (categoria_actual in categorias_comparacion) {

      datos_categoria <- largos |>
        dplyr::filter(categoria_delito == categoria_actual)

      grafica <- grafica |>
        plotly::add_bars(
          data = datos_categoria,
          x = ~mes_anio,
          y = ~indice,
          name = categoria_actual,
          marker = list(
            color = colores[[categoria_actual]]
          ),
          visible = if (identical(categoria_actual, "Total")) TRUE else "legendonly",
          hovertemplate = paste0(
            "<b>%{x}</b><br>",
            "Índice vs. promedio diario: %{y:.2f}",
            "<extra>", categoria_actual, "</extra>"
          )
        )
    }

    grafica |>
      layout(
        barmode = "group",
        xaxis = list(
          title = NULL
        ),
        yaxis = list(
          title = "Índice relativo al promedio",
          rangemode = "tozero"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.28
        ),
        margin = list(
          t = 15,
          r = 10,
          b = 95,
          l = 65
        )
      )
  })

}
