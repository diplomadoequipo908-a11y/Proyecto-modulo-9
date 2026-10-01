# ============================================================
# GLOBAL DATA
# ============================================================
# The crime database is summarized inside the GeoPackage/SQLite
# layer before the result enters R. The raw 1.98M-row object is
# therefore never kept in R memory.

library(sf)
library(dplyr)


# ============================================================
# 1. LOAD GEOGRAPHIC DATA
# ============================================================

cuadrantes <- st_read(
  "data/cuadranes.gpkg",
  quiet = TRUE
)


# ============================================================
# 2. CRIME CATEGORY MAP
# ============================================================

# Names shown in the dashboard -> original values in the source.
delitos_mapa <- c(
  "Robo taxi CV" =
    "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA",
  "Robo transeunte" =
    "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA",
  "Robo de vehículo" =
    "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA",
  "Homicidio doloso" =
    "HOMICIDIO DOLOSO",
  "Lesiones dolosas disparo" =
    "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO",
  "Robo negocio CV" =
    "ROBO A NEGOCIO CON VIOLENCIA",
  "Robo repartidor" =
    "ROBO A REPARTIDOR CON Y SIN VIOLENCIA",
  "Robo casa habitación CV" =
    "ROBO A CASA HABITACIÓN CON VIOLENCIA",
  "Robo cuentahabiente CV" =
    "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA",
  "Robo microbus" =
    "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA",
  "Robo metro" =
    "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA",
  "Robo transportista" =
    "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA",
  "Secuestro" =
    "SECUESTRO",
  "Violación" =
    "VIOLACIÓN",
  "Feminicidio" =
    "FEMINICIDIO"
)

delitos_disponibles <- names(delitos_mapa)

# Keep every dashboard crime category available in the charts.

# ============================================================
# 3. BUILD HOURLY SUMMARY IN THE DATABASE
# ============================================================

sql_escape <- function(x) {
  gsub(
    "'",
    "''",
    x,
    fixed = TRUE
  )
}

# Use ASCII-only temporary SQL field names. This avoids driver-specific
# renaming of fields containing accents such as "Violación".
delito_sql_ids <- paste0(
  "delito_",
  seq_along(delitos_mapa)
)

sql_expressions <- paste(
  sprintf(
    "SUM(CASE WHEN UPPER(TRIM(categoria_delito)) = UPPER(TRIM('%s')) THEN 1 ELSE 0 END) AS %s",
    sql_escape(unname(delitos_mapa)),
    delito_sql_ids
  ),
  collapse = ",\n      "
)

query_resumen <- paste0(
  "SELECT\n",
  "      DATE(fecha_hecho) AS fecha_hecho,\n",
  "      CASE\n",
  "        WHEN hora_hecho IS NULL OR hora_hecho = '' THEN NULL\n",
  "        ELSE CAST(SUBSTR(hora_hecho, 1, 2) AS INTEGER)\n",
  "      END AS hora_hecho,\n",
  "      cuadrante AS cuadrante_id,\n",
  "      sector_id,\n",
  "      alcaldia AS alcaldi,\n",
  "      COUNT(*) AS total_delitos,\n",
  "      ",
  sql_expressions,
  "\n",
  "FROM carpetas_procesadas\n",
  "WHERE UPPER(TRIM(categoria_delito)) NOT IN (",
  "'DELITO DE BAJO IMPACTO',",
  "'HECHO NO DELICTIVO'",
  ")\n",
  "  AND fecha_hecho IS NOT NULL\n",
  "GROUP BY\n",
  "      DATE(fecha_hecho),\n",
  "      CASE\n",
  "        WHEN hora_hecho IS NULL OR hora_hecho = '' THEN NULL\n",
  "        ELSE CAST(SUBSTR(hora_hecho, 1, 2) AS INTEGER)\n",
  "      END,\n",
  "      cuadrante,\n",
  "      sector_id,\n",
  "      alcaldia\n"
)

resumen_delitos <- st_read(
  "data/carpetas_procesadas.gpkg",
  query = query_resumen,
  quiet = TRUE
)

if (inherits(resumen_delitos, "sf")) {
  resumen_delitos <- st_drop_geometry(
    resumen_delitos
  )
}

# Rename the temporary ASCII crime columns to the dashboard labels.
rename_map <- stats::setNames(
  delito_sql_ids,
  delitos_disponibles
)

for (nombre_visible in names(rename_map)) {
  nombre_sql <- rename_map[[nombre_visible]]

  if (!nombre_sql %in% names(resumen_delitos)) {
    stop(
      paste0(
        "No se encontró la columna SQL esperada: ",
        nombre_sql
      )
    )
  }

  names(resumen_delitos)[
    match(nombre_sql, names(resumen_delitos))
  ] <- nombre_visible
}

resumen_delitos <- resumen_delitos |>
  mutate(
    fecha_hecho = as.Date(fecha_hecho),
    hora_hecho = as.integer(hora_hecho),
    cuadrante_id = as.character(cuadrante_id),
    sector_id = as.character(sector_id),
    alcaldi = as.character(alcaldi),
    total_delitos = as.numeric(total_delitos)
  )

for (columna in delitos_disponibles) {
  resumen_delitos[[columna]] <- as.numeric(
    resumen_delitos[[columna]]
  )
}

if (!"Secuestro" %in% names(resumen_delitos)) {
  resumen_delitos[["Secuestro"]] <- 0
}



categorias_dashboard <- c(
  "Total",
  delitos_disponibles
)

columnas_dashboard <- c(
  "total_delitos",
  delitos_disponibles
)

resumen_delitos <- resumen_delitos |>
  arrange(
    fecha_hecho,
    hora_hecho,
    alcaldi,
    sector_id,
    cuadrante_id
  )

# ============================================================
# 4. AVAILABLE DATE RANGE
# ============================================================

fecha_inicio <- min(
  resumen_delitos$fecha_hecho,
  na.rm = TRUE
)

fecha_fin <- max(
  resumen_delitos$fecha_hecho,
  na.rm = TRUE
)


# ============================================================
# 5. SANITY CHECK
# ============================================================

stopifnot(
  all(
    c(
      "fecha_hecho",
      "hora_hecho",
      "cuadrante_id",
      "sector_id",
      "alcaldi",
      columnas_dashboard
    ) %in%
      names(resumen_delitos)
  )
)
