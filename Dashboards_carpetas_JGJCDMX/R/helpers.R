# ============================================================
# HELPER FUNCTIONS
# ============================================================


# ============================================================
# SECTOR LOOKUP
# ============================================================

sector_lookup <- cuadrantes |>
  st_drop_geometry() |>
  distinct(
    sector,
    sector_id,
    alcaldi
  )


# ============================================================
# POPULATION DATA
# ============================================================

pob_sector <- read.csv(
  "data/POB_por_sector.csv",
  check.names = FALSE
)

pob_cuadrante <- read.csv(
  "data/POB_por_cuadrante.csv",
  check.names = FALSE
)

pob_alcaldia <- read.csv(
  "data/POB_por_alcaldia.csv",
  check.names = FALSE
)


# ============================================================
# CDMX POPULATION
# ============================================================

pob_cdmx <- sum(
  pob_alcaldia$POBTOT,
  na.rm = TRUE
)


# ============================================================
# MINIMUM POSITIVE QUADRANT POPULATION
# ============================================================

min_pob_cuadrante <- min(
  pob_cuadrante$POBTOT[
    pob_cuadrante$POBTOT > 0
  ],
  na.rm = TRUE
)


# ============================================================
# CRIME COLUMN HELPER
# ============================================================

columna_delito <- function(crime_type) {
  if (is.null(crime_type) || identical(crime_type, "Total")) {
    return("total_delitos")
  }

  if (crime_type %in% delitos_disponibles) {
    return(crime_type)
  }

  "total_delitos"
}

# ============================================================
# CURRENT POPULATION
# ============================================================

poblacion_actual <- function(
    nivel_geografico,
    seleccion
) {

  if (nivel_geografico == "CDMX") {
    return(as.numeric(pob_cdmx))
  }

  if (nivel_geografico == "Alcaldía") {
    valor <- pob_alcaldia |>
      filter(
        alcaldi == seleccion
      ) |>
      pull(
        POBTOT
      )

    return(
      ifelse(
        length(valor) == 0 || is.na(valor[1]),
        NA_real_,
        as.numeric(valor[1])
      )
    )
  }

  if (nivel_geografico == "Sector") {
    sector_id_actual <- sector_lookup |>
      filter(
        sector == seleccion
      ) |>
      pull(
        sector_id
      )

    valor <- pob_sector |>
      filter(
        sector_id %in% sector_id_actual
      ) |>
      pull(
        POBTOT
      )

    return(
      ifelse(
        length(valor) == 0 || is.na(valor[1]),
        NA_real_,
        as.numeric(valor[1])
      )
    )
  }

  if (nivel_geografico == "Cuadrante") {
    valor <- pob_cuadrante |>
      filter(
        cuadrante_id == seleccion
      ) |>
      pull(
        POBTOT
      )

    if (
      length(valor) == 0 ||
      is.na(valor[1]) ||
      valor[1] <= 0
    ) {
      return(as.numeric(min_pob_cuadrante))
    }

    return(as.numeric(valor[1]))
  }

  NA_real_
}
