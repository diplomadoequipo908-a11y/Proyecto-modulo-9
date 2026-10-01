# ============================================================
# DASHBOARD COLOR PALETTE
# ============================================================

# Main dashboard colors

dark_blue <- "#0e2a49"
gold <- "#ab8800"
light_blue <- "#75cae7"
orange <- "#FF5F15"
bright_green <- "#74d102"
black <- "#000000"
gray <- "#898b8c"
red <- "#c20112"
green <- "#006847"

# Colors used to identify up to five simultaneously selected
# map regions. The same color is used for the region's line,
# bar, and hatch pattern.
selected_region_palette <- c(
  gold,
  light_blue,
  orange,
  bright_green,
  dark_blue
)

# Choropleth palette: light red -> dark red.
choropleth_red_palette <- c(
  "#fde3e3",
  "#f06b6b",
  red
)

# ============================================================
# CRIME CATEGORY COLORS
# ============================================================
# One stable color per crime category, reused across temporal
# comparison charts. Total is always dark blue.

color_categorias <- function(
    categorias
) {

  categorias <- unique(
    as.character(categorias)
  )

  resto <- setdiff(
    categorias,
    "Total"
  )

  colores_resto <- if (
    length(resto) > 0
  ) {
    grDevices::hcl.colors(
      length(resto),
      palette = "Dark 3"
    )
  } else {
    character(0)
  }

  colores <- c(
    Total = dark_blue,
    stats::setNames(
      colores_resto,
      resto
    )
  )

  colores[categorias]
}
