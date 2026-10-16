

.value_registry <- list(
  predicted_speed = list(
    column = "predicted_mean",
    label  = "Predicted speed (km/h)"
  ),
  predicted_volume = list(
    column = "predicted_mean",
    label  = "Predicted traffic volume"
  ),
  relative_congestion = list(
    column = "relative_congestion",
    label  = "Relative congestion (vs city average)"
  )
)





#' Interactive map of road-segment traffic measures
#'
#' Displays standard traffic quantities such as predicted speed,
#' predicted volume, or relative congestion on an interactive map.
#'
#' @param sf_aug An `sf` object returned by `augment_roads()`.
#' @param value Character scalar. One of:
#'   `"predicted_speed"`, `"predicted_volume"`, `"relative_congestion"`.
#' @param engine Currently only `"leaflet"` is supported.
#'
#' @return A leaflet widget.
#' @export
map_roads_interactive <- function(
    sf_aug,
    value = c("predicted_speed", "predicted_volume", "relative_congestion"),
    engine = "leaflet"
) {
  if (!inherits(sf_aug, "sf")) {
    stop("`sf_aug` must be an sf object.")
  }

  value <- match.arg(value)

  spec <- .value_registry[[value]]
  col  <- spec$column
  lab  <- spec$label

  if (!col %in% names(sf_aug)) {
    stop("Required column `", col, "` not found in `sf_aug`.")
  }

  vals <- sf_aug[[col]]
  if (!is.numeric(vals)) {
    stop("Mapped column must be numeric.")
  }

  if (engine != "leaflet") {
    stop("Only engine = 'leaflet' is currently supported.")
  }

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive maps.")
  }
  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required for color scales.")
  }

  pal <- leaflet::colorNumeric(
    palette  = viridisLite::viridis(256),
    domain   = vals,
    na.color = "#CCCCCC"
  )

  tooltip <- paste0(
    "<strong>Segment:</strong> ", sf_aug$seg_id,
    "<br><strong>", lab, ":</strong> ",
    signif(vals, 4)
  )

  m <- leaflet::leaflet(
    sf_aug,
    width  = "100%",
    height = 800
  )

  m <- leaflet::addProviderTiles(
    m,
    leaflet::providers$CartoDB.Positron
  )

  m <- leaflet::addPolylines(
    m,
    color   = pal(vals),
    weight  = 4,
    opacity = 0.9,
    label   = lapply(tooltip, htmltools::HTML)
  )

  m <- leaflet::addLegend(
    m,
    pal     = pal,
    values  = vals,
    title   = paste(lab),
    opacity = 1,
    position = "bottomright"
  )

  m
}




#' Interactive map with multiple standard traffic layers
#'
#' @param sf_aug sf object with road geometries
#' @param values Character vector of traffic measures to include.
#'
#' @return leaflet widget
#' @export
map_roads_interactive_layers <- function(
    sf_aug,
    values = c("predicted_speed", "relative_congestion")
) {
  if (!inherits(sf_aug, "sf")) stop("`sf_aug` must be sf.")

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required.")
  }
  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required.")
  }

  values <- intersect(values, names(.value_registry))
  if (length(values) == 0) {
    stop("No valid traffic measures requested.")
  }

  # Internal group IDs (stable, simple strings)
  group_ids <- values

  # Human-readable labels (ONLY for display)
  group_labels <- vapply(
    values,
    function(v) .value_registry[[v]]$label,
    character(1)
  )

  m <- m <- leaflet::leaflet(sf_aug, width  = "100%", height = 800)
  m <- leaflet::addProviderTiles(
    m,
    leaflet::providers$CartoDB.Positron
  )

  # Register groups using IDs
  m <- leaflet::addLayersControl(
    m,
    overlayGroups = group_ids,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )

  for (i in seq_along(values)) {
    value <- values[i]
    gid   <- group_ids[i]
    lab   <- group_labels[i]
    col   <- .value_registry[[value]]$column

    if (!col %in% names(sf_aug)) next
    vals <- sf_aug[[col]]

    pal <- leaflet::colorNumeric(
      palette  = viridisLite::viridis(256),
      domain   = vals,
      na.color = "#CCCCCC"
    )

    m <- leaflet::addPolylines(
      m,
      data    = sf_aug,
      color   = pal(vals),
      weight  = 4,
      opacity = 0.9,
      group   = gid
    )

    m <- leaflet::addLegend(
      m,
      pal     = pal,
      values  = vals,
      title   = paste(lab),
      group   = gid,
      opacity = 1
    )

  }

  m
}
