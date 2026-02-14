#' Static map of road-segment traffic measures
#'
#' @param sf_aug sf object with road geometries
#' @param value One of "predicted_speed", "predicted_volume",
#'   or "relative_congestion"
#' @importFrom rlang .data
#' @return ggplot object
#' @export
plot_roads_static <- function(
    sf_aug,
    value = c("predicted_speed", "predicted_volume", "relative_congestion")
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
  if (!is.numeric(sf_aug[[col]])) {
    stop("Mapped column must be numeric.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for static plotting.")
  }

  ggplot2::ggplot(sf_aug) +
    ggplot2::geom_sf(
      ggplot2::aes(color = .data[[col]]),
      linewidth = 0.7
    ) +
    ggplot2::scale_color_viridis_c(
      option = "viridis",
      na.value = "grey80",
      name = lab
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = lab
    )
}

