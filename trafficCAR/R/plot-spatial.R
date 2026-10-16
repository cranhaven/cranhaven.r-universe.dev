# shared input validation helpers

#' @keywords internal
.validate_sf_roads <- function(roads) {
  if (!inherits(roads, "sf")) stop("`roads` must be an sf object.")
  if (is.null(sf::st_geometry(roads))) stop("`roads` must have a geometry column.")
  if (nrow(roads) < 1) stop("`roads` must have at least one row.")
  invisible(TRUE)
}

#' @keywords internal
.validate_traffic_fit <- function(fit) {
  if (!inherits(fit, "traffic_fit")) stop("`fit` must be a `traffic_fit`.")
  if (is.null(fit$draws) || !is.list(fit$draws)) stop("`fit$draws` must be a list.")
  invisible(TRUE)
}

#' @keywords internal
.validate_draw_matrix <- function(mat, name, n_segments) {
  if (is.null(mat)) stop("Required draws `", name, "` not found in `fit$draws`.")
  if (!is.matrix(mat)) stop("`fit$draws$", name, "` must be a matrix.")
  if (!is.numeric(mat)) stop("`fit$draws$", name, "` must be numeric.")
  if (ncol(mat) != n_segments) {
    stop("`fit$draws$", name, "` must have ncol equal to nrow(roads).")
  }
  if (any(!is.finite(mat))) stop("`fit$draws$", name, "` must contain only finite values.")
  invisible(TRUE)
}



#' Plot predicted traffic outcome on road network
#'
#' @param fit traffic_fit
#' @param roads sf with segment_id
#' @return ggplot
#' @export
plot_predicted <- function(fit, roads) {
  .validate_traffic_fit(fit)
  .validate_sf_roads(roads)

  .validate_draw_matrix(fit$draws$mu, "mu", n_segments = nrow(roads))

  pred <- colMeans(fit$draws$mu)
  roads$predicted <- pred

  ggplot2::ggplot(roads) +
    ggplot2::geom_sf(
      ggplot2::aes(color = .data$predicted),
      linewidth = 1
    ) +
    ggplot2::scale_color_viridis_c(
      name = if (is.null(fit$outcome_label)) "" else fit$outcome_label
    )
}

#' Plot relative congestion on road network
#'
#' Shows systematic deviations after accounting for covariates.
#'
#' @param fit traffic_fit
#' @param roads sf
#' @return ggplot
#' @export
plot_relative_congestion <- function(fit, roads) {
  .validate_traffic_fit(fit)
  .validate_sf_roads(roads)

  .validate_draw_matrix(fit$draws$x, "x", n_segments = nrow(roads))

  x_mean <- colMeans(fit$draws$x)
  s <- stats::sd(x_mean)

  if (!is.finite(s) || s <= 0) {
    warning("Relative congestion is constant (sd=0); returning zeros.")
    rel <- rep(0, length(x_mean))
  } else {
    rel <- x_mean / s
  }

  roads$relative_congestion <- rel

  ggplot2::ggplot(roads) +
    ggplot2::geom_sf(
      ggplot2::aes(color = .data$relative_congestion),
      linewidth = 1
    ) +
    ggplot2::scale_color_gradient2(
      low = "blue", mid = "white", high = "red",
      name = "Relative congestion\n(vs city average)"
    )
}
