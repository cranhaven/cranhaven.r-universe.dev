#' Fit a trend surface to sky digital numbers
#'
#' @description
#' Fits a trend surface to sky digital numbers using [spatial::surf.ls()] as
#' the computational workhorse.
#'
#' @details
#' This function models the variation in digital numbers across the sky dome
#' by fitting a polynomial surface in Cartesian space. It is intended to
#' capture smooth large-scale gradients and is more effective when called
#' via [apply_by_direction()].
#'
#' @param extrapolate logical vector of length one. If `TRUE`, predictions
#'   are extrapolated to the entire extent of `r`; otherwise, predictions
#'   are limited to the convex hull of the input sky points.
#'
#' @inheritParams interpolate_planar
#' @inheritParams spatial::surf.ls
#'
#' @return
#' List with named elements:
#' \describe{
#'   \item{`raster`}{[terra::SpatRaster-class] containing the fitted surface.}
#'   \item{`model`}{object of class `trls` returned by [spatial::surf.ls()].}
#'   \item{`r2`}{numeric value giving the coefficient of determination (R\eqn{^2}) of the fit.}
#' }
#'
#' @references \insertAllCited{}
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' m <- !is.na(z)
#' r <- caim$Blue
#'
#' bin <- binarize_by_region(r, ring_segmentation(z, 15), "thr_isodata") &
#'   select_sky_region(z, 0, 88)
#'
#' g <- sky_grid_segmentation(z, a, 10, first_ring_different = TRUE)
#' sky_points <- extract_sky_points(r, bin, g, dist_to_black = 3)
#' plot(bin)
#' points(sky_points$col, nrow(caim) - sky_points$row, col = 2, pch = 10)
#' sky_points <- extract_dn(r, sky_points, use_window = TRUE)
#'
#' sky_s <- fit_trend_surface(sky_points, r, np = 4, col_id = 3,
#'                            extrapolate = TRUE)
#' plot(sky_s$raster)
#' binarize_with_thr(r/sky_s$raster, 0.5) %>% plot()
#'
#' sky_s <- fit_trend_surface(sky_points, r, np = 6, col_id = 3,
#'                            extrapolate = FALSE)
#' plot(sky_s$raster)
#' binarize_with_thr(r/sky_s$raster, 0.5) %>% plot()
#' }
fit_trend_surface <- function(sky_points,
                              r,
                              np = 6,
                              col_id = "dn",
                              extrapolate = FALSE) {

  if (!is.data.frame(sky_points)) {
    stop("`sky_points` must be a data frame.")
  }
  .assert_single_layer(r)
  .check_vector(np, "integerish", 1, sign = "positive")
  handling_col_id <- c(
    tryCatch(.check_vector(col_id, "numeric", sign = "any"),
             error = function(e) FALSE),
    tryCatch(.check_vector(col_id, "character"),
             error = function(e) FALSE)
  )
  if (!any(handling_col_id)) {
    stop("`col_id` must be the name or position of the column in `sky_points` containing the values to interpolate.")
  }
  if (is.numeric(col_id)) col_id <- names(sky_points)[col_id]
  required_cols <- c("row", "col", col_id)
  if (!all(required_cols %in% names(sky_points))) {
    stop(sprintf("`sky_points` must contain columns %s.",
                 paste(sprintf('"%s"', required_cols), collapse = ", ")))
  }
  .check_vector(extrapolate, "logical", 1)

  xy <- terra::cellFromRowCol(r, sky_points$row, sky_points$col) %>%
    terra::xyFromCell(r, .)
  fit <- spatial::surf.ls(x = xy[, 1],
                          y = xy[, 2],
                          z = sky_points[, col_id],
                          np)
  xl <- terra::xmin(r)
  xu <- terra::xmax(r)
  yl <- terra::ymin(r)
  yu <- terra::ymax(r)

  out <- spatial::trmat(fit, xl, xu, yl, yu, max(ncol(r), nrow(r)))
  out <- terra::rast(out$z) %>% t %>% flip
  terra::crs(out) <- terra::crs(r)
  terra::ext(out) <- terra::ext(r)
  out <- terra::resample(out, r)

  observed <- sky_points[, col_id]
  fitted <- terra::extract(out, xy)[,]
  ss_res <- sum((observed - fitted)^2)
  ss_tot <- sum((observed - mean(observed))^2)
  r2 <- 1 - ss_res / ss_tot

  if (!extrapolate) {
    xy <- xy[grDevices::chull(xy),]
    v <- terra::vect(list(xy), type = "polygon", crs = terra::crs(r))
    v <- terra::rasterize(v, r) %>% is.na()
    out[v] <- NA
  }

  list(raster = out, model = fit, r2 = r2)
}
