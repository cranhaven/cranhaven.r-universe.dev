#' Map between zenith–azimuth angles and raster coordinates
#'
#' @description
#' Bidirectional helpers to convert between angular coordinates on the
#' hemispherical image (`zenith`, `azimuth`) and raster
#' coordinates (`row`, `col`).
#'
#' @section Functions:
#' \describe{
#'   \item{`row_col_from_zenith_azimuth`}{Return image indices for given angles.}
#'   \item{`zenith_azimuth_from_row_col`}{Return angles in degrees for given
#'   image indices.}
#' }
#'
#' @details
#' **`zenith, azimuth` → `row, col`.**
#' A sparse set of valid sky points is sampled over the image and enriched with
#' their angular coordinates. Two local least-squares surfaces
#' (`spatial::surf.ls`, `np = 6`) are fitted to predict `row` and `col` as
#' smooth functions of (`azimuth`, `zenith`). Predictions are rounded to the
#' nearest integer index. Out-of-bounds indices are not produced under normal
#' conditions; clamp externally if needed.
#'
#' **`row, col` → `zenith, azimuth`.**
#' Angles are obtained by direct lookup on `z` and `a` using
#' `terra::cellFromRowCol`. If any queried cell is `NA` (e.g., outside the
#' calibrated lens footprint), a synthetic `z` is reconstructed from the lens
#' model attached to `z` (attribute `lens_coef`), and `a` is rebuilt with
#' `azimuth_image()` using the stored orientation attribute in `a`. This yields
#' robust angle retrieval near borders.
#'
#' @inheritParams sky_grid_segmentation
#' @inheritParams zenith_image
#'
#' @param zenith,azimuth numeric vectors. Angles in degrees. Must have equal length.
#' @param row,col numeric vectors. raster coodinates. Must have equal length.
#'
#' @return See *Functions*
#'
#' @name zenith_azimuth_from_row_col
#' @rdname zenith_azimuth_from_row_col
#' @aliases row_col_from_zenith_azimuth
#'
#' @export
#'
#' @examples
#' z <- zenith_image(1000, lens())
#' a <- azimuth_image(z)
#'
#' rc <- row_col_from_zenith_azimuth(z, a, zenith = c(30, 60), azimuth = c(90, 270))
#' rc
#'
#' ang <- zenith_azimuth_from_row_col(z, a, row = rc$row, col = rc$col)
#' ang
zenith_azimuth_from_row_col <- function(z, a, row, col) {

  .check_r_z_a_m(NULL, z, a)
  .check_vector(row, "numeric", sign = "positive")
  .check_vector(col, "numeric", sign = "positive")
  if (length(row) != length(col)) {
    stop("`row` and `col` must have same length.")
  }

  i <- terra::cellFromRowCol(z, row, col)
  u <- is.na(z[i])[,]

  if (any(u)) {
    r <- relative_radius_image(ncol(z), FALSE)
    lens_coef <- attr(z, "lens_coef")

    diameter <- ncol(z)
    # code taken from zenith_image
    x <- relative_radius_image(diameter, FALSE)
    angle <- seq(0, 180, length.out = 2 * nrow(x) + 2)
    R <- calc_relative_radius(angle, lens_coef)
    rcl <- matrix(c(c(0, R[-length(R)]), R, angle), ncol = 3)
    z3 <- terra::classify(x, rcl)
    z1 <- terra::flip(z3)
    z2 <- terra::rev(z3)
    z4 <- terra::rev(z1)
    terra::ext(z1) <- terra::ext(0, diameter/2, diameter/2, diameter)
    terra::ext(z2) <- terra::ext(diameter/2, diameter, diameter/2, diameter)
    terra::ext(z4) <- terra::ext(diameter/2, diameter, 0, diameter/2)
    z1 <- terra::extend(z1, terra::ext(0, diameter, 0, diameter))
    z2 <- terra::extend(z2, terra::ext(0, diameter, 0, diameter))
    z3 <- terra::extend(z3, terra::ext(0, diameter, 0, diameter))
    z4 <- terra::extend(z4, terra::ext(0, diameter, 0, diameter))
    z <- sum(z1, z2, z3, z4, na.rm = TRUE)

    a <- azimuth_image(z, attr(a, "orientation"))
  }
  zenith <- z[i][,]
  azimuth <- a[i][,]

  data.frame(z = zenith, a = azimuth)
}

#' @rdname zenith_azimuth_from_row_col
#' @export
row_col_from_zenith_azimuth <- function(z, a, zenith, azimuth) {
  .check_r_z_a_m(NULL, z, a)
  .check_vector(zenith, "numeric", sign = "positive")
  .check_vector(azimuth, "numeric", sign = "positive")
  if (length(zenith) != length(azimuth)) {
    stop("`zenith` and `azimuth` must have same length.")
  }

  stopifnot(length(zenith) == length(azimuth))

  size <- 100
  sky_points <- expand.grid(row = seq(1, nrow(z), length.out = size) %>%
                              round(),
                            col = seq(1, nrow(z), length.out = size) %>%
                              round())
  sky_points <- extract_dn(z, sky_points, use_window = FALSE)
  sky_points <- sky_points[!is.na(sky_points[,3]), c("row", "col")]
  sky_points <- extract_rr(z, z, a, sky_points,
                           use_window = FALSE)$sky_points

  fit <- spatial::surf.ls(x = sky_points[, "a"],
                          y = sky_points[, "z"],
                          z = sky_points[, "row"],
                          np = 6)
  row <- predict(fit, azimuth, zenith) %>% round()

  fit <- spatial::surf.ls(x = sky_points[, "a"],
                          y = sky_points[, "z"],
                          z = sky_points[, "col"],
                          np = 6)
  col <- predict(fit, azimuth, zenith) %>% round()

  data.frame(row, col)
}
