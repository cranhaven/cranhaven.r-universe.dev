#' Interpolate in planar space
#'
#' @description
#' Interpolate values from canopy photographs using inverse distance weighting
#' (IDW) with k-nearest neighbors in image (planar) coordinates.
#' A radius limits neighbor search.
#'
#' @details
#' Delegates interpolation to [lidR::knnidw()], passing `k`, `p`, and `rmax`
#' unchanged. Defaults follow \insertCite{Lang2013;textual}{rcaiman}.
#' Note that `rmax` is given in pixels but intended to approximate 15–20 deg in
#' angular terms. Therefore, this value needs fine-tuning based on image
#' resolution and lens projection. For best results, the interpolated quantity
#' should be linearly related to scene radiance; see [extract_radiometry()] and
#' [read_caim_raw()]. For JPEG images, consider [invert_gamma_correction()] to reverse gamma
#' encoding.
#'
#' @param sky_points `data.frame` with columns `row`, `col`, and one additional
#'   numeric column with values to interpolate. Typically returned by
#'   [extract_rr()] or [extract_dn()].
#' @param r numeric [terra::SpatRaster-class] with one layer. Image from which
#'   `sky_points` were derived, or another raster with the same number of rows
#'   and columns. Used only as geometric template; cell values are ignored.
#' @param col_id numeric or character vector of length one. The name or position
#'   of the column in `sky_points` containing the values to interpolate.
#'
#' @inheritParams fisheye_to_equidistant
#'
#' @note No consistency checks are performed to ensure that `sky_points` and `r`
#'   are geometrically compatible. Incorrect combinations may lead to invalid
#'   outputs.
#'
#' @return Numeric [terra::SpatRaster-class] with one layer and the same geometry
#'   as `r`.
#'
#' @references \insertAllCited{}
#'
#' @export
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
#' g <- sky_grid_segmentation(z, a, 10)
#' sky_points <- extract_sky_points(r, bin, g, dist_to_black = 3)
#' plot(bin)
#' points(sky_points$col, nrow(caim) - sky_points$row, col = 2, pch = 10)
#' sky_points <- extract_dn(r, sky_points)
#'
#' sky <- interpolate_planar(sky_points, r, col_id = 3)
#' plot(sky)
#' plot(r/sky)
#' }
interpolate_planar <- function(sky_points, r,
                               k = 3,
                               p = 2,
                               rmax = 200,
                               col_id = "dn") {
  if (!is.data.frame(sky_points)) {
    stop("`sky_points` must be a data frame.")
  }
  .check_vector(k, "integerish", 1, sign = "positive")
  .check_vector(p, "numeric", 1, sign = "positive")
  .check_vector(rmax, "numeric", 1, sign = "positive")
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


  cells <- terra::cellFromRowCol(r, sky_points$row, sky_points$col)
  xy <- terra::xyFromCell(r, cells)

  if (max(sky_points[,col_id]) < 250) {
    const <- 10000
  } else {
    const <- 1
  }

  las <- .make_fake_las(
    c(xy[, 1]     , 0 - rmax, 0 - rmax       , xmax(r) + rmax, xmax(r) + rmax),
    c(xy[, 2]     , 0 - rmax, ymax(r) + rmax , ymax(r) + rmax, 0 - rmax),
    c(sky_points[,col_id] * const, 0       , 0              ,              0, 0)
  )
  las@data$Classification <- 2
  lidR::crs(las) <- 7589

  ir <- suppressWarnings(
        lidR::rasterize_terrain(las, res = 1,
                                algorithm = lidR::knnidw(k = k,
                                                         p = p,
                                                         rmax = rmax)
                                )
         )
  ir <- terra::resample(ir, r) / const


  p <- terra::vect(xy, "points")
  terra::crs(p) <- crs(r)

  buff <- terra::buffer(p, rmax)
  buff <- terra::rasterize(buff, r)
  ir[is.na(buff)] <- NA
  ir
}
