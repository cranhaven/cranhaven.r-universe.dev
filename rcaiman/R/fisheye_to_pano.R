#' Fisheye to panoramic
#'
#' @description
#' Reprojects a fisheye (hemispherical) image into a panoramic view using a
#' cylindrical projection. The output is standardized so that rows correspond
#' to zenith angle bands and columns to azimuthal sectors.
#'
#' @details
#' This function computes a cylindrical projection by aggregating pixel values
#' according to their zenith and azimuth angles. Internally, it creates a
#' segmentation grid with [sky_grid_segmentation()] and applies
#' [extract_feature()] to compute a summary statistic (e.g., mean) of pixel
#' values within each cell.
#'
#' @note
#' An early version of this function was used in
#' \insertCite{Diaz2021;textual}{rcaiman}.
#'
#' @inheritParams fisheye_to_equidistant
#' @inheritParams sky_grid_segmentation
#' @inheritParams extract_feature
#'
#' @returns
#' [terra::SpatRaster-class] with rows representing zenith angle bands and
#' columns representing azimuthal sectors. The number of layers and names
#' matches that of the input `r`.
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
#' pano <- fisheye_to_pano(caim, z, a)
#' plotRGB(pano %>% normalize_minmax() %>% multiply_by(255))
#' }
fisheye_to_pano<- function(r, z, a, fun = mean, angle_width = 1) {
  .check_r_z_a_m(r, z, a, r_type = "any")
  if (!is.function(fun) && !methods::is(fun, "standardGeneric")) {
    stop("`fun` must be a function or a standard generic.")
  }
  .check_vector(angle_width, "numeric", 1, sign = "positive")

  .fisheye_to_pano <- function(r) {
    g <- sky_grid_segmentation(z, a, angle_width)
    blue <- extract_feature(r, g, fun, return = "vector")
    xy <- .decode_label(as.numeric(names(blue)))
    r <- matrix(NA, ncol = max(xy$sector_ID), nrow = max(xy$ring_ID))
    r <- terra::rast(r)
    terra::crs(r) <- terra::crs(g)
    terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
    cells <- terra::cellFromXY(r, as.matrix(xy) - 0.5)
    r[cells] <- blue
    terra::flip(r) #because nadir is 0 and for raster 0 is the bottom
  }

  if (terra::nlyr(r) == 1) {
    r <- suppressWarnings(.fisheye_to_pano(r))
  } else {
    layer_names <- names(r)
    r <- Map(function(r) .fisheye_to_pano(r), as.list(r))
    r <- terra::rast(r)
    names(r) <- layer_names
  }
  r
}
