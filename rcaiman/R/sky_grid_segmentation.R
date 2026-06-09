#' Assign sky-grid labels
#'
#' @description
#' Segment a hemispherical view into equal-angle bins in zenith and azimuth,
#' assigning each pixel a grid-cell ID.
#'
#' @details
#' The intersection of zenith rings and azimuth sectors forms a grid whose cells
#' are circular trapezoids. By default, IDs encode both components as
#' `sectorID * 1000 + ringID`. If `first_ring_different = TRUE`, the zenith ring
#' is not subdivided.
#'
#' The code below outputs a comprehensive list of valid values for `angle_width`.
#' For convenience, the column `radians_denom` can be used to provide
#' `angle_width` as `180 / radians_denom_i`, where `radians_denom_i` is a value
#' taken from `radians_denom`.
#'
#' ```
#' df <- data.frame(degrees = 90 / 1:180)
#'
#' deg_to_pi_expr <- function(deg) {
#'   frac <- MASS::fractions(deg / 180)
#'   strsplit(as.character(frac), "/")[[1]][2] %>% as.numeric()
#' }
#'
#' df$radians_denom <- sapply(df$degrees, function(deg) deg_to_pi_expr(deg))
#'
#' z <- zenith_image(10, lens())
#' a <- azimuth_image(z)
#' u <- c()
#' for (i in 1:nrow(df)) {
#'   u <- c(u, tryCatch(is((sky_grid_segmentation(z, a,
#'                             180/df$radians_denom[i])), "SpatRaster"),
#'                      error = function(e) FALSE))
#' }
#' df <- df[u, ]
#' df
#' ```
#'
#' @param z [terra::SpatRaster-class] generated with [zenith_image()].
#' @param a [terra::SpatRaster-class] generated with [azimuth_image()].
#' @param angle_width numeric vector of length one. Angle in deg that must
#'   divide both 0–360 and 0–90 into an integer number of segments. Retrieve a
#'   set of valid values by running
#'   `lapply(c(45, 30, 18, 10), function(a) vapply(0:6, function(x) a/2^x, 1))`.
#' @param first_ring_different logical vector of length one. If `TRUE`, do not
#'   subdivide the first ring.
#'
#' @return Single-layer [terra::SpatRaster-class] with integer labels. The
#'   object carries attributes `angle_width` and `first_ring_different`.
#'
#' @seealso [sky_grid_centers()], [ring_segmentation()], [sector_segmentation()]
#'
#' @export
#'
#' @examples
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#' g <- sky_grid_segmentation(z, a, 15)
#' plot(g == 24005)
#' \dontrun{
#' display_caim(g = g)
#' }
sky_grid_segmentation <- function(z, a, angle_width,
                                  first_ring_different = FALSE) {
  .check_r_z_a_m(NULL, z, a)
  .check_vector(angle_width, "numeric", 1, sign = "positive")
  .check_vector(first_ring_different, "logical", 1)

  fun <- function(s, r) s * 1000 + r
  g <- fun(sector_segmentation(a, angle_width),
           ring_segmentation(z, angle_width))

  if (first_ring_different) {
    for (i in seq(1, 360/angle_width)*1000 + 1) {
      g[g == i] <- 1000
    }
  }

  names(g) <- paste0("Sky grid, ", angle_width, " degrees")
  attr(g, "angle_width") <- angle_width
  attr(g, "first_ring_different") <- first_ring_different
  g
}
