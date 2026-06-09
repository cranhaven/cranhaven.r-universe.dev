#' Map sky-grid centers to raster coordinates
#'
#' @description
#' Return image row and column indices for the center point of each
#' cell in a sky grid composed of circular trapezoids of equal angular
#' resolution defined by `angle_width`.
#'
#' @inheritParams sky_grid_segmentation
#'
#' @return `data.frame` with integer columns `row` and `col`, one per grid cell.
#'
#' @seealso [sky_grid_segmentation()]
#'
#' @export
#'
#' @examples
#' z <- zenith_image(100, lens())
#' a <- azimuth_image(z)
#' sky_grid_centers(z, a, 45)
sky_grid_centers <- function(z, a, angle_width) {
  .check_r_z_a_m(NULL, z, a)
  .check_vector(angle_width, "numeric", 1, sign = "positive")

  az <- expand.grid(seq(0 + angle_width/2, 360 - angle_width/2, angle_width),
                    seq(0 + angle_width/2, 90 - angle_width/2, angle_width))
  rr <- calc_relative_radius(az[,2], attr(z, "lens_coef"))

  pol <- data.frame(theta = az[,1] * pi/180 + pi/2,
                    z = rr * 90 * pi/180)
  cart <- pracma::pol2cart(as.matrix(pol))
  p <- terra::vect(matrix(cart, ncol = 2))
  terra::crs(p) <- terra::crs(z)

  z <- terra::deepcopy(z)
  terra::ext(z) <- terra::ext(-pi/2,pi/2,-pi/2,pi/2)
  i <- terra::rasterize(p, z)
  i <- !is.na(i)
  i <- cells(i)[i[]]
  sky_points <- terra::rowColFromCell(z, i)
  sky_points <- as.data.frame(sky_points)
  names(sky_points) <- c("row", "col")
  sky_points
}
