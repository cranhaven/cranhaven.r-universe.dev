#' Fisheye to equidistant
#'
#' Reproject a hemispherical image from fisheye to equidistant projection
#' (also known as polar projection) to standardize its geometry for
#' subsequent analysis and comparison between images.
#'
#' Pixel values and coordinates are treated as 3D points and reprojected
#' using Cartesian interpolation. Internally, this function uses
#' [lidR::knnidw()] as interpolation engine, so arguments `k`, `p`, and
#' `rmax` are passed to it without modification.
#'
#' @param r [terra::SpatRaster-class] of one or more layers (e.g., RGB channels or
#'   binary masks) in fisheye projection.
#' @param radius numeric vector of length one. Radius (in pixels) of the
#'   reprojected hemispherical image. Must be an integer value (no decimal
#'   part). If `NULL` (default), it is set to `ncol(r) / 2`.
#' @param k,p,rmax numeric vector of length one. Parameters passed to
#'   [lidR::knnidw()]: number of neighbors (`k`), inverse distance weighting
#'   exponent (`p`), and maximum search radius (`rmax`) in units of the output
#'   resolution.
#'
#' @inheritParams compute_canopy_openness
#' @inheritParams sky_grid_segmentation
#'
#' @return [terra::SpatRaster-class] with the same number of layers as `r`,
#'   reprojected to equidistant projection with circular shape and radius
#'   given by `radius
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- system.file("external/APC_0836.jpg", package = "rcaiman")
#' caim <- read_caim(path)
#' calc_diameter(c(0.801, 0.178, -0.179), 1052, 86.2)
#' z <- zenith_image(2216,  c(0.801, 0.178, -0.179))
#' a <- azimuth_image(z)
#' zenith_colrow <- c(1063, 771)
#'
#' caim <- expand_noncircular(caim, z, zenith_colrow)
#' m <- !is.na(caim$Red) & select_sky_region(z, 0, 86.2)
#' caim[!m] <- 0
#' m2 <- fisheye_to_equidistant(m, z, a, !is.na(z), radius = 600)
#' m2 <- binarize_with_thr(m2, 0.5) #to turn it logical
#' caim2[!m2] <- 0
#'
#' plot(caim)
#' }
fisheye_to_equidistant <- function(r, z, a, m,
                                   radius = NULL,
                                   k = 1,
                                   p = 1,
                                   rmax = 100)
  {
  .check_r_z_a_m(r, z, a, m, r_type = "any")
  .check_vector(radius, "numeric", 1, allow_null = TRUE, sign = "positive")
  .check_vector(k, "integerish", 1, sign = "positive")
  .check_vector(p, "numeric", 1, sign = "positive")
  .check_vector(rmax, "numeric", 1, sign = "positive")

  if (is.null(radius)) radius <- ncol(r) / 2

  .fisheye_to_equidistant <- function(r) {
    new_r <- zenith_image(radius * 2, lens())
    terra::ext(new_r) <- terra::ext(-pi / 2, pi / 2, -pi / 2, pi / 2)

    m <- !is.na(z) & !is.na(r) & m

    pol <- data.frame(theta = a[m] * pi / 180 + pi / 2,
                      r = z[m] * pi / 180,
                      z = r[m])
    names(pol) <- c("theta", "r", "z")
    cart <- pracma::pol2cart(as.matrix(pol))

    res <- terra::res(new_r)[1]

    const <- 10000

    res <- res * 1000
    las <- .make_fake_las(cart[,1]*1000, cart[,2]*1000, cart[,3]*const)
    las@data$Classification <- 2
    lidR::crs(las) <- 7589
    ir <- suppressWarnings(
      lidR::rasterize_terrain(las, res = res,
                              algorithm = lidR::knnidw(k = k,
                                                       p = p,
                                                       rmax = res * rmax)
      )
    )
    ir[is.na(ir)] <- 0
    ir / const
  }
  layer_names <- names(r)
  r <- Map(function(r) .fisheye_to_equidistant(r), as.list(r))
  r <- terra::rast(r)
  names(r) <- layer_names

  i <- terra::cellFromXY(r, matrix(c(0, 0), ncol = 2))
  terra::ext(r) <- terra::ext(0, ncol(r), 0, nrow(r))
  zenith_colrow <- terra::rowColFromCell(r, i) %>% as.numeric() %>% rev()

  z2 <- zenith_image(radius*2, lens())
  r <- expand_noncircular(r, z2, zenith_colrow)
  r[is.na(r)] <- 0
  r
}
