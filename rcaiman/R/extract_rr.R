#' Extract digital numbers at sky points and normalize by estimated zenith
#' radiance
#'
#' @description Compute relative radiance at selected sky points by dividing
#' their digital numbers (DN) by an estimated zenith DN.
#'
#' @param r [terra::SpatRaster-class]. Raster supplying the DN values; must
#'   share rows and columns with the image used to obtain `sky_points`.
#'   DN must be linearly related to radiance. See [read_caim_raw()].
#' @param no_of_points numeric vector of length one or `NULL`. Number of
#'   near-zenith points used to estimate the zenith DN using inverse distance
#'   weighting (power = 2). If `NULL`, the zenith DN is forced to 1, so `rr =
#'   dn`.
#'
#' @inheritParams extract_dn
#' @inheritParams sky_grid_segmentation
#'
#' @return List with named elements:
#' \describe{
#'   \item{`zenith_dn`}{numeric. Estimated DN at the zenith.}
#'   \item{`sky_points`}{`data.frame` with columns `row`, `col`, `a`, `z`, `dn`,
#'   and `rr` (pixel location, angular coordinates, extracted DN, and relative
#'   radiance). If `no_of_points` is `NULL`, `zenith_dn = 1` and `dn = rr`.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#'
#' # See fit_cie_model() for details on the CSV file
#' path <- system.file("external/sky_points.csv",
#'                     package = "rcaiman")
#' sky_points <- read.csv(path)
#' sky_points <- sky_points[c("Y", "X")]
#' colnames(sky_points) <- c("row", "col")
#' head(sky_points)
#'
#' plot(caim$Blue)
#' points(sky_points$col, nrow(caim) - sky_points$row, col = 2, pch = 10)
#' rr <- extract_rr(caim$Blue, z, a, sky_points, 1)
#' points(rr$sky_points$col, nrow(caim) - rr$sky_points$row, col = 3, pch = 0)
#' }
extract_rr <- function(r, z, a, sky_points,
                                no_of_points = 3,
                                use_window = TRUE) {
  .check_r_z_a_m(r, z, a, r_type = "single")
  .check_sky_points(sky_points)
  .check_vector(no_of_points, "integerish", 1, allow_null = TRUE, sign = "positive")
  if (!is.null(no_of_points)) {
    if (nrow(sky_points) < no_of_points) {
      stop("`sky_points` must contain at least `no_of_points` rows.")
    }
  }
  .check_vector(use_window, "logical", 1)

  # Extract spherical coordinates
  sky_points <- extract_dn(c(z, a), sky_points, use_window = FALSE)
  names(sky_points)[3:4] <- c("z", "a")
  if(any(is.na(sky_points$z))) {
      stop("This problem is caused by sky points located too close to the horizon.
       A common solution is to mask the near-horizon region before proceeding,
       for example using:
           bin <- bin & select_sky_region(z, 0, 80)")

  }

  # Extract values from the image with the points
  dn <- extract_dn(r, sky_points[,c("row", "col")], use_window = use_window)[,3]
  sky_points <- cbind(sky_points, dn)

  # Estimate zenith DN
  if (is.null(no_of_points)) {
    zenith_dn <- 1
  } else {
    spherical_distance <- calc_spherical_distance (
                                              sky_points$z %>% .degree2radian(),
                                              sky_points$a %>% .degree2radian(),
                                              0, 0)
    k <- no_of_points
    sorted_indices <- order(spherical_distance)
    w <- spherical_distance[sorted_indices][2:(k + 1)]
    w <- 1 / w^2
    u <- sky_points[sorted_indices[2:(k + 1)], "dn"]
    zenith_dn <- sum(u * (w / sum(w)))
  }

  # Calculate relative radiance
  sky_points$rr <- sky_points$dn / zenith_dn


  list(zenith_dn = zenith_dn,
       sky_points = sky_points)
}
