#' Cross-calibrate lens
#'
#' Estimate a lens projection for an uncalibrated camera by referencing a
#' calibrated camera photographed from the exact same location.
#'
#' @description
#' Given two photographs taken from the same point (matching entrance pupils and
#' aligned optical axes), with calibrated and uncalibrated cameras, derives a
#' polynomial projection for the uncalibrated device. Intended for cases where a
#' camera calibrated with a method of higher accuracy than [calibrate_lens()] is
#' available, or when there is a main camera to which all other devices should
#' be adjusted.
#'
#' Points must be digitized in tandem with ImageJ and saved as CSV files.
#' See [calibrate_lens()] for background and general concepts.
#'
#' @param path_to_csv_uncal,path_to_csv_cal character vectors of length one.
#'   Paths to CSV files created with ImageJ’s point selection tool (uncalibrated
#'   and calibrated images, respectively).
#' @param zenith_colrow_uncal,zenith_colrow_cal numeric vectors of length two.
#'   Raster coordinates of the zenith for the uncalibrated and calibrated
#'   images; see [calc_zenith_colrow()].
#' @param diameter_cal numeric vector of length one. Image diameter (pixels) of
#'   the calibrated camera.
#' @param lens_coef numeric vector. Lens projection coefficients of the
#'   calibrated camera.
#' @param degree numeric vector of length one. Polynomial degree for the
#'   uncalibrated model fit (default 3).
#'
#' @return List with components:
#' \describe{
#'   \item{`ds`}{`data.frame` with zenith angle (`theta`, radians) and pixel radius
#'     (`px`) from the uncalibrated camera.}
#'   \item{`model`}{`lm` object: polynomial fit of `px` ~ `theta`.}
#'   \item{`horizon_radius`}{numeric vector of length one. Pixel radius at 90 deg.}
#'   \item{`lens_coef`}{numeric vector. Distortion coefficients normalized by
#'     `horizon_radius`.}
#' }
#'
#' @seealso [calibrate_lens()], [calc_zenith_colrow()]
#'
#' @export
crosscalibrate_lens <- function(path_to_csv_uncal,
                                path_to_csv_cal,
                                zenith_colrow_uncal,
                                zenith_colrow_cal,
                                diameter_cal,
                                lens_coef,
                                degree = 3) {

  .check_vector(path_to_csv_uncal, "character", 1)
  .assert_file_exists(path_to_csv_uncal)
  .check_vector(path_to_csv_cal, "character", 1)
  .assert_file_exists(path_to_csv_cal)
  .check_vector(zenith_colrow_uncal, "numeric", 2, sign = "positive")
  .check_vector(zenith_colrow_cal, "numeric", 2, sign = "positive")
  .check_vector(diameter_cal, "numeric", 1, sign = "positive")
  .check_vector(lens_coef, "numeric", sign = "any")
  .check_vector(degree, "integerish", 1, sign = "positive")

  csv_uncal <- utils::read.csv(path_to_csv_uncal)
  csv_uncal <- cbind(csv_uncal$X, csv_uncal$Y)
  csv_cal <- utils::read.csv(path_to_csv_cal)
  csv_cal <- cbind(csv_cal$X, csv_cal$Y)

  ## center in (0,0)
  csv_uncal[, 1] <- csv_uncal[, 1] - zenith_colrow_uncal[1]
  csv_uncal[, 2] <- csv_uncal[, 2] - zenith_colrow_uncal[2]
  csv_cal[, 1] <- csv_cal[, 1] - zenith_colrow_cal[1]
  csv_cal[, 2] <- csv_cal[, 2] - zenith_colrow_cal[2]

  csv_uncal <- pracma::cart2pol(csv_uncal)
  csv_cal <- pracma::cart2pol(csv_cal)
  px <- csv_uncal[, 2]

  angle <- seq(0, 90,  length.out = 90)
  R <- calc_relative_radius(angle, lens_coef)
  inv_fun <- splinefun(R * diameter_cal/2, .degree2radian(angle))

  theta <- inv_fun(csv_cal[, 2])

  fit <- lm(px ~ poly(theta, degree, raw = TRUE) - 1)
  horizon_radius <- stats::predict(fit, data.frame(theta = pi / 2)) %>% unname()

  list(ds = data.frame(theta, px),
       model = fit,
       horizon_radius = horizon_radius,
       lens_coef = (coefficients(fit) / horizon_radius) %>% unname())
}
