#' Fit cone-shaped model
#'
#' @description
#' Fit a polynomial model to predict relative radiance from
#' spherical coordinates using data sampled from a canopy photograph.
#'
#' @details
#' This model requires only `sky_points`, making it useful in workflows where
#' sun position cannot be reliably estimated, such as in [apply_by_direction()].
#' Otherwise, [fit_cie_model()] is a better choice.
#'
#' Depending on `method`, it can fit:
#' \describe{
#'   \item{*A zenith-only quadratic model*}{\deqn{sDN = a + b\theta + c\theta^2}}
#'   \item{*A zenith-plus-azimuth model, adding sinusoidal terms*}{\deqn{sDN = a + b\theta + c\theta^2 + d\sin(\phi) + e\cos(\phi)}}
#' }
#'
#' See \insertCite{Diaz2018;textual}{rcaiman} for details on the full model.
#'
#' @param sky_points `data.frame` returned by [extract_rr()]. If it is generated
#'   by other means, it must have columns `row`, `col`, `z`, `a`, and `rr`.
#' @param method character. Model type to fit:
#'   \describe{
#'     \item{`"zenith_only"`}{Quadratic polynomial in zenith angle.}
#'     \item{`"zenith_n_azimuth"`}{Quadratic polynomial in zenith plus
#'       sinusoidal terms in azimuth.}
#'   }
#'
#' @return List with the following components:
#' \describe{
#'   \item{`fun`}{Function taking `zenith` and `azimuth` (degrees) and returning
#'     predicted relative radiance.}
#'   \item{`model`}{`lm` object fitted by [stats::lm()].}
#' }
#' Returns `NULL` (with a warning) if the number of input points is fewer than 20.
#'
#' @export
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
#' rr <- extract_rr(r, z, a, sky_points)
#'
#' model <- fit_coneshaped_model(rr$sky_points)
#' summary(model$model)
#' sky_cs <- model$fun(z, a) * rr$zenith_dn
#' plot(sky_cs)
#'
#' z_mini <- zenith_image(50, lens())
#' sky_cs <- model$fun(z_mini, azimuth_image(z_mini))
#' persp(sky_cs, theta = 90, phi = 20)
#'
#' }
fit_coneshaped_model <- function(sky_points,
                                 method = "zenith_n_azimuth") {
  if (!is.data.frame(sky_points)) {
    stop("`sky_points` must be a data frame.")
  }
  required_cols <- c("a", "z", "rr")
  if (!all(required_cols %in% names(sky_points))) {
    stop(sprintf("`sky_points` must contain columns %s.",
                 paste(sprintf('"%s"', required_cols), collapse = ", ")))
  }
  .assert_choice(method, c("zenith_only", "zenith_n_azimuth"))

  Blue <- sky_points$rr
  Zenith <- sky_points$z
  Azimuth <- sky_points$a

  if (length(Blue) >= 20) {
    if (method == "zenith_n_azimuth") {
      model <- lm(Blue ~ poly(Zenith, 2, raw = TRUE) +
        sin(Azimuth * pi / 180) + cos(Azimuth * pi / 180))

      # Only to avoid note from check, code is OK without this line.
      a <- b <- d <- e <- NA

      .sky_fun <- function(zenith, azimuth) {
        x <- coefficients(model)
        x[is.na(x)] <- 0
        for (i in 1:5) assign(letters[i], x[i])
        a + b * zenith + c * zenith^2 +
          d * sin(azimuth * pi / 180) + e * cos(azimuth * pi / 180)
      }
    } else {
      model <- lm(Blue ~ poly(Zenith, 2, raw = TRUE))
      .sky_fun <- function(zenith, azimuth) {
        x <- coefficients(model)
        x[is.na(x)] <- 0
        for (i in 1:5) assign(letters[i], x[i])
        a + b * zenith + c * zenith^2
      }
    }
    return(list(fun = .sky_fun, model = model))
  } else {
    warning("Insufficient number of points to attempt model fitting")
    return(NULL)
  }
}
