#' Out-of-the-box above-canopy sky
#'
#' @description
#' Generate an above‑canopy sky brightness map without manual tuning.
#'
#' @details
#' Interpolates sky brightness with IDW and k‑nearest neighbors in spherical
#' space via [interpolate_spherical()], blending observations with a fitted sky
#' model. Blending and IDW parameters are derived from `sky_cie` validation
#' metrics, and the result is scaled by the modeled zenith value to yield
#' digital numbers.
#'
#' @note
#' This function is part of a paper under preparation.
#'
#' @param sky_cie list. Output of [ootb_sky_cie()].
#' @inheritParams interpolate_spherical
#'
#' @return Named list with:
#' \describe{
#'   \item{`dn_raster`}{numeric [terra::SpatRaster-class] with interpolated
#'     above‑canopy sky brightness in digital numbers.}
#'   \item{`w`}{numeric. Weight assigned to the model‑based filling source.}
#'   \item{`k`}{integer. Number of nearest neighbors used by IDW.}
#'   \item{`p`}{numeric. IDW power parameter.}
#' }
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' caim <- read_caim()
#' z <- zenith_image(ncol(caim), lens())
#' a <- azimuth_image(z)
#'
#' path <- system.file("external/example.txt", package = "rcaiman")
#' sky_cie <- read_sky_cie(gsub(".txt", "", path), caim$Blue, z, a)
#'
#' sky_points <- sky_cie$model$rr$sky_points
#' sky_above <- ootb_sky_above(sky_points, z, a, sky_cie)
#' plot(sky_above$dn_raster)
#' plot(caim$Blue/sky_above$dn_raster)
#' }
ootb_sky_above <- function(sky_points, z, a, sky_cie, size = 100) {
  #basic checks handled by the functions called below

  # read metrics
  r2 <- sky_cie$model_validation$r_squared
  error <- stats::median(abs(1 - sky_cie$model_validation$obs /
                               sky_cie$model_validation$pred))

  # obtain parameters
  w <-  1 - stats::plogis(error, 0.042, 0.05) + stats::plogis(0, 0.042, 0.05)
  if (!is.numeric(r2)) r2 <- 0
  k <- 2 + round(r2 * 10)
  p <- abs(sky_cie$model$coef[1]) + log(sky_cie$model$coef[3]+1)
  p <- 2 + sqrt(p)
  p <- min(5, p)

  if (k > nrow(sky_points)) {
    stop("`k` must be greater than the number of rows in `sky_points`.")
  }

  # Lang et al. 2010
  sky <- interpolate_spherical(sky_points,
                               z,
                               a,
                               filling_source = sky_cie$rr_raster,
                               w = w,
                               k = k,
                               p = p,
                               angular_radius = 20,
                               rule = "any",
                               size = size)

  sky <- sky * sky_cie$model$rr$zenith_dn
  names(sky) <- paste(names(sky), "(digital numbers")

  list(dn_raster = sky,
       w = w,
       k=  k,
       p = unname(p))
}

