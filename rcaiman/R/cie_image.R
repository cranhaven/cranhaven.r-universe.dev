#' CIE sky model
#'
#' Written by Gaston Mauro Diaz based on Pascal code by Mait Lang.
#'
#' Angles should be provided in radians.
#'
#' @param AzP numeric vector. Azimuth angle of a sky point.
#' @param Zp numeric vector. Zenith Angle of a sky point.
#' @param AzS numeric vector of length one. Azimuth angle of the sun disc.
#' @param Zs numeric vector of length one. Zenith angle of the sun disc.
#' @param .a,.b,.c,.d,.e numeric vector of length one. Sky model parameter.
#'
#' @noRd
#' @references http://dx.doi.org/10.1016/j.energy.2016.02.054
#'
#' @return numeric vector of length equal to AzP length.
.cie_sky_model <- function(AzP, Zp, AzS, Zs, .a, .b, .c, .d, .e) {
  # calculate angular distance between sky point and Sun
  Chi <- calc_spherical_distance(Zp, AzP, Zs, AzS)

  # Gradation function
  Phi_Z <- 1 + .a * exp(.b / cos(Zp))
  Phi_0 <- 1 + .a * exp(.b)
  gradation <- Phi_Z / Phi_0

  # Indicatrix function
  F_Chi <-  1 + .c * (exp(.d * Chi) - exp(.d * pi/2)) + .e * cos(Chi)^2
  F_Zs  <-  1 + .c * (exp(.d *  Zs) - exp(.d * pi/2)) + .e * cos(Zs)^2
  indicatrix <- F_Chi / F_Zs

  unname(gradation * indicatrix)
}
.cie_sky_model <- compiler::cmpfun(.cie_sky_model)



#' CIE sky image
#'
#' Generate an image of relative radiance or luminance based on the CIE
#' General Sky model.
#'
#' @inheritParams sky_grid_segmentation
#' @inheritParams fit_cie_model
#'
#' @param sky_coef numeric vector of length five. Parameters of the CIE sky model.
#'
#' @return [terra::SpatRaster-class] with one layer whose pixel values
#'   represent relative luminance or radiance across the sky hemisphere,
#'   depending on whether the data used to obtain `sky_coef` was luminance
#'   or radiance.
#'
#' @note
#' Coefficient sets and formulation are available in [cie_table].
#'
#' @export
#'
#' @examples
#' z <- zenith_image(50, lens())
#' a <- azimuth_image(z)
#' sky_coef <- cie_table[4,1:5] %>% as.numeric()
#' sun_angles <- c(z = 45, a = 0)
#' plot(cie_image(z, a, sun_angles, sky_coef))
cie_image <- function(z, a, sun_angles, sky_coef) {
  .check_r_z_a_m(NULL, z, a)
  .check_vector(sun_angles, "numeric", 2, sign = "nonnegative")
  if (!identical(names(sun_angles), c("z", "a"))) {
    stop("`sun_angles` must be a named numeric vector of length two with names 'z' and 'a' in that order.")
  }
  .check_vector(sky_coef, "numeric", 5, sign = "any")

  Zp <- .degree2radian(z[])
  AzP <- .degree2radian(a[])

  Zs <- .degree2radian(sun_angles["z"])
  AzS <- .degree2radian(sun_angles["a"])

  relative_luminance <- .cie_sky_model(AzP, Zp, AzS, Zs,
                                       as.numeric(sky_coef[1]),
                                       as.numeric(sky_coef[2]),
                                       as.numeric(sky_coef[3]),
                                       as.numeric(sky_coef[4]),
                                       as.numeric(sky_coef[5]))
  terra::values(z) <- relative_luminance
  z[is.infinite(z)] <- 0
  names(z) <- "Relative radiance or luminance"
  z
}
