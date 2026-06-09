## --- base de datos de lentes ---
.LENSES <- list(
  equidistant = list(
    coef = c(2/pi),
    fov  = 180
  ),
  Nikkor_10.5mm = list(
    coef = c(0.716, 0.0115, -0.0393),
    fov  = 165
  ),
  Nikon_FCE9 = list(
    coef = c(0.643, 0.0346, -0.0245),
    fov  = 190
  ),
  Olloclip = list(
    coef = c(0.801, 0.178, -0.179),
    fov  = 172
  ),
  Nikkor_8mm = list(
    coef = c(0.689, 0.0131, -0.0295),
    fov  = 186
  )
)


#' Access the lens database
#'
#' @description
#' Retrieve projection coefficients and field-of-view (FOV, deg) for known
#' lenses. Coefficients expect zenith angle in radians and return relative
#' radius.
#'
#' @details
#' In upward-looking leveled hemispherical photography, the zenith corresponds
#' to the center of a circular image whose perimeter represents the horizon,
#' assuming a lens with a 180° field of view. The relative radius is the radial
#' distance to a given point, expressed as a fraction of the maximum radius
#' (i.e., the horizon). The equidistant projection model, also called polar
#' projection, is the standard reference model, characterized by a linear
#' relationship between zenith angle and relative radius.
#'
#' Real lenses deviate from ideal projections. Following
#' [Hemisfer software](https://www.schleppi.ch/patrick/hemisfer/), this package
#' models deviations with polynomial functions. All angular values are in
#' radians.
#'
#' Currently available lenses:
#' \describe{
#' \item{"equidistant"}{Ideal equidistant projection
#' \insertCite{Schneider2009}{rcaiman}.}
#' \item{"Nikkor_10.5mm"}{AF DX Fisheye Nikkor 10.5mm f/2.8G ED
#' \insertCite{Pekin2009}{rcaiman}.}
#' \item{"Nikon_FCE9"}{Nikon FC-E9 converter \insertCite{Diaz2024}{rcaiman}.}
#' \item{"Olloclip"}{Auxiliary lens for mobile devices manufactured by Olloclip
#' \insertCite{Diaz2024}{rcaiman}.}
#' \item{"Nikkor_8mm"}{AF–S Fisheye Nikkor 8–15mm f/3.5–4.5E ED
#' \insertCite{Diaz2024}{rcaiman}.}
#' }
#'
#' See [calibrate_lens()] for fitting new projection functions.
#'
#' @param type Character vector of length one. Lens identifier. See *Details*.
#' @param return Character vector of length one. Either `"coef"`(default) or
#'   `"fov"`. Controls whether to return projection coefficients or maximum FOV.
#'
#' @return Numeric vector. Depends on `return`:
#' \describe{
#'   \item{"coef"}{Polynomial coefficients of the projection function
#'     (relative radius as a function of `theta`, radians).}
#'   \item{"fov"}{numeric vector of length one. Maximum field of view (deg).}
#' }
#'
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' lens("Nikon_FCE9")
#' lens("Nikon_FCE9", return = "fov")
#'
#' .fp <- function(theta, lens_coef) {
#'   x <- lens_coef[1:5]
#'   x[is.na(x)] <- 0
#'   for (i in 1:5) assign(letters[i], x[i])
#'   a * theta + b * theta^2 + c * theta^3 + d * theta^4 + e * theta^5
#' }
#'
#' theta <- seq(0, pi/2, pi/180)
#' plot(theta, .fp(theta, lens()), type = "l", lty = 2,
#'       ylab = "relative radius")
#' lines(theta, .fp(theta, lens("Nikon_FCE9")))
#'
lens <- function(type = "equidistant", return = "coef") {
  .assert_choice(return, c("coef", "fov"), name = "return",
                 allow_null = FALSE, multiple = FALSE, case_sensitive = TRUE)

  if (!is.character(type) || length(type) != 1)
    stop("`type` must be a character vector of length one.", call. = FALSE)

  type <- trimws(type)
  .assert_choice(type, names(.LENSES), name = "type",
                 allow_null = FALSE, multiple = FALSE, case_sensitive = TRUE)

  # retorno
  x <- .LENSES[[type]]
  if (return == "coef") x$coef else x$fov
}

