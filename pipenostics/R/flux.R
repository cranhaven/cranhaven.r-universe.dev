#' @title
#'  Convert heat flux to specific heat loss power
#'
#' @family units
#'
#' @description
#'  Convert \href{https://en.wikipedia.org/wiki/Heat_flux}{heat flux}
#'  measured for a cylindrical steel pipe to \emph{specific heat loss power} of pipe.
#'
#' @param x
#'  value of
#'  \itemize{
#'    \item \emph{heat flux}, [\emph{W/m^2}], for \code{loss_flux(x, d, wth)}
#'    \item \emph{specific heat loss power}, [\emph{kcal/m/h}], for \code{flux_loss(x, d, wth)(x)}
#'  }
#'  Type: \code{\link{assert_double}}.
#'
#' @param d
#'  outside (if \emph{wth = 0}) or internal (if \emph{wth > 0}) diameter of cylindrical pipe, [\emph{m}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param wth
#'  wall thickness of pipe, [\emph{mm}], or \emph{0} if argument \emph{d} is an outside diameter of pipe.
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  value of
#'  \itemize{
#'    \item \emph{specific heat loss power}, [\emph{kcal/m/h}], for \code{loss_flux(x, d, wth)}
#'    \item \emph{heat flux}, [\emph{W/m^2}], for \code{flux_loss(x, d, wth)(x)}
#'  }
#'  Type: \code{\link{assert_double}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # Consider pipes:
#' diameter      <- c(998, 1395)  # [mm]
#' wall_thikness <- c(  2,    5)  # [mm]
#'
#' # Then maximum possible normative neat loss according (Minenergo-325) for
#' # these pipe diameters are
#' loss_max <- c(218, 1040)  # [kcal/m/h]
#'
#' # The appropriate flux is
#' flux <- flux_loss(loss_max, diameter * 1e-3, wall_thikness)
#' print(flux)
#'
#' # [1] 80.70238 275.00155  # [W/m^2]
#'
#' stopifnot(
#'   all.equal(loss_flux(flux, diameter * 1e-3, wall_thikness), loss_max, tolerance = 5e-6)
#' )
#'
#'
#' @rdname flux
#' @export
loss_flux <- function(x, d, wth = 0){
  checkmate::assert_double(x, lower = 0, upper = 6e4)
  checkmate::assert_double(d, lower = 10e-3, upper = 5, any.missing = FALSE)
  checkmate::assert_double(wth, lower = 0, upper = .35 * max(d) * 1e3, any.missing = FALSE)

  h <- 3600    # [s/h]
  J <- 4186.8  # [J/kcal]
  flux <- x    # [W/m^2]
  loss <- h/J * flux * base::pi * (d + wth * 1e-3)  # [kcal/m/h]
  loss
}

#' @rdname flux
#' @export
flux_loss <- function(x, d, wth = 0){
  checkmate::assert_double(x, lower = 0, upper = 1500)
  checkmate::assert_double(d, lower = 10e-3, upper = 5, any.missing = FALSE)
  checkmate::assert_double(wth, lower = 0, upper = .35 * max(d) * 1e3, any.missing = FALSE)

  h <- 3600    # [s/h]
  J <- 4186.8  # [J/kcal]
  loss <- x    # [kcal/m/h]
  flux <- loss * J/h / base::pi / (d + wth * 1e-3)  # [W/m^2]
  flux
}
