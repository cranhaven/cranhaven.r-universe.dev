#' @title 
#'  Estimate Reynolds number
#'
#' @family Fluid properties
#'
#' @description
#'  Estimate \emph{Reynolds number} for fluid flow in a cylindrical pipe.
#'
#' @param d
#'  internal diameter of pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param mu
#'  dynamic viscosity of fluid in pipe, [\emph{kg/m/s}]. Type: \code{\link{assert_double}}.
#'
#' @param u
#'  mean velocity of fluid  in pipe, [\emph{m/s}]. Type: \code{\link{assert_double}}.
#'
#' @param rho
#'  mass density of fluid  in pipe, [\emph{kg/m^3}]. Type: \code{\link{assert_double}}.
#'
#' @param v
#'  volumetric flow rate of fluid in pipe, [\emph{m^3/s}]. Type: \code{\link{assert_double}}.
#'
#' @param m
#'   mass flow rate of fluid in pipe, [\emph{kg/s}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  \emph{Reynolds number} - a dimensionless quantity that reveals the ratio
#'  between inertial and viscous forces in the fluid, []. Type: \code{\link{assert_double}}.
#'
#' @details
#'  The calculation of \emph{Reynolds number} is bounded by physically
#'  reasonable limits of fluid properties found in domain specificity of
#'  the package.
#'
#' @examples
#'  library(pipenostics)
#'
#'  # Reynolds numbers for typical district heating water flows at temperature
#'  # near 25 C in a set of pipes with different sizes:
#'  range(re_u(seq(.25, 1, 0.05), .89, 1, 1000))
#'  # [1]  280.8989 1123.5955
#'
#' @rdname reynolds
#' @export
re_u <- function(d, mu, u, rho){
  checkmate::assert_double(
    d, lower = 10e-3, upper = 2, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    mu, lower =  1.0e-8, upper = 2, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    rho, lower =  0.0, upper = 2000, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    u,   lower =  0.0, upper = 10, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(mu), length(u), length(rho)
  )))

  d/mu * rho * u  ## [m]/[kg/m/s] * [kg/m^3] * [m/s] = []
}

#' @rdname reynolds
#' @export
re_v <- function(d, mu, v, rho){
  checkmate::assert_double(
    d, lower = 10e-3  , upper = 2, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    mu, lower =  1.0e-8, upper = 2, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    v, lower =  0.0, upper =    5, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    rho, lower =  0.0, upper = 2000, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(mu), length(v), length(rho)
  )))

  a <- .25*pi*d^2  ## [m^2]
  d/mu/a * rho * v  ## [m]/[kg/m/s]/[m^2] * [kg/m^3] * [m^3/s] = []
}

#' @rdname reynolds
#' @export
re_m <- function(d, mu, m){
  checkmate::assert_double(
    d, lower = 10e-3, upper = 2, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    mu, lower = 1.0e-8, upper = 2, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(mu), length(m)
  )))

  a <- .25*pi*d^2  ## [m^2]
  d/mu/a * m  ## [m]/[kg/m/s]/[m^2] * [kg/s] = []
}


