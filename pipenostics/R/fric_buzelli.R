#' @title
#'  Estimate pipe friction factor with Buzelli formula
#'
#' @family Fluid properties
#'
#' @description
#'  Estimate \emph{Darcy friction factor} explicitly with extremely accurate
#'  \emph{Buzelli} approximation of \emph{Colebrook equation}.
#'
#' @param reynolds
#'  Reynolds number, []. Type: \code{\link{assert_double}}.
#'
#' @param roughness
#'  relative roughness, []. Type: \code{\link{assert_double}}.
#'
#' @param strict
#'  calculate only inside the precision region. Type: \code{\link{assert_flag}}.
#'
#' @return
#'  pipe friction factor, []. Type: \code{\link{assert_double}}.
#'
#' @details
#'  Buzelli's formula is reported to be extremely accurate in the region:
#'  \itemize{
#'    \item \code{3.0e3 <= reynolds  <= 3.0e8}
#'    \item \code{    0 <= roughness <= 0.05}
#'  }
#'
#'  In \code{strict = TRUE} mode argument values outside this precision region
#'  are not allowed, whereas in \code{strict = FALSE} either \emph{NA}s are
#'  generated in that case or calculation for laminar flow is performed when
#'  \code{reynolds < 2100.0}.
#'
#' @references
#'  \enumerate{
#'   \item  Offor, U. and Alabi, S. (2016) \emph{An Accurate and Computationally Efficient Explicit Friction Factor Model}.
#'  Advances in Chemical Engineering and Science, \emph{6}, pp. 237-245. \doi{http://dx.doi.org/10.4236/aces.2016.63024}.
#'
#'   \item Buzzelli, D. (2008) \emph{Calculating friction in one step}. Machine Design, \emph{80} (12), pp. 54–55.
#' }
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  fric_buzelli(c(2118517, 2000, 2118517), c(1e-6, 70e-3/1, 7e-3/1))
#'  # [1] 0.01031468 0.03200000 0.03375076  # []
#'
#'  fric_buzelli(c(2118517, 5500, 2118517), c(1e-6, 50e-3/1, 7e-3/1), TRUE)
#'  # [1] 0.01031468 0.07556734 0.03375076

fric_buzelli <- function(reynolds, roughness = 0, strict = FALSE) {
  checkmate::assert_double(
    reynolds, lower = 64, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    roughness, lower = 0, upper = 1, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(reynolds), length(roughness)
  )))
  checkmate::assert_flag(strict)

  if (strict) {
    checkmate::assert_double(reynolds,  lower = 3.0e3, upper = 3.0e8)
    checkmate::assert_double(roughness, lower = 0 , upper = 0.05)
  }

  B1 <- (.774*log(reynolds) - 1.41)/(1 + 1.32*sqrt(roughness))
  B2 <- roughness/3.7 * reynolds + 2.51*B1
  f <- ( B1 - (B1 + 2*log10(B2/reynolds))/(1 + 2.18/B2) )^-2
  if (strict) return(f)
  f[reynolds < 3.0e3 | reynolds > 3.0e8 | roughness < 0 | roughness > 0.05] <- NA_real_
  laminar <- reynolds < 2.10e3
  f[laminar] <- 64/reynolds[laminar]
  f
}
