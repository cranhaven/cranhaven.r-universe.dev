#' @title
#'  Estimate pipe friction factor with Romeo's formula
#'
#' @family Fluid properties
#'
#' @description
#'  Estimate \emph{Darcy friction factor} explicitly with extremely accurate
#'  \emph{Romeo-Royo-Monzón} approximation of \emph{Colebrook equation}.
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
#'  Romeo's formula is reported to be extremely accurate in the region:
#'  \itemize{
#'    \item \code{3.0e3 <= reynolds  <= 1.5e8}
#'    \item \code{0.0   <= roughness <= 0.05}
#'  }
#'
#'  In \code{strict = TRUE} mode argument values outside this precision region
#'  are not allowed, whereas in \code{strict = FALSE} either \emph{NA}s are
#'  generated in that case or calculation for laminar flow is performed when
#'  \code{reynolds < 2100.0}.
#'
#' @references
#'   \enumerate{
#'    \item Offor, U. and Alabi, S. (2016) \emph{An Accurate and Computationally Efficient Explicit Friction Factor Model}.
#'  Advances in Chemical Engineering and Science, \emph{6}, pp. 237-245. \doi{10.4236/aces.2016.63024}.
#'
#'    \item Eva Romeo, Carlos Royo, Antonio Monzón, \emph{Improved explicit equations for estimation of friction factor in rough and smooth pipes},
#'  Chemical Engineering Journal, \emph{Volume 86}, Issue 3, 2002, Pages 369-374, ISSN 1385-8947. \doi{10.1016/S1385-8947(01)00254-6}.
#'  }
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  fric_romeo(c(2118517, 2000, 2118517), c(0, 70e-3/1, 7e-3/1))
#'  # [1] 0.01028473 0.03200000 0.03373215  # []
#'
#'  fric_romeo(c(2118517, 3030, 2118517), c(0, 50e-3/1, 7e-3/1), TRUE)
#'  # [1] 0.01028473 0.07859636 0.03373215  # []

fric_romeo <- function(reynolds, roughness = 0, strict = FALSE) {
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
    checkmate::assert_double(reynolds,  lower = 3.0e3, upper = 1.5e8)
    checkmate::assert_double(roughness, lower = 0.00 , upper = 0.05)
  }

  C <- (roughness/7.7918)^.9924 + (5.3326/(208.815 + reynolds))^.9345
  B <- roughness/3.827 - 4.567/reynolds*log10(C)
  A <- roughness/3.7065 - 5.0272/reynolds*log10(B)
  f <- .25/log10(A)^2
  if (strict) return(f)
  f[reynolds < 3.06e3 | reynolds > 1.50e8 | roughness > 0.05] <- NA_real_
  laminar <- reynolds < 2.10e3
  f[laminar] <- 64/reynolds[laminar]
  f
}
