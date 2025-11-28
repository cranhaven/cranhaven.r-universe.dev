#' @title
#'  Estimate pipe friction factor with Vatankhah formula
#'
#' @family Fluid properties
#'
#' @description
#'  Estimate \emph{Darcy friction factor} explicitly with extremely accurate
#'  \emph{Vatankhah-Kouchakzadeh} approximation of \emph{Colebrook equation}.
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
#'  Vatankhah's formula is reported to be extremely accurate in the region:
#'  \itemize{
#'    \item \code{5.0e3 <= reynolds  <= 1.0e8}
#'    \item \code{1e-6  <= roughness <= 0.05}
#'  }
#'
#'  In \code{strict = TRUE} mode argument values outside this precision region
#'  are not allowed, whereas in \code{strict = FALSE} either \emph{NA}s are
#'  generated in that case or calculation for laminar flow is performed when
#'  \code{reynolds < 2100.0}.
#'
#' @references
#'   \enumerate{
#'      \item Offor, U. and Alabi, S. (2016) \emph{An Accurate and Computationally Efficient Explicit Friction Factor Model}.
#'  Advances in Chemical Engineering and Science, \emph{6}, pp. 237-245. \doi{10.4236/aces.2016.63024}.
#'
#'      \item Ali R. Vatankhah, Salah Kouchakzadeh (2009) \emph{Exact equations for pipe-flow problems}.
#'  Journal of Hydraulic Research, \emph{47:4}, pp. 537-538, DOI: \doi{10.1080/00221686.2009.9522031}
#'   }
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  fric_vatankhan(c(2118517, 2000, 2118517), c(1e-6, 70e-3/1, 7e-3/1))
#'  # [1] 0.01031665 0.03200000 0.03375210  # []
#'
#'  fric_vatankhan(c(2118517, 5500, 2118517), c(1e-6, 50e-3/1, 7e-3/1), TRUE)
#'  # [1] 0.01031665 0.07556163 0.03375210

fric_vatankhan <- function(reynolds, roughness = 0, strict = FALSE) {
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
    checkmate::assert_double(reynolds,  lower = 5.0e3, upper = 1.0e8)
    checkmate::assert_double(roughness, lower = 1e-6 , upper = 0.05)
  }

  G <- .124*reynolds*roughness + log(.4587*reynolds)
  f <- (.8686*log(.4587*reynolds/(G - .31)^(G/(G + .9633))))^-2
  if (strict) return(f)
  f[reynolds < 5.0e3 | reynolds > 1.0e8 | roughness < 1e-6 | roughness > 0.05] <- NA_real_
  laminar <- reynolds < 2.10e3
  f[laminar] <- 64/reynolds[laminar]
  f
}
