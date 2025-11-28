#' @title
#'  Megapascals to pounds per square inch
#'
#' @family units
#'
#' @description
#'  Convert pressure (stress) measured in \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{megapascals} (MPa)
#'  to \href{https://en.wikipedia.org/wiki/Pounds_per_square_inch}{pounds per square inch} (PSI)
#'
#' @param x
#'  pressure (stress) measured in \emph{megapascals}. [\emph{MPa}].
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  pressure (stress) in \emph{pounds per square inch},
#'  [\emph{PSI}]. Type: \code{\link{assert_double}}.
#'
#' @seealso
#'  \code{\link{mpa_psi}} for converting \emph{pounds per square inch} to \emph{megapascals}
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  psi_mpa(c(6.89475728e-3, 1))
#'  # [1] 1.0000 145.0377 # [PSI]
#'
psi_mpa <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE, min.len = 1L)
  145.03773800721814*x
}
