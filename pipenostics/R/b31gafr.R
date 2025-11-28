#' @title
#'  ASME B31G. A-factor
#'
#' @family ASME B31G functions
#'
#' @description
#'  Calculate intermediate factor related to the geometry of the corroded area.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of the corroded area, [\emph{inch}].
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  Intermediate factor related to the geometry of the corroded area, [].
#'  Type: \code{\link{assert_double}}.
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}.
#'  Manual for determining the remaining strength of corroded pipelines. A
#'  supplement to \emph{ASTME B31} code for pressure piping.
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  b31gafr(30, .438, 7.5)
#'  # [1] 1.847  # A-factor is less than 5, so the corrosion is not critical
#'
b31gafr <- function(d, wth, l){
  checkmate::assert_double(
    d, lower = 3.93e-2, upper = 1.27e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    wth, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    l, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(wth), length(l)
  )))

  1e-3*trunc(1e3*.893*l/sqrt(d*wth))
}
