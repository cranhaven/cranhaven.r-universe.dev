#' @title
#'  ASME B31G. Allowable corrosion depth in pipe
#'
#' @family ASME B31G functions
#'
#' @description
#'  Calculate allowable depth of the corroded area in the pipe.
#'
#' @param dep
#'   design pressure of pipe, [\emph{PSI}]. Type: \code{\link{assert_double}}.
#'
#' @param maop
#'  maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}]. Type: \code{\link{assert_double}}.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of corroded area, [\emph{inch}].
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  allowable depth of the corroded area in the pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
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
#'  b31gacd(1093, 910, 30, .438, 7.5)
#'  # [1] 0.249  # [inch]
#'
b31gacd <- function(dep, maop, d, wth, l){
  checkmate::assert_double(
    dep, lower = 0, upper = 6e3, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    maop, lower = 25.4, upper = 1.27e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
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
    length(dep), length(maop),  length(d), length(wth), length(l)
  )))

  A <- b31gafr(d, wth, l)
  mcp <-
    ifelse(A > 4,
           wth - wth*maop/1.1/dep,
           1e-3*trunc(1e3*(maop - 1.1*dep)*3.0*wth*sqrt(A^2 + 1.0) /
             (2.0*maop - 2.2*dep*sqrt(A^2 + 1.0))))
  1e-3*trunc(1e3*ifelse(mcp > .8*wth, .8*wth, mcp) + .5)
}
