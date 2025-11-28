#' @title
#'  ASME B31G. Operational status of pipe
#'
#' @family ASME B31G functions
#'
#' @description
#'  Determine the operational status of pipe: is it excellent? or is
#'  technological control required? or is it critical situation?
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param depth
#'  measured maximum depth of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  Operational status of pipe as an integer value:
#'  \itemize{
#'    \item \emph{1} - excellent
#'    \item \emph{2} - monitoring is recommended
#'    \item \emph{3} - alert! replace the pipe immediately!
#'  }
#'  Type: \code{\link{assert_numeric}} and \code{\link{assert_subset}}.
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
#'  b31gops(.438, .1)
#'  # [1] 2  # typical status for the most of pipes
#'
#'  b31gops(.5, .41)
#'  # [1] 3  # alert! Corrosion depth is too high! Replace the pipe!
#'
b31gops <- function(wth, depth){
  checkmate::assert_double(
    wth, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    depth, lower = 0, upper = 2.54e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_true(commensurable(c(length(wth), length(depth))))

  a <- .8*wth  # alert setting
  as.integer(1 + (depth > .1*wth & depth <= a) + 2*(depth > a))
}
