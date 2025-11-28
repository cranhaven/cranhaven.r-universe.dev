#' @title
#'  Inches to mm
#'
#' @family units
#'
#' @description
#'  Convert length measured in \href{https://en.wikipedia.org/wiki/Inch}{inches}
#'  to \href{https://en.wikipedia.org/wiki/Millimetre}{millimeters} (mm)
#'
#' @param x
#'  length measured in \emph{inches}, [\emph{inch}].
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  length in \emph{millimeters}, [\emph{mm}].
#'  Type: \code{\link{assert_double}}.
#'
#' @seealso
#'  \code{\link{inch_mm}} for converting \emph{mm} to \emph{inches}
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  mm_inch(c(0.03937008, 1))
#'  # [1]  1.0 25.4  # [mm]
#'
mm_inch <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE, min.len = 1L)
  25.4*x
}
