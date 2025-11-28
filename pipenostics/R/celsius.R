#' @title
#'  Convert to Celsius scale
#'
#' @family units
#'
#' @description
#'  Convert temperature measured in
#'  \href{https://en.wikipedia.org/wiki/Kelvin}{Kelvin}- or
#'  \href{https://en.wikipedia.org/wiki/Fahrenheit}{Fahrenheit}-scale
#'  to \href{https://en.wikipedia.org/wiki/Celsius}{Celsius} (\emph{°C}).
#'
#' @param x
#'  temperature in initial scale:
#'  \itemize{
#'    \item for \code{c_k(x)} - in \href{https://en.wikipedia.org/wiki/Kelvin}{Kelvin}-scale, [\emph{K}]
#'    \item for \code{c_f(x)} - in \href{https://en.wikipedia.org/wiki/Fahrenheit}{Fahrenheit}-scale, [\emph{°F}]
#'  }
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  temperature in \emph{Celsius}-scale, [\emph{°C}]. Type: \code{\link{assert_double}}.
#'
#' @seealso
#'  \code{\link{k_c}} and \code{\link{f_c}} for converting from Celsius-scale.
#'
#' @examples
#'  library(pipenostics)
#'
#' # Convert from Kelvin to Celsius:
#' c_k(c(0, 373.15))
#' # [1]  -273.15  100
#'
#' # Convert from Fahrenheit to Celsius:
#' c_f(c(-459.67, 212))
#' # [1]  -273.15  100
#'
#' @rdname celsius
#' @export
c_k <- function(x){
  checkmate::assert_double(
    x, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  x - 273.15
}

#' @rdname celsius
#' @export
c_f <- function(x){
  checkmate::assert_double(
    x, lower = -459.67, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  (x - 32)/1.8
}
