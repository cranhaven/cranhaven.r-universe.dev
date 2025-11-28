#' @title
#'  Covert to Kelvin scale
#'
#' @family units
#'
#' @description
#'  Convert temperature measured in
#'  \href{https://en.wikipedia.org/wiki/Celsius}{Celsius}- or
#'  \href{https://en.wikipedia.org/wiki/Fahrenheit}{Fahrenheit}-scale
#'  to \href{https://en.wikipedia.org/wiki/Kelvin}{Kelvin} (\emph{K}).
#'
#' @param x
#'  temperature in initial scale:
#'  \itemize{
#'    \item for \code{k_c(x)} - in \href{https://en.wikipedia.org/wiki/Celsius}{Celsius}-scale, [\emph{°C}]
#'    \item for \code{k_f(x)} - in \href{https://en.wikipedia.org/wiki/Fahrenheit}{Fahrenheit}-scale, [\emph{°F}]
#'  }
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  temperature in \emph{Kelvin}-scale, [\emph{K}]. Type: \code{\link{assert_double}}.
#'
#' @seealso
#'  \code{\link{c_k}} and \code{\link{f_k}} for converting from Kelvin-scale.
#'
#' @examples
#'  library(pipenostics)
#'
#' # Convert from Celsius to Kelvin:
#' k_c(c(-273.15, 100))
#' # [1]  0  373.15
#'
#' # Convert from Fahrenheit to Kelvin:
#' k_f(c(-459.67, 212))
#' # [1]  0  373.15
#'
#' @rdname kelvin
#' @export
k_c <- function(x){
  checkmate::assert_double(
    x, lower = -273.15, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  x + 273.15
}


#' @rdname kelvin
#' @export
k_f <- function(x){
  checkmate::assert_double(
    x, lower = -459.67, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  (x - 32)/1.8 + 273.15
}
