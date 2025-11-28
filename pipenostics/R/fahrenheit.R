#' @title
#'  Convert to Fahrenheit scale
#'
#' @family units
#'
#' @description
#'  Convert temperature measured in
#'  \href{https://en.wikipedia.org/wiki/Kelvin}{Kelvin}- or
#'  \href{https://en.wikipedia.org/wiki/Celsius}{Celsius}-scale
#'  to \href{https://en.wikipedia.org/wiki/Fahrenheit}{Fahrenheit} (\emph{°F}).
#'
#' @param x
#'  temperature in initial scale:
#'  \itemize{
#'    \item for \code{f_k(x)} - in \href{https://en.wikipedia.org/wiki/Kelvin}{Kelvin}-scale, [\emph{K}]
#'    \item for \code{f_c(x)} - in \href{https://en.wikipedia.org/wiki/Celsius}{Celsius}-scale, [\emph{°C}]
#'  }
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  temperature in \emph{Fahrenheit}-scale, [\emph{°F}]. Type: \code{\link{assert_double}}.
#'
#' @seealso
#'  \code{\link{k_f}} and \code{\link{c_f}} for converting from Fahrenheit-scale.
#'
#' @examples
#'  library(pipenostics)
#'
#' # Convert from Kelvin to Fahrenheit:
#' f_k(c(0, 373.15))
#' # [1]  -459.67 212
#'
#' # Convert from Celsius to Fahrenheit:
#' f_c(c(-273.15, 100))
#' # [1]  -459.67, 212
#'
#' @rdname fahrenheit
#' @export
f_k <- function(x){
  checkmate::assert_double(
    x, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  1.8 * x - 459.67
}

#' @rdname fahrenheit
#' @export
f_c <- function(x){
  checkmate::assert_double(
    x, lower = -273.15, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  1.8 * x + 32
}
