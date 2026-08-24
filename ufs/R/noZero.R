#' Remove one or more zeroes before the decimal point
#'
#' @param str The character string to process.
#'
#' @return The processed string.
#' @seealso [formatCI()], [formatR()], [formatPvalue()]
#' @export
#'
#' @examples noZero("0.3");

noZero <- function (str) {
  return(gsub("0*\\.", ".", str));
}
