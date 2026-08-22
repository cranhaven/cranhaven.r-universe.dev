#' Padd a character vector
#'
#' @param x The character vector
#' @param width The width to pad to
#' @param padding The character to pad with
#'
#' @returns `x` with padding appended to each element
#' @export
#'
#' @examples padString(
#'   c("One",
#'     "Two",
#'     "Three"
#'   ),
#'   width = 7
#' );
padString <- function(x, width, padding = " ") {

  nchars <- nchar(x);
  padWidth <- width - nchars;

  return(paste0(x, repStr(n = padWidth, str = padding)));

}
