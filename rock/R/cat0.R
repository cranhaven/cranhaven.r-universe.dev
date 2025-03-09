#' Concatenate to screen without spaces
#'
#' The cat0 function is to cat what paste0 is to paste; it
#' simply makes concatenating many strings without a separator easier.
#'
#' @param ... The character vector(s) to print; passed to [cat].
#' @param sep The separator to pass to [cat], of course, `""` by default.
#'
#' @return Nothing (invisible `NULL`, like [cat]).
#' @export
#'
#' @examples cat0("The first variable is '", names(mtcars)[1], "'.");
cat0 <- function(..., sep="") {
  return(cat(..., sep=sep));
}
