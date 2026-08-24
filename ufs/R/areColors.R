#' Check whether elements of a vector are valid colors
#'
#' This function by Josh O'Brien checks whether elements of a vector are valid
#' colors. It has been copied from a Stack Exchange answer (see
#' <https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation>).
#'
#'
#' @param x The vector.
#' @return A logical vector.
#' @author Josh O'Brien
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @keywords utilities
#' @examples
#'
#' ufs::areColors(c(NA, "black", "blackk", "1", "#00", "#000000"));
#'
#' @export areColors
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
             error = function(e) FALSE)
  })
}
