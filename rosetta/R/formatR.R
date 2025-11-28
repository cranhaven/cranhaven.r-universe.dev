#' Pretty formatting of correlation coefficients
#'
#' @param r The Pearson correlation to format.
#' @param digits The number of digits to round to.
#'
#' @return The formatted correlation.
#' @seealso [noZero()], [formatCI()], [formatPvalue()]
#' @export
#'
#' @examples formatR(cor(mtcars$mpg, mtcars$disp));

formatR <- function (r, digits = 2) {
  return(ufs::noZero(round(r,
                           digits = digits)));
}
