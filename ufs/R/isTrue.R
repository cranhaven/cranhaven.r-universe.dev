#' More flexible version of isTRUE
#'
#' Returns `TRUE` for `TRUE` elements, `FALSE` for `FALSE` elements,
#' and whatever is specified in `na` for `NA` items.
#'
#' @param x The vector to check for `TRUE`, `FALSE`, and `NA` values.
#' @param na What to return for `NA` values.
#'
#' @return A logical vector.
#' @examples isTrue(c(TRUE, FALSE, NA));
#' isTrue(c(TRUE, FALSE, NA), na=TRUE);
#'
#' @export
isTrue <- function(x, na = FALSE) {
  naValues <- ifelse(rep(na, length(x)),
                     is.na(x),
                     rep(FALSE, length(x)));
  return(ifelse(is.na(x), naValues, x==TRUE));
}
