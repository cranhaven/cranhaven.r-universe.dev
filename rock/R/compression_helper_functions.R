#' Vector compression helper functions
#'
#' These functions can help when compressing vectors. They always compress
#' their input (`x`) into a single element by various means.
#'
#' `compress_with_sum` computes the sum of the elements, doing its best to
#' convert all input values to numeric values.
#' `compress_with_or` returns `0` if all elements are `FALSE`, `0`, `NA` or
#' empty character values (`""`), and `1` otherwise.
#'
#' @param x The vector to compress
#'
#' @return The compressed element
#' @rdname compression_helper_functions
#' @export
#'
#' @examples rock::compress_with_sum(c(1, '1', 0));
#' rock::compress_with_or(c(1, '1', 0));
#' rock::compress_with_or(c(0, '', 0, FALSE));
compress_with_sum <- function(x) {
  x <- convertToNumeric(x);
  return(sum(x, na.rm = TRUE));
}

#' @rdname compression_helper_functions
#' @export
compress_with_or <- function(x) {

  missings <- is.na(x);
  if (all(missings)) {
    return(0);
  }
  x <- x[!missings];

  noContents <- nchar(as.character(x)) == 0;
  if (all(noContents)) {
    return(0);
  }
  x <- x[!noContents];

  falses <- unlist(lapply(x, isFALSE));
  if (all(falses)) {
    return(0);
  }
  x <- x[!falses];

  zeroes <- convertToNumeric(x) == 0;
  zeroes <- zeroes[!is.na(zeroes)];
  if (all(zeroes)) {
    return(0);
  }
  x <- x[!zeroes];

  return(1);

}
