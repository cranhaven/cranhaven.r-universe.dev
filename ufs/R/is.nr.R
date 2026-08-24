#' `NULL` and `NA` 'proof' checking of whether something is a number
#'
#' Convenience function that returns TRUE if the argument is not null, not NA,
#' and is.numeric.
#'
#' @param x The value or vector to check.
#' @return TRUE or FALSE.
#'
#' @examples is.nr(8);    ### Returns TRUE
#' is.nr(NULL); ### Returns FALSE
#' is.nr(NA);   ### Returns FALSE
#'
#' @export is.nr
is.nr <- function(x) {
  if (!is.null(x)) {
    if (!is.na(x)) {
      if (is.numeric(x)) {
        return(TRUE);
      }
    }
  }
  return(FALSE);
}
