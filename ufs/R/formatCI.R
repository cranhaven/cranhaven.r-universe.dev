#' Pretty formatting of confidence intervals
#'
#' @param ci A confidence interval (a vector of 2 elements; longer
#'   vectors work, but I guess that wouldn't make sense).
#' @param sep The separator of the values, usually "; " or ", ".
#' @param prefix,suffix The prefix and suffix, usually a type of
#'   opening and closing parenthesis/bracket.
#' @param digits The number of digits to which to round the values.
#' @param noZero Whether to strip the leading zero (before the decimal
#'   point), as is typically done when following APA style and
#'   displaying correlations, *p* values, and other numbers that cannot
#'   reach 1 or more.
#'
#' @return A character vector of one element.
#' @seealso [noZero()], [formatR()], [formatPvalue()]
#' @export
#'
#' @examples ### With leading zero ...
#' formatCI(c(0.55, 0.021));
#'
#' ### ... and without
#' formatCI(c(0.55, 0.021), noZero=TRUE);

formatCI <- function(ci,
                     sep='; ',
                     prefix='[',
                     suffix=']',
                     digits=2,
                     noZero=FALSE) {
  if (noZero) {
    return(paste0(prefix,
                  paste0(ufs::noZero(round(ci, digits)),
                         collapse=sep), suffix));
  } else {
    return(paste0(prefix,
                  paste0(round(ci, digits),
                         collapse=sep), suffix));
  }
}
