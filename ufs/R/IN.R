#' Case insensitive version of %in%
#'
#' This is simply 'in', but applies [base::toupper()] to both
#' arguments, first.
#'
#' @param find The element(s) to look up in the vector or matrix.
#' @param table The vector or matrix in which to look up the element(s).
#'
#' @usage find \%IN\% table
#'
#' @return A logical vector.
#' @rdname IN
#' @aliases %IN%
#' @examples letters[1:4] %IN% LETTERS
#'
#' @export
"%IN%" <- function(find, table) {
  return(toupper(find) %in% toupper(table));
}
