#' Get indices of YAML delimiters
#'
#' @param x The character vector.
#'
#' @return A numeric vector.
#'
#' @examples yaml_delimiter_indices(
#'   c("not here",
#'     "---",
#'     "above this one",
#'     "but nothing here",
#'     "below this one, too",
#'     "---")
#' );
#' ### [1] 2 6
#' @export
yaml_delimiter_indices <- function(x) {
  return(grep("^\\s*\\-\\-\\-\\s*$", x));
}
