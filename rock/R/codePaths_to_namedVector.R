#' Get a vector to find the full paths based on the leaf code identifier
#'
#' This function names a vector with the leaf code
#' using the `codeTreeMarker` stored in the [rock::opts] object as marker.
#'
#' @param x A vector of code paths.
#'
#' @return The named vector of code paths.
#' @export
#'
#' @examples codePaths_to_namedVector(
#'   c("codes>reason>parent_feels",
#'     "codes>reason>child_feels")
#' );
codePaths_to_namedVector <- function(x) {
  if (is.null(x)) {
    return(x);
  }
  codeTreeMarker <- rock::opts$get("codeTreeMarker");
  leafs <-
    gsub(
      paste0(
        "^.*",
        codeTreeMarker,
        "([a-zA-Z0-9_]+)$"
      ),
      "\\1",
      x
    );
  res <- x;
  names(res) <- leafs;
  return(res);
}
