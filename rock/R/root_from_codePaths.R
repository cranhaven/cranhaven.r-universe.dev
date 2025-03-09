#' Get the roots from a vector with code paths
#'
#' @param x A vector of code paths.
#'
#' @return A vector with the root of each element.
#' @export
#'
#' @examples root_from_codePaths(
#'   c("codes>reason>parent_feels",
#'     "codes>reason>child_feels")
#' );
root_from_codePaths <- function(x) {
  codeTreeMarker <- rock::opts$get("codeTreeMarker");
  roots <-
    gsub(
      paste0(
        "^([a-zA-Z0-9_]+)",
        codeTreeMarker,
        ".*$"
      ),
      "\\1",
      x
    );
  res <- roots;
  names(res) <- x;
  return(res);
}
