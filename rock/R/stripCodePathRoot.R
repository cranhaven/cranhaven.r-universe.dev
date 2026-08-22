#' Strip the root from a code path
#'
#' This function strips the root (just the first element) from a code path,
#' using the `codeTreeMarker` stored in the [rock::opts] object as marker.
#'
#' @param x A vector of code paths.
#'
#' @return The modified vector of code paths.
#' @export
#'
#' @examples stripCodePathRoot("codes>reason>parent_feels");
stripCodePathRoot <- function(x) {
  codeTreeMarker <- rock::opts$get("codeTreeMarker");
  return(
    gsub(
      paste0(
        "^[a-zA-Z0-9_]+",
        codeTreeMarker,
        "(.*)$"
        ),
      "\\1",
      x
    )
  );
}
