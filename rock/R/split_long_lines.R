#' Split long lines
#'
#' This function splits long lines at a given number of characters,
#' keeping words intact. It's basically a wrapper around [strwrap()].
#'
#' @param x The string (e.g. a source)
#' @param length The maximum length
#' @param collapseResult Whether to collapse the result from a vector (with
#' line breaks separating the elements) to a single character value (where
#' the vector elements are glued together using `splitString`) or not.
#' @param splitString The character to use to split lines.
#'
#' @return A character vector.
#' @export
#'
#' @examples cat(
#'   rock::split_long_lines(
#'     paste0(
#'       "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ",
#'       "Vestibulum et dictum urna. Donec neque nunc, lacinia vitae ",
#'       "varius vitae, pretium quis nibh. Aliquam pulvinar, lacus ",
#'       "sed varius vulputate, justo nibh blandit quam, ",
#'       "nec sollicitudin velit augue eget erat."
#'     )
#'   )
#' );
split_long_lines <- function(x,
                             length = 60,
                             collapseResult = FALSE,
                             splitString = rock::opts$get("utteranceMarker")) {

  res <- strwrap(x, width=length);

  if (collapseResult) {
    res <- paste0(res, collapse=splitString);
  }

  return(res);

}
