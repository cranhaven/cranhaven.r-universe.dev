#' Wrap all elements in a vector
#'
#' @param x The character vector
#' @param width The number of
#' @param sep The glue with which to combine the new lines
#' @param ... Other arguments are passed to [strwrap()].
#'
#' @return A character vector
#' @export
#'
#' @examples res <- wrapVector(
#'   c(
#'     "This is a sentence ready for wrapping",
#'     "So is this one, although it's a bit longer"
#'   ),
#'   width = 10
#' );
#'
#' print(res);
#' cat(res, sep="\n");
wrapVector <- function(x,
                       width = 0.9 * getOption("width"),
                       sep = "\n",
                       ...) {

  res <- strwrap(
    x = x,
    width = width,
    simplify = FALSE,
    ...
  );

  res <- lapply(
    res,
    paste,
    collapse = sep
  );

  res <-
    unlist(res);

  names(res) <- names(x);

  return(
    res
  );


}
