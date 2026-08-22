#' Specify that something is a source
#'
#' This function converts an object to a character vector and marks it as
#' a ROCK source.
#'
#' @param x The source contents.
#'
#' @returns A character vector with class `rock_source`.
#' @export
#'
#' @examples exampleROCK <-
#'   rock::as.rock_source(c(
#'     "Some example text,",
#'     "and some more.       [[look_a_code]]",
#'     "And the end."));
#'
#' ### This can then be processed by other {rock}
#' ### functions, for example:
#' rock::prettify_source(
#'   exampleROCK
#' );
as.rock_source <- function(x) {

  res <- as.character(x);

  class(res) <- c("rock_source", "character");

  return(res);

}
