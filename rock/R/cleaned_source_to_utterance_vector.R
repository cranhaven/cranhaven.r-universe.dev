#' Convert a character vector into an utterance vector
#'
#' Utterance vectors are split by the utterance marker. Note that if `x` has
#' more than one element, the separate elements will remain separate.
#'
#' @param x The character vector.
#' @param utteranceMarker The utterance marker (by default, a newline
#' character conform the ROCK standard).
#' @param fixed Whether the `utteranceMarker` is a regular expression.
#' @param perl If the `utteranceMarker` is a regular expression, whether it is
#' a perl regular expression.
#'
#' @return A character vector with separate utterances, split
#' by `utteranceMarker`.
#' @export
#'
#' @examples cleaned_source_to_utterance_vector("first\nsecond\nthird");
cleaned_source_to_utterance_vector <- function(x,
                                               utteranceMarker = rock::opts$get("utteranceMarker"),
                                               fixed = FALSE,
                                               perl = TRUE) {
  x <- paste(x,
             collapse=utteranceMarker);
  res <-
    strsplit(x,
             split = utteranceMarker,
             fixed = fixed,
             perl = perl)[[1]];

  return(res);
}
