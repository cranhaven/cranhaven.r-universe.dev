### http://www.real-statistics.com/reliability/split-half-methodology/spearman-browns-predicted-reliability/

#' Spearman-Brown formula
#'
#' @param nrOfItems Number of items (or 'subtests') in the scale (or 'test').
#' @param itemReliability The reliability of one item (or 'subtest').
#' @param scaleReliability The reliability of the scale (or, desired reliability of the scale).
#'
#' @return For `spearmanBrown`, the predicted scale reliability; for `spearmanBrown_requiredLength`,
#' the number of items required to achieve the desired scale reliability; and for `spearmanBrown_reversed`,
#' the reliability of one item.
#' @export
#' @rdname spearmanBrown
#'
#' @examples spearmanBrown(10, .4);
#' spearmanBrown_reversed(10, .87);
#' spearmanBrown_requiredLength(.87, .4);
spearmanBrown <- function(nrOfItems, itemReliability) {
  return((nrOfItems * itemReliability) / (1 + (nrOfItems-1) * itemReliability));
}

#' @rdname spearmanBrown
#' @export
spearmanBrown_reversed <- function(nrOfItems, scaleReliability) {
  return(scaleReliability / (nrOfItems - (nrOfItems-1) * scaleReliability));
}

#' @rdname spearmanBrown
#' @export
spearmanBrown_requiredLength <- function(scaleReliability, itemReliability) {
  return((scaleReliability * (1 - itemReliability)) / (itemReliability * (1 - scaleReliability)));
}

