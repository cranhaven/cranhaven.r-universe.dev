#' @title Entropy of a discrete probability distribution
#' @description Entropy of a discrete probability distribution
#' @param px probability distribution
#' @param epsilon small number to avoid log(0)
#' @param normalized boolean to normalize entropy
#' @return entropy
#' @export
H <- function(px, epsilon = 1e-20, normalized = FALSE) {
if (inherits(px, "data.frame")) {
  px <- px[, 1]
}
  i0 <- px == 0
  if (sum(i0) > 0) {
    px[i0] <- epsilon
    px <- px / sum(px)
  }
  a <- -sum(px * log10(px))
  if (normalized) a <- a / log10(length(px))
  return(a)
}
