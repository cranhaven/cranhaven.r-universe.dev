#' @title Cross entropy
#' @description Cross entropy
#' @param px probability distribution
#' @param py probability distribution
#' @param epsilon small number to avoid log(0)
#' @return cross entropy
#' @export
crossH <- function(px, py, epsilon=1e-20) {
  px[px == 0] <- epsilon
  py[py == 0] <- epsilon
  px <- px / sum(px)
  py <- py / sum(py)
  return(-sum(px * log10(py)))
}