#' Mean standardize a variance matrix
#'
#' \code{meanStdG} mean standardizes a variance matrix (e.g. a G-matrix).
#'
#' @param G A variance matrix.
#' @param means A vector of trait means.
#' @return A mean standardized variance matrix.
#' @author Geir H. Bolstad
#' @examples
#' G <- matrix(c(1, 1, 0, 1, 4, 1, 0, 1, 2), ncol = 3)
#' means <- c(1, 1.4, 2.1)
#' meanStdG(G, means)
#' @keywords array algebra
#' @export
meanStdG <- function(G, means) {
  G / (means %*% t(means))
}
