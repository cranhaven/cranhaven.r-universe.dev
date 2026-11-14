#' Calculate response differences along a set of selection gradients
#'
#' \code{responsdiffBeta} calculates the response difference along selection 
#' gradients between two additive-genetic variance matrices as described in 
#' Hansen and Houle (2008).
#'
#' @param G1 A variance matrix.
#' @param G2 A variance matrix.
#' @param Beta Either a vector or a matrix of unit length selection gradients
#'   stacked column wise.
#' @param means1 An optional vector of trait means of G1 (for internal mean
#'   standardization).
#' @param means2 An optional vector of trait means of G2 (for internal mean
#'   standardization).
#' @description \code{G1} and \code{G2} need to be symmetric and positive definite.
#' @return An object of \code{class} \code{'responsdiffBeta'}, which is a list
#' with the following components:
#' \tabular{llllll}{
#' \code{Beta} \tab\tab\tab\tab The matrix of selection gradients. \cr
#' \code{d} \tab\tab\tab\tab The response difference of each selection gradient.
#' }
#' @references Hansen, T. F. & Houle, D. (2008) Measuring and comparing
#' evolvability and constraint in multivariate characters. J. Evol. Biol.
#' 21:1201-1219.
#' @author Geir H. Bolstad
#' @examples
#' G1 <- matrix(c(1, 1, 0, 1, 2, 2, 0, 2, 3), ncol = 3) / 10
#' G2 <- matrix(c(1, 2, 0,2, 1, 1,0, 1, 3), ncol = 3)
#' Beta <- randomBeta(5, 3)
#' X <- responsediffBeta(G1, G2, Beta)
#' summary(X)
#' @keywords array algebra
#' @export
#' 

responsediffBeta <- function(G1, G2, Beta, means1 = 1, means2 = 1) {
  if (any(G1[upper.tri(G1)] != G1[t(lower.tri(G1))])) {
    stop("G1 is not symmetric.")
  }
  if (any(G2[upper.tri(G2)] != G2[t(lower.tri(G2))])) {
    stop("G2 is not symmetric.")
  }
  if (length(means1) == 1 & means1[1] == 1) {
    means1 <- rep(1, nrow(G1))
  }
  if (length(means2) == 1 & means1[1] == 1) {
    means2 <- rep(1, nrow(G2))
  }
  G1 <- G1 / (means1 %*% t(means1))
  G2 <- G2 / (means1 %*% t(means2))
  Beta <- cbind(Beta)
  dB <- sqrt(diag(t(Beta) %*% (G1 - G2) %*% (G1 - G2) %*% Beta))
  est <- list(Beta = Beta, 
              d = dB
  )
  class(est) <- "responsediffBeta"
  est$call <- match.call()
  est
}

#' Summarizing response differences over a set of selection gradients
#'
#' \code{summary} method for class \code{'responsediffBeta'}.
#'
#' @param object An object of class \code{'responsediffBeta'}.
#' @param ... Additional arguments.
#' @return A list with the following components:
#' \tabular{llllll}{
#' \code{Averages} \tab\tab\tab\tab The averages of the response differences 
#' over all selection gradients. \cr
#' \code{Minimum} \tab\tab\tab\tab The minimum of the response differences 
#' over all selection gradients. \cr
#' \code{Maximum} \tab\tab\tab\tab The maximum of the response differences 
#' over all selection gradients.
#' }
#' @author Geir H. Bolstad
#' @seealso \code{\link{responsediffBeta}}
#' @keywords array algebra
#' @export
summary.responsediffBeta <- function(object, ...) {
  X <- list()
  X$call <- object$call
  X$Averages <- c(
    d_mean = mean(object$d)
  )
  X$Minimum <- c(
    d_min = min(object$d)
  )
  X$Maximum <- c(
    d_max = max(object$d)
  )
  class(X) <- "summary.responsediffBeta"
  X
}

#' @export
print.summary.responsediffBeta <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nAverage:\n")
  print(x$Averages)
  cat("\nMinimum:\n")
  print(x$Minimum)
  cat("\nMaximum:\n")
  print(x$Maximum)
}
