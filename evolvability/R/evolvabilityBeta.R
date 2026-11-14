#' Calculate evolvability parameters along a set of selection gradients
#'
#' \code{evolvabilityBeta} calculates (unconditional) evolvability (e),
#' respondability (r), conditional evolvability (c), autonomy (a) and
#' integration (i) along selection gradients given an additive-genetic variance
#' matrix as described in Hansen and Houle (2008).
#'
#' @param G A variance matrix.
#' @param Beta Either a vector or a matrix of unit length selection gradients
#'   stacked column wise.
#' @param means An optional vector of trait means (for internal mean
#'   standardization).
#' @description \code{G} needs to be symmetric and positive definite.
#' @return An object of \code{class} \code{'evolvabilityBeta'}, which is a list
#' with the following components:
#' \tabular{llllll}{
#' \code{Beta} \tab\tab\tab\tab The matrix of selection gradients. \cr
#' \code{e} \tab\tab\tab\tab The evolvability of each selection gradient. \cr
#' \code{r} \tab\tab\tab\tab The respondability of each selection gradient. \cr
#' \code{c} \tab\tab\tab\tab The conditional evolvability of each selection
#' gradient. \cr
#' \code{a} \tab\tab\tab\tab The autonomy of each selection gradient. \cr
#' \code{i} \tab\tab\tab\tab The integration of each selection gradient.
#' }
#' @references Hansen, T. F. & Houle, D. (2008) Measuring and comparing
#' evolvability and constraint in multivariate characters. J. Evol. Biol.
#' 21:1201-1219.
#' @author Geir H. Bolstad
#' @examples
#' G <- matrix(c(1, 1, 0, 1, 2, 2, 0, 2, 3), ncol = 3) / 10
#' Beta <- randomBeta(5, 3)
#' X <- evolvabilityBeta(G, Beta)
#' summary(X)
#' @keywords array algebra
#' @export
evolvabilityBeta <- function(G, Beta, means = 1) {
  if (any(G[upper.tri(G)] != G[t(lower.tri(G))])) {
    stop("G is not symmetric.")
  }
  if (length(means) == 1 & means[1] == 1) {
    means <- rep(1, nrow(G))
  }
  G <- G / (means %*% t(means))
  Beta <- cbind(Beta)
  eB <- diag(t(Beta) %*% G %*% Beta)
  rB <- sqrt(diag(t(Beta) %*% (G %*% G) %*% Beta))
  cB <- 1 / diag(t(Beta) %*% solve(G) %*% Beta)
  aB <- cB / eB
  iB <- 1 - aB
  est <- list(Beta = Beta, 
              e = eB, 
              r = rB, 
              c = cB, 
              a = aB, 
              i = iB
              )
  class(est) <- "evolvabilityBeta"
  est$call <- match.call()
  est
}

#' Summarizing evolvability parameters over a set of selection gradients
#'
#' \code{summary} method for class \code{'evolvabilityBeta'}.
#'
#' @param object An object of class \code{'evolvabilityBeta'}.
#' @param ... Additional arguments.
#' @return A list with the following components:
#' \tabular{llllll}{
#' \code{Averages} \tab\tab\tab\tab The averages of the evolvability parameters 
#' over all selection gradients. \cr
#' \code{Minimum} \tab\tab\tab\tab The minimum of the evolvability parameters 
#' over all selection gradients. \cr
#' \code{Maximum} \tab\tab\tab\tab The maximum of the evolvability parameters 
#' over all selection gradients.
#' }
#' @author Geir H. Bolstad
#' @seealso \code{\link{evolvabilityBeta}}
#' @keywords array algebra
#' @export
summary.evolvabilityBeta <- function(object, ...) {
  X <- list()
  X$call <- object$call
  X$Averages <- c(
    e_mean = mean(object$e), 
    r_mean = mean(object$r), 
    c_mean = mean(object$c),
    a_mean = mean(object$a), 
    i_mean = mean(object$i)
  )
  X$Minimum <- c(
    e_min = min(object$e), 
    r_min = min(object$r), 
    c_min = min(object$c),
    a_min = min(object$a), 
    i_min = min(object$i)
  )
  X$Maximum <- c(
    e_max = max(object$e), 
    r_max = max(object$r), 
    c_max = max(object$c),
    a_max = max(object$a), 
    i_max = max(object$i)
  )
  class(X) <- "summary.evolvabilityBeta"
  X
}

#' @export
print.summary.evolvabilityBeta <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nAverage:\n")
  print(x$Averages)
  cat("\nMinimum:\n")
  print(x$Minimum)
  cat("\nMaximum:\n")
  print(x$Maximum)
}
