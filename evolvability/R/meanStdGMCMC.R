#' Mean standardize the posterior distribution of a G-matrix
#'
#' \code{meanStdGMCMC} mean standardizes the posterior distribution of a
#' variance matrix (e.g. a G-matrix)
#'
#' @param G_mcmc A posterior distribution of a variance matrix in the form of a
#'   table. Each row in the table must be one iteration of the posterior
#'   distribution (or bootstrap distribution). Each iteration of the matrix must
#'   be on the form as given by \code{c(x)}, where \code{x} is a matrix. A
#'   posterior distribution of a matrix in the slot \code{VCV} of a object of
#'   class \code{MCMCglmm} is by default on this form.
#' @param means_mcmc A posterior distribution of a vector of means in the form
#'   of a table. Each row in the table must be one iteration of the posterior
#'   distribution (or bootstrap distribution). A posterior distribution of a
#'   mean vector in the slot \code{Sol} of an object of class \code{MCMCglmm} is
#'   by default on this form.
#' @return The posterior distribution of a mean standardized variance matrix.
#' @author Geir H. Bolstad
#' @examples
#' # Simulating a posterior distribution
#' # (or bootstrap distribution) of a G-matrix:
#' G <- matrix(c(1, 1, 0, 1, 4, 1, 0, 1, 2), ncol = 3)
#' G_mcmc <- sapply(c(G), function(x) rnorm(10, x, 0.01))
#' G_mcmc <- t(apply(G_mcmc, 1, function(x) {
#'   G <- matrix(x, ncol = sqrt(length(x)))
#'   G[lower.tri(G)] <- t(G)[lower.tri(G)]
#'   c(G)
#' }))
#'
#' # Simulating a posterior distribution
#' # (or bootstrap distribution) of trait means:
#' means <- c(1, 1.4, 2.1)
#' means_mcmc <- sapply(means, function(x) rnorm(10, x, 0.01))
#'
#' # Mean standardizing the G-matrix:
#' meanStdGMCMC(G_mcmc, means_mcmc)
#' @keywords array algebra multivariate
#' @export
meanStdGMCMC <- function(G_mcmc, means_mcmc) {
  X <- cbind(means_mcmc, G_mcmc)
  n1 <- ncol(means_mcmc)
  n2 <- ncol(X)
  t(apply(X, 1, function(x) x[(n1 + 1):n2] / c(x[1:n1] %*% x[1:n1])))
}
