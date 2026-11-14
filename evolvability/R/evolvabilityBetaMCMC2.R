#' Calculate posterior distribution of evolvability parameters from a selection
#' gradient estimated with uncertainty
#'
#' \code{evolvabilityBetaMCMC2} calculates (unconditional) evolvability (e),
#' respondability (r), conditional evolvability (c), autonomy (a) and
#' integration (i) along a selection gradient estimate with uncertainty.
#'
#' @param G_mcmc A posterior distribution of a variance matrix in the form of a
#'   table. Each row in the table must be one iteration of the posterior
#'   distribution (or bootstrap distribution). Each iteration of the matrix must
#'   be on the form as given by \code{c(x)}, where \code{x} is a matrix. A
#'   posterior distribution of a matrix in the slot \code{VCV} of a object of
#'   class \code{MCMCglmm} is by default on this form.
#' @param Beta_mcmc A posterior distribution of a unit length selection gradient
#'   where iterations are given row wise.
#' @param post.dist logical: should the posterior distribution of the evolvability
#'  parameters be saved.
#' \tabular{llllll}{
#' \code{Beta.median} \tab\tab\tab\tab posterior median and highest posterior
#'  density interval of the selection gradient. \cr
#' \code{summary} \tab\tab\tab\tab The posterior median and highest posterior
#'  density interval of evolvability parameters. \cr
#' \code{post.dist} \tab\tab\tab\tab The full posterior distributions of the
#'  evolvability parameters.
#' }
#' @references Hansen, T. F. & Houle, D. (2008) Measuring and comparing
#'  evolvability and constraint in multivariate characters. J. Evol. Biol.
#'   21:1201-1219.
#' @author Geir H. Bolstad
#' @examples
#' {
#'   # Simulating a posterior distribution
#'   # (or bootstrap distribution) of a G-matrix:
#'   G <- matrix(c(1, 1, 0, 1, 4, 1, 0, 1, 2), ncol = 3)
#'   G_mcmc <- sapply(c(G), function(x) rnorm(10, x, 0.01))
#'   G_mcmc <- t(apply(G_mcmc, 1, function(x) {
#'     G <- matrix(x, ncol = sqrt(length(x)))
#'     G[lower.tri(G)] <- t(G)[lower.tri(G)]
#'     c(G)
#'   }))
#'
#'   # Simulating a posterior distribution
#'   # (or bootstrap distribution) of trait means:
#'   means <- c(1, 1.4, 2.1)
#'   means_mcmc <- sapply(means, function(x) rnorm(10, x, 0.01))
#'
#'   # Mean standardizing the G-matrix:
#'   G_mcmc <- meanStdGMCMC(G_mcmc, means_mcmc)
#'
#'   # Simulating a posterior distribution (or bootstrap distribution)
#'   # of a unit length selection gradient:
#'   Beta <- randomBeta(1, 3)
#'   Beta.mcmc <- sapply(c(Beta), function(x) rnorm(10, x, 0.01))
#'   Beta.mcmc <- t(apply(Beta.mcmc, 1, function(x) x / sqrt(sum(x^2))))
#'
#'   # Running the model:
#'   evolvabilityBetaMCMC2(G_mcmc, Beta_mcmc = Beta.mcmc, post.dist = TRUE)
#' }
#' @keywords array algebra multivariate
#' @importFrom stats median
#' @export
evolvabilityBetaMCMC2 <- function(G_mcmc, Beta_mcmc, post.dist = FALSE) {
  G_Beta <- cbind(G_mcmc, Beta_mcmc)
  dimG <- sqrt(ncol(G_mcmc))
  X1 <- t(apply(G_Beta, 1, function(GB) {
    G <- matrix(GB[1:(dimG^2)], ncol = dimG)
    B <- cbind(GB[(dimG^2 + 1):ncol(G_Beta)])
    eB <- t(B) %*% G %*% B
    rB <- sqrt(t(B) %*% (G %*% G) %*% B)
    cB <- 1 / (t(B) %*% solve(G) %*% B)
    aB <- cB / eB
    iB <- 1 - aB
    c(eB = eB, rB = rB, cB = cB, aB = aB, iB = iB)
  }))
  X <- list()
  X$Beta.median <- cbind(median = apply(Beta_mcmc, 2, median), 
                         coda::HPDinterval(coda::mcmc(Beta_mcmc))
                         )
  X$summary <- cbind(median = apply(X1, 2, median), 
                     coda::HPDinterval(coda::mcmc(X1))
                     )
  if (post.dist == TRUE) X$post.dist <- X1
  X$call <- match.call()
  class(X) <- "evolvabilityBetaMCMC2"
  return(X)
}

#' @export
print.evolvabilityBetaMCMC2 <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nEvolvability parameters, posterior medians and 95% HPD intervals:\n")
  print(x$summary)
}
