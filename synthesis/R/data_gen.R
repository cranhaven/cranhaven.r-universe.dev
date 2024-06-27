#' Generate correlated uniform variates
#'
#' @param n       The data length to be generated.
#' @param ndim    The number of potential predictors (default is 9).
#' @param r       The target correlation, default is 0.6.
#' @param sigma   A symmetric matrix of Pearson correlation, should be same as ndim.
#' @param method  The target correlation type, inluding Pearson and Spearman correlation.
#'
#' @return A matrix of correlated uniform variates
#'
#' @references Schumann, E. (2009). Generating correlated uniform variates. COMISEF. http://comisef. wikidot. com/tutorial: correlateduniformvariates.
#'
data.gen.unif <- function(n, ndim = 9, r = 0.6, sigma, method = c("pearson", "spearman")) {

    dp <- matrix(NA, n, ndim)

    if (missing(sigma)) {
        # Pearson correlation
        if (method == "spearman")
            rho <- 2 * sin(r * pi/6) else rho <- r

        # Correlation matrix
        sigma <- matrix(rep(rho, ndim * ndim), ndim)
        diag(sigma) <- 1
    } else {
        if (ndim != ncol(sigma))
            stop("The dimension of sigma is different from ndim!")
    }

    # Generate sample
    dp <- pnorm(matrix(rnorm(n * ndim), ncol = ndim) %*% chol(sigma))  # from stats and base package

    return(dp)
}

#---------------------------------------------------------------------------
#' Generate correlated normal variates
#'
#' @param n     The data length to be generated.
#' @param mu    A vector giving the means of the variables.
#' @param sd    A vector giving the standard deviation of the variables.
#' @param r     The target Pearson correlation, default is 0.6.
#' @param sigma A positive-definite symmetric matrix specifying the covariance matrix of the variables.
#'
#' @return A matrix of correlated normal variates
#' @importFrom MASS mvrnorm
#'
data.gen.norm <- function(n, mu = rep(0, 2), sd = rep(1, 2), r = 0.6, sigma) {
    # n<-1000;mu=rep(0,2); sd=rep(1,2); ndim=2;r=0.6
    dp <- matrix(NA, n, length(mu))

    if (missing(sigma)) {

        if (length(mu) != length(sd))
            stop("The dimension of sd is different from mu!")

        # Covariance matrix
        sigma <- matrix(rep(r, length(sd) * length(sd)), length(sd))
        diag(sigma) <- 1
        sigma <- sigma * prod(sd)

    } else {
        if (length(mu) != ncol(sigma))
            stop("The dimension of sigma is different from mu!")
    }

    # Generate sample
    dp <- MASS::mvrnorm(n, mu, sigma)

    return(dp)
}
