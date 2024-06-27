#' Friedman with independent uniform variates
#'
#' @param nobs  The data length to be generated.
#' @param ndim  The number of potential predictors (default is 9).
#' @param noise The noise level in the time series.
#'
#' @return A list of 3 elements: a vector of response (x), a matrix of potential predictors (dp) with each column containing one potential predictor, and a vector of true predictor numbers.
#' @export
#'
#' @examples
#' ###synthetic example - Friedman
#' #Friedman with independent uniform variates
#' data.fm1 <- data.gen.fm1(nobs=1000, ndim = 9, noise = 0)
#'
#' #Friedman with correlated uniform variates
#' data.fm2 <- data.gen.fm2(nobs=1000, ndim = 9, r = 0.6, noise = 0)
#'
#' plot.ts(cbind(data.fm1$x,data.fm2$x), col=c('red','blue'), main=NA, xlab=NA,
#'         ylab=c('Friedman with \n independent uniform variates',
#'         'Friedman with \n correlated uniform variates'))

data.gen.fm1 <- function(nobs, ndim = 9, noise = 1) {
    # nobs<-1000;ndim=9;noise=1
    nwarm <- 500
    n <- nobs + nwarm
    x <- matrix(NA, n, 1)
    dp <- matrix(NA, n, ndim)
    
    for (i in 1:ndim) dp[, i] <- runif(n, min = 0, max = 1)
    # plot.ts(dp[,1:10])
    
    for (i in 1:n) {
        eps <- rnorm(1, mean = 0, sd = 1)
        x[i] <- 10 * sin(pi * dp[i, 1] * dp[i, 2]) + 20 * (dp[i, 3] - 0.5)^2 + 10 * 
            dp[i, 4] + 5 * dp[i, 5] + noise * eps
    }
    
    x <- x[(nwarm + 1):n]
    dp <- dp[(nwarm + 1):n, ]
    data_generated <- list(x = x, dp = dp, true.cpy = 1:5)
    # plot.ts(cbind(x,dp[,1:5]),type='l')
    
    return(data_generated)
}

#---------------------------------------------------------------------------
# Simulating from a multivariate normal distribution and then transforming the
# values by using the normal cdf. This will produce correlated standard uniform
# variates. You can then shift and scale to get your desired mean and SD. Note
# that this will give you a given rank correlation.

#' Friedman with correlated uniform variates
#'
#' @param nobs  The data length to be generated.
#' @param ndim  The number of potential predictors (default is 9).
#' @param r     Target Spearman correlation.
#' @param noise The noise level in the time series.
#'
#' @return A list of 3 elements: a vector of response (x), a matrix of potential predictors (dp) with each column containing one potential predictor, and a vector of true predictor numbers.
#' @export
#'
#' @examples
#' ###synthetic example - Friedman
#' #Friedman with independent uniform variates
#' data.fm1 <- data.gen.fm1(nobs=1000, ndim = 9, noise = 0)
#'
#' #Friedman with correlated uniform variates
#' data.fm2 <- data.gen.fm2(nobs=1000, ndim = 9, r = 0.6, noise = 0)
#'
#' plot.ts(cbind(data.fm1$x,data.fm2$x), col=c('red','blue'), main=NA, xlab=NA,
#'         ylab=c('Friedman with \n independent uniform variates',
#'         'Friedman with \n correlated uniform variates'))

data.gen.fm2 <- function(nobs, ndim = 9, r = 0.6, noise = 0) {
    # nobs<-1000;ndim=9;noise=0;r=0.6
    nwarm <- 500
    n <- nobs + nwarm
    x <- matrix(NA, n, 1)
    dp <- matrix(NA, n, ndim)
    
    # Pearson correlation
    rho <- 2 * sin(r * pi/6)
    
    # Correlation matrix
    Sigma <- matrix(rep(rho, ndim * ndim), ndim)
    diag(Sigma) <- 1
    
    # Generate sample
    dp <- pnorm(matrix(rnorm(n * ndim), ncol = ndim) %*% chol(Sigma))  # from stats and base package
    # plot.ts(dp[,1:10])
    
    for (i in 1:n) {
        eps <- rnorm(1, mean = 0, sd = 1)
        x[i] <- 10 * sin(pi * dp[i, 1] * dp[i, 2]) + 20 * (dp[i, 3] - 0.5)^2 + 10 * 
            dp[i, 4] + 5 * dp[i, 5] + noise * eps
    }
    
    x <- x[(nwarm + 1):n]
    dp <- dp[(nwarm + 1):n, ]
    data_generated <- list(x = x, dp = dp, true.cpy = 1:5)
    # plot.ts(cbind(x,dp[,1:5]),type='l')
    
    return(data_generated)
}

