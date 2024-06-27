#----------------------------------------------------------------------------------------------------
#' Nonlinear system with independent/correlate covariates
#'
#' @param nobs  The data length to be generated.
#' @param ndim  The number of potential predictors (default is 9).
#' @param r     Target Spearman correlation among covariates.
#' @param noise The noise level in the time series.
#'
#' @return A list of 3 elements: a vector of response (x), a matrix of potential predictors (dp) with each column containing one potential predictor, and a vector of true predictor numbers.
#' @export
#'
#' @examples
#' ###synthetic example - Friedman
#' #Friedman with independent uniform variates
#' data.nl1 <- data.gen.nl1(nobs=1000)
#'
#' #Friedman with correlated uniform variates
#' data.nl2 <- data.gen.nl2(nobs=1000)
#'
#' plot.ts(cbind(data.nl1$x,data.nl2$x), col=c('red','blue'), main=NA, xlab=NA,
#'         ylab=c('Nonlinear system with \n independent uniform variates',
#'         'Nonlinear system with \n correlated uniform variates'))
data.gen.nl1 <- function(nobs, ndim = 15, r = 0.6, noise = 1) {
    # nobs<-1000;ndim=9;noise=1;r=0.6
    nwarm <- 500
    n <- nobs + nwarm
    x <- matrix(NA, n, 1)
    dp <- matrix(NA, n, ndim)
    
    Sigma <- matrix(rep(r, ndim * ndim), ndim)
    diag(Sigma) <- 1
    dp <- MASS::mvrnorm(n, mu = rep(0, ndim), Sigma = Sigma, empirical = TRUE)
    # plot.ts(dp[,1:10])
    
    for (i in 1:n) {
        eps <- rnorm(1, mean = 0, sd = 1)
        x[i] <- dp[i, 2]^2 + cos(dp[i, 6]) + 0.35 * sin(dp[i, 9]) + noise * eps
    }
    
    x <- x[(nwarm + 1):n]
    dp <- dp[(nwarm + 1):n, ]
    data_generated <- list(x = x, dp = dp, true.cpy = c(2, 6, 9))
    # plot.ts(cbind(x,dp[,c(2,6,9)]),type='l')
    
    return(data_generated)
}
#----------------------------------------------------------------------------------------------------
#' Nonlinear system with Exogenous covariates
#'
#' @param nobs  The data length to be generated.
#' @param ndim  The number of potential predictors (default is 9).
#' @param noise The noise level in the time series.
#'
#' @return A list of 3 elements: a vector of response (x), a matrix of potential predictors (dp) with each column containing one potential predictor, and a vector of true predictor numbers.
#' @export
#'
#' @references Sharma, A., & Mehrotra, R. (2014). An information theoretic alternative to model a natural system using observational information alone. Water Resources Research, 50(1), 650-660.
#'
#' @examples
#' ###synthetic example - Friedman
#' #Friedman with independent uniform variates
#' data.nl1 <- data.gen.nl1(nobs=1000)
#'
#' #Friedman with correlated uniform variates
#' data.nl2 <- data.gen.nl2(nobs=1000)
#'
#' plot.ts(cbind(data.nl1$x,data.nl2$x), col=c('red','blue'), main=NA, xlab=NA,
#'         ylab=c('Nonlinear system with \n independent uniform variates',
#'         'Nonlinear system with \n correlated uniform variates'))
data.gen.nl2 <- function(nobs, ndim = 7, noise = 1) {
    # nobs<-1000;ndim=9;noise=0
    nwarm <- 500
    n <- nobs + nwarm
    x <- matrix(NA, n, 1)
    dp <- matrix(NA, n, ndim)
    
    for (i in 1:ndim) dp[, i] <- rnorm(n, mean = 0, sd = 1)
    # plot.ts(dp[,1:10])
    
    for (i in 1:n) {
        eps <- rnorm(1, mean = 0, sd = 1)
        x[i] <- 0.8 * dp[i, 1] + 0.4 * dp[i, 2]^2 + 0.7 * dp[i, 3]^3 + noise * eps
    }
    
    x <- x[(nwarm + 1):n]
    dp <- dp[(nwarm + 1):n, ]
    data_generated <- list(x = x, dp = dp, true.cpy = c(1, 2, 3))
    # plot.ts(cbind(x,dp[,c(2,6,9)]),type='l')
    
    return(data_generated)
}


