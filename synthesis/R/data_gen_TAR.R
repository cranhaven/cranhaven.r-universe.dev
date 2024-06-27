#' Generate a two-regime threshold autoregressive (TAR) process.
#'
#' @details
#' The two-regime Threshold Autoregressive  (TAR) model is given by the following formula:
#' \deqn{Y_t = \phi_{1,0}+\phi_{1,1} Y_{t-1} +\ldots+ \phi_{1,p} Y_{t-p}+\sigma_1 e_t, \mbox{  if } Y_{t-d}\le r}
#' \deqn{Y_t = \phi_{2,0}+\phi_{2,1} Y_{t-1} +\ldots+ \phi_{2,p} Y_{t-p}+\sigma_2 e_t, \mbox{  if } Y_{t-d} > r.}
#' where r is the threshold and d the delay.
#'
#' @param nobs    the data length to be generated
#' @param ndim    The number of potential predictors (default is 9)
#' @param phi1    the coefficient vector of the lower-regime model
#' @param phi2    the coefficient vector of the upper-regime model
#' @param theta   threshold
#' @param d       delay
#' @param p       maximum autoregressive order
#' @param noise   the white noise in the data
#'
#' @return A list of 2 elements: a vector of response (x), and a matrix of potential predictors (dp) with each column containing one potential predictor.
#' @export
#'
#' @references Cryer, J. D. and K.-S. Chan (2008). Time Series Analysis With Applications in R Second Edition Springer Science+ Business Media, LLC.
#'
#' @examples
#' # TAR2 model from paper with total 9 dimensions
#' data.tar<-data.gen.tar(500)
#' plot.ts(cbind(data.tar$x,data.tar$dp))
data.gen.tar <- function(nobs, ndim = 9, phi1 = c(0.6, -0.1), phi2 = c(-1.1, 0), 
    theta = 0, d = 2, p = 2, noise = 0.1) {
    if (length(phi1) != p) 
        stop("wrong dimension of p!")
    
    nwarm1 <- nwarm2 <- 250
    n <- nobs + nwarm1 + nwarm2
    x <- matrix(0, n, 1)
    for (i in 1:nwarm1) {
        x[i] <- rnorm(1, mean = 0, sd = 1)
    }
    dp <- matrix(0, (nobs), ndim)
    for (i in (nwarm1 + 1):n) {
        eps <- rnorm(1, mean = 0, sd = 1)
        xid <- x[i - d]
        if (xid < theta) 
            x[i] <- sum(phi1 * x[(i - 1):(i - p)]) + noise * eps else x[i] <- sum(phi2 * x[(i - 1):(i - p)]) + noise * eps
    }
    for (i in 1:ndim) dp[, i] <- x[(n - i - nobs + 1):(n - i)]
    x <- x[(n - nobs + 1):n]
    data_generated <- list(x = x, dp = dp, true.cpy = unique(c(which(phi1 != 0), 
        which(phi2 != 0))))
    return(data_generated)
}
