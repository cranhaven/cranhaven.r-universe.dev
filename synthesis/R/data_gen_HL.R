#' Generate predictor and response data: Hysteresis Loop
#'
#' @param nobs  The data length to be generated.
#' @param a     The \emph{a} parameter. Default: 0.8.
#' @param b     The \emph{b} parameter. Default: 0.6.
#' @param c     The \emph{c} parameter. Default: 0.2.
#' @param m     Positive integer for the split line parameter.
#' If m=1, split line is linear; If m is even, split line has a u shape;
#' If m is odd and higher than 1, split line has a chair or classical shape.
#' @param n     Positive odd integer for the bulging parameter, indicates degree of outward curving (1=highest level of bulging).
#' @param fp    The frequency in the generated response. fp = 25 used in the WRR paper.
#' @param fd    A vector of frequencies for potential predictors. fd = c(3,5,10,15,25,30,55,70,95) used in the WRR paper.
#' @param sd.x  The noise level in the predictor.
#' @param sd.y  The noise level in the response.
#'
#' @details
#' The Hysteresis is a common nonlinear phenomenon in natural systems and it can be numerical simulated by the following formulas:
#' \deqn{x_{t} = a*cos(2pi*f*t)}
#' \deqn{y_{t} = b*cos(2pi*f*t)^m - c*sin(2pi*f*t)^n}
#' The default selection for the system parameters (\emph{a} = 0.8, \emph{b} = 0.6, \emph{c} = 0.2, \emph{m} = 3, \emph{n} = 5) is known to generate a classical hysteresis loop.
#'
#' @return A list of 3 elements: a vector of response (x), a matrix of potential predictors (dp) with each column containing one potential predictor, and a vector of true predictor numbers.
#' @export
#'
#' @references LAPSHIN, R. V. 1995. Analytical model for the approximation of hysteresis loop and its application to the scanning tunneling microscope. Review of Scientific Instruments, 66, 4718-4730.
#' @examples
#' ###synthetic example - Hysteresis loop
#' #frequency, sampled from a given range
#' fd <- c(3,5,10,15,25,30,55,70,95)
#'
#' data.HL <- data.gen.HL(m=3,n=5,nobs=512,fp=25,fd=fd)
#' plot.ts(cbind(data.HL$x,data.HL$dp))

data.gen.HL <- function(nobs = 512, a = 0.8, b = 0.6, c = 0.2, m = 3, n = 5, fp = 25, 
    fd, sd.x = 0.1, sd.y = 0.1) {
    t <- seq(0, 1, length.out = nobs)
    
    index <- which(fd %in% fp)
    ndim <- length(fd)
    
    dp <- matrix(0, nobs, ndim)
    for (i in 1:ndim) {
        
        dp[, i] <- a * cos(2 * pi * fd[i] * t) + rnorm(nobs, 0, sd.x)
        
    }
    
    x <- b * cos(2 * pi * fp * t)^m - c * sin(2 * pi * fp * t)^n + rnorm(nobs, 0, 
        sd.y)
    
    data_generated <- list(x = x, dp = dp, true.cpy = index)
    
    return(data_generated)
}
