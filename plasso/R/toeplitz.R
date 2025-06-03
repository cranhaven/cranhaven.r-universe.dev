#' Simulated 'Toeplitz' Data
#' 
#' @description
#' Simulated data from a DGP with an underlying causal relationship between
#' covariates X and the target y.
#' The covariates matrix X consists of 10 variables whose effect size on target
#' y is defined by the vector
#' \code{c(1, -0.83, 0.67, -0.5, 0.33, -0.17, 0, ..., 0)}
#' with the first six effect sizes decreasing in absolute terms continuously
#' from 1 to 0 and alternating in their sign.
#' The true causal effect of all other covariates is 0.
#' The variables in X follow a normal distribution with mean zero while the
#' covariance matrix follows a Toeplitz matrix.
#' The target y is then a linear transformation of X plus a vector of standard
#' normal random variables (i.e. error term).
#' (See vignette for more details.)
#'
#' @docType data
#' 
#' @usage data(toeplitz)
#'
#' @keywords datasets
#' 
#' @export
#' 
#' @examples
#' # load toeplitz data
#' data(toeplitz)
#' # extract target and features from data
#' y = as.matrix(toeplitz[,1])
#' X = toeplitz[,-1]
#' # fit cv.plasso to the data
#' \donttest{p.cv = plasso::cv.plasso(X,y)}
#'
"toeplitz"