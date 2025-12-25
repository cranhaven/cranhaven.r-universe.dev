
#' Predictive posterior sample from copula GLM
#' 
#' Sample from the predictive posterior density of a copula generalized linear model regression
#'
#' @param object Result from calling \code{bayescopulaglm}
#' @param newdata \code{data.frame} of new data
#' @param nsims number of posterior draws to take. The default and minimum is 1. The maximum is the number of simulations in \code{object}
#' @param ... further arguments passed to or from other methods
#' @return \code{array} of dimension \code{c(n, J, nsims)} of predicted values, where \code{J} is the number of endpoints
#' 
#' @examples
#' set.seed(1234)
#' n <- 100
#' M <- 1000
#' 
#' x <- runif(n, 1, 2)
#' y1 <- 0.25 * x + rnorm(100)
#' y2 <- rpois(n, exp(0.25 * x))
#' 
#' formula.list <- list(y1 ~ 0 + x, y2 ~ 0 + x)
#' family.list <- list(gaussian(), poisson())
#' data = data.frame(y1, y2, x)
#' 
#' ## Perform copula regression sampling with default
#' ## (noninformative) priors
#' sample <- bayescopulaglm(
#'   formula.list, family.list, data, M = M
#' )
#' predict(sample, newdata = data)
#' 
#' @export
predict.bayescopulaglm <- function(
  object, newdata, nsims = 1, ...
) {
  if ( !('bayescopulaglm' %in% class(object) ) ) {
    stop ( 'object must be of class bayescopulaglm' )
  }
  if ( class(newdata) != 'data.frame' ) {
    stop( 'newdata must be a data.frame' )
  }
  if ( nsims < 0 | nsims > nrow(object$phisample) ) {
    stop('nsims must be an integer between 1 and number of simulations in object')
  }
  
  ## Obtain matrix of response variables and list of design matrices; and
  ## other statistics needed to pass onto c++ function
  ymat <- sapply(object$formula.list, function(f) newdata[, all.vars(f)[1]] ) 
  Xlist <- lapply(object$formula.list, function(f) model.matrix(f, newdata) )
  distnamevec <- sapply(object$family.list, function(f) f$family)
  linknamevec <- sapply(object$family.list, function(f) f$link)
  n <- nrow(ymat)
  J <- ncol(ymat)
  
  res <- copula_predict_all (
    Xlist, distnamevec, linknamevec, object$betasample, object$phisample,
    object$Gammasample, n, J, nsims
  )
  dimnames(res)[[2]] <- sapply(object$formula.list, function(f) all.vars(f)[1])
  return(res)
}

