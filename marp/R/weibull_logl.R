#' A function to calculate the log-likelihood of Weibull model
#' @param param parameters of Weibull model
#' @param x input data for Weibull model
#' @return returns the value of negative log-likelihood of the Weibull model
#'
#' @examples
#' set.seed(42)
#' data <-  rgamma(30,3,0.01)
#'
#' # set some parameters
#' par_hat <- c(330.801103808081, 1.80101338777944) # estimated parameters
#' param <- log(par_hat) # input parameters for logl function
#'
#' # calculate log-likelihood
#' result <- marp::weibull_logl(param, data)
#'
#' # print result
#' cat("-logl = ", result, "\n")
#'
#'
#' @export

weibull_logl <- function(param, x) {
  lambda <- exp(param[1])
  k <- exp(param[2])
  logl <- sum(stats::dweibull(x, k, lambda, log = TRUE))
  return(-logl)
}
