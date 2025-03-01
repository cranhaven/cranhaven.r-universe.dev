#' A function to calculate the log-likelihood of Log-Logistics model
#' @param param parameters of Log-Logistics model
#' @param x input data for Log-Logistics model
#' @return returns the value of negative log-likelihood of the Log-Logistics model
#'
#' @examples
#' set.seed(42)
#' data <-  rgamma(30,3,0.01)
#'
#' # set some parameters
#' par_hat <- c(2.6037079185931518, 247.59811806509711) # estimated parameters
#' param <-  c(log(par_hat[2]),log(par_hat[1])) # input parameters for logl function
#'
#' # calculate log-likelihood
#' result <- marp::loglogis_logl(param, data)
#'
#' # print result
#' cat("-logl = ", result, "\n")
#'
#' @export


loglogis_logl <- function(param, x) {
  alpha <- exp(param[1])
  beta <- exp(param[2])
  logl <- sum(dllog(x, beta, alpha, log = TRUE))
  return(-logl)
}
