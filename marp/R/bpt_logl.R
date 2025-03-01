#' A function to calculate the log-likelihood of BPT model
#' @param param parameters of BPT model
#' @param x input data for BPT model
#' @return returns the value of negative log-likelihood of the BPT model
#'
#' @examples
#' set.seed(42)
#' data <-  rgamma(30,3,0.01)
#'
#' # set some parameters
#' par_hat <- c(292.945125794581, 0.718247184450307) # estimated parameters
#' param <-  c(log(par_hat[1]),log(par_hat[2]^2)) # input parameters for logl function
#'
#' # calculate log-likelihood
#' result <- marp::bpt_logl(param, data)
#'
#' # print result
#' cat("-logl = ", result, "\n")
#'
#' @export

bpt_logl <- function(param, x) {
  mu <- exp(param[1])
  alpha <- exp(param[2])
  n <- length(x)
  logl <-log(1 - 0) + (n / 2) * (log(mu) - log(2 * pi * alpha)) - sum((x - mu) ^ 2 / (2 * x * mu * alpha)) - 3 * sum(log(x)) / 2
  return(-logl)
}
