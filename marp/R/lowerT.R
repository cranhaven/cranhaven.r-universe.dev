#' An utility function to calculate upper limit of T statistic
#' @param low lower limit
#' @param hat estimates
#' @param sigmasq variance
#' @param Tstar T statistics estimated from bootstrap samples
#' @param weights model weights
#' @param B number of bootstraps
#' @param alpha confidence level
#' @return returns upper limit of T-statistic
#' @examples
#' # set some parameters
#' low <- 100 # lower bound
#' hat <- rep(150, 6) # estimates obtained from each model
#' sigmasq <- 10 # variance
#' Tstar <- matrix(rep(100,600),6,100) # T statistics estimated from bootstrap samples
#' weights <- rep(1/6, 6) # model weights
#' B <- 100 # number of bootstrapped samples
#' alpha <- 0.05 # confidence level
#'
#' # calculate the upper limit of T statistics
#' res <- marp::lowerT(low, hat, sigmasq, Tstar, weights, B, alpha)
#'
#' # print result
#' cat("res = ", res, "\n")
#'
#' @export

lowerT <-  function(low, hat, sigmasq, Tstar, weights, B, alpha) {
  upperT <- (hat - low) / sqrt(sigmasq)
  temp <- sapply(1:6, function(i) weights[i] * sum(Tstar[i, ] >= upperT[i]) / B)
  return(sum(temp) - alpha / 2)
}
