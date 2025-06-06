#' @export
#' @importFrom stats runif

rasynorm <-
function(n,mu=0,sigma=1,tau=0.5)
{
  if(any(sigma <= 0)) stop("standard deviation must be strictly > 0.")
  if(any(tau <= 0 | tau >= 1)) stop("tau must be between 0 and 1.")
  x = runif(n)
  qasynorm(x,mu,sigma,tau)
}
