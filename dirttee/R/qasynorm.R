#' @export
#' @importFrom stats optimize

qasynorm <-
function(p,mu=0,sigma=1,tau=0.5)
{
  if(any(sigma <= 0)) stop("standard deviation must be strictly > 0.")
  if(any(tau <= 0 | tau >= 1)) stop("tau must be between 0 and 1.")
  mapply(function(z)optimize(function(y)abs(pasynorm(y,mu,sigma,tau)-z),interval=c(mu-5*sigma,mu+5*sigma))$minimum,p)
}
