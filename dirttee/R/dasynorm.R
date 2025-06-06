#' @export
dasynorm <- function(x,mu=0,sigma=1,tau=0.5)
{
  if(any(sigma <= 0)) stop("standard deviation must be strictly > 0.")
  if(any(tau <= 0 | tau >= 1)) stop("tau must be between 0 and 1.")
  (2*sqrt(tau*(1-tau)/pi)/sigma)/(sqrt(1-tau) + sqrt(tau))*exp(-abs(tau - (x <= mu))*(x - mu)^2/sigma^2)
}
