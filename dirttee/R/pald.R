pald <- function (q, mu = 0, sigma = 1, p = 0.5, lower.tail = TRUE) 
{
  if(length(q) == 0) stop("q must be provided.")
  if(sigma <= 0) stop("sigma must be a positive number.")
  if(p >= 1 | p <= 0) stop("p must be a real number in (0,1).")
  if(any(abs(mu) == Inf)) stop("mu must be a finite real number.")
  if(lower.tail != TRUE && lower.tail != FALSE) stop("lower.tail must be TRUE or FALSE.")
  acum = ifelse(test = q < mu, yes = p * exp((1 - p) * (q - 
                                                          mu)/sigma), no = 1 - (1 - p) * exp(-(p) * (q - mu)/sigma))
  ifelse(test = lower.tail == TRUE, yes = return(acum), no = return(1 - 
                                                                      acum))
}