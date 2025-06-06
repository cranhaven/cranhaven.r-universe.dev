dald <- function (y, mu = 0, sigma = 1, p = 0.5) 
{
  if (length(y) == 0) stop("y must be provided.")
  if(sum(y[is.na(y) == TRUE]) > 0) stop("There are some NA's values in y.")
  if(sigma <= 0) stop("sigma must be a positive number.")
  if(p >= 1 | p <= 0) stop("p must be a real number in (0,1).")
  if(any(abs(mu) == Inf)) stop("mu must be a finite real number.")
  densi = ifelse(test = y < mu, yes = (p * (1 - p)/sigma) * 
                   exp((1 - p) * (y - mu)/sigma), no = (p * (1 - p)/sigma) * 
                   exp(-(p) * (y - mu)/sigma))
  return(densi)
}