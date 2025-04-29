F.GEV <- function (x, xi, alfa, k) 
{
  if (k == 0) {
    y <- (x - xi)/alfa
  }
  else {
    y <- -k^(-1) * log(1 - k * (x - xi)/alfa)
  }
  F <- exp(-exp(-y))
  return(F)
}
