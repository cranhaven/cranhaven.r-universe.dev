### Kappa distribution
### function originally defined in homtest package
F.kappa <-  function (x, xi, alfa, k, h) 
{
  if (k > 0) {
    if (x > (xi + alfa/k)) {
      stop("if k>0 x must be lower than xi + alfa/k")
    }
  }
  if (h > 0) {
    if (x < (xi + alfa * (1 - h^(-k))/k)) {
      stop("if h>0 x must be higher than xi + alfa*(1 - h^(-k))/k")
    }
  }
  else if (k < 0) {
    if (x < (xi + alfa/k)) {
      stop("if h<=0 and k<0 x must be higher than xi + alfa/k")
    }
  }
  if (k == 0) {
    k <- 10^(-100)
  }
  if (h == 0) {
    F <- F.GEV(x, xi, alfa, k)
  }
  else {
    F <- (1 - h * (1 - k * (x - xi)/alfa)^(1/k))^(1/h)
  }
  return(F)
}
