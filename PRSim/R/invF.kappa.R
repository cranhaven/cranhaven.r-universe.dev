### define inverse Kappa distribution
### originally defined in R-package homtest
invF.kappa <-  function (F, xi, alfa, k, h) 
{
  # if (F < 0) {
  #   stop("F must be between 0 and 1")
  # }
  # if (F > 1) {
  #   stop("F must be between 0 and 1")
  # }
  if (k == 0) {
    k <- 10^(-100)
  }
  if (h == 0) {
    x <- invF.GEV(F, xi, alfa, k)
  }
  else {
    x <- xi + (alfa/k) * (1 - ((1 - F^h)/h)^k)
  }
  return(x)
}
