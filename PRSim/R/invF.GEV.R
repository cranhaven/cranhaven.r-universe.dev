### define inverse GEV distribution
### originally defined in R-package homtest
invF.GEV <- function (F, xi, alfa, k) 
{
  if (k == 0) {
    x <- xi - alfa * log(-log(F))
  }
  else {
    x <- xi + alfa * (1 - (-log(F))^k)/k
  }
  return(x)
}
