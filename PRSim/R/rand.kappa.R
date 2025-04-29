### random generation from kappa distribution
### originally from homtest package
rand.kappa <- function (numerosita, xi, alfa, k, h) 
{
  F <- runif(numerosita, min = 1e-10, max = 0.9999999999)
  x <- invF.kappa(F, xi, alfa, k, h)
  return(x)
}
