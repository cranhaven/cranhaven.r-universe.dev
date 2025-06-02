expit <- function(x){
  z = exp(x)
  ind = which(is.infinite(z))
  y = z/(1+z)
  y[ind] = 1
  y
}