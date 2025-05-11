confband_Sigma_hat <- function(x, fnw, func, w, a, b, ...){
  n <- length(x)
  fkw <- sapply(1:a, function(k) func(x[seq(from = (k - 1)*b + 1, to = k*b)], ...)(w))
  Sigma_hat <- tcrossprod(fkw - fnw)*b/(a - 1)
  return(Sigma_hat)
}
