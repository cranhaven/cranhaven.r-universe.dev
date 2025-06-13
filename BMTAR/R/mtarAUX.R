prodB = function(x){
  prod = 1
  for (a in 1:length(x)) {
    prod = prod*x[a]
  }
  return(prod)
}
dmnormB = function(x, mean, sigma){
  dist = Brobdingnag::as.brob(t(x - mean) %*% solve(sigma) %*% (x - mean))
  cte = (2*pi)^{-nrow(sigma)/2}*determinant(sigma, logarithm = FALSE)$modulus^{-1/2}
  return(cte*exp(-1/2*dist))
}
dwishartB = function(x, nu, S){
  k = ncol(x)
  producto = Brobdingnag::as.brob(1)
  for (i in 1:k) {
    producto = producto*exp(Brobdingnag::as.brob(lgamma((nu + 1 - i)/2)))
  }
  densidades = (Brobdingnag::as.brob(2)^(nu*k/2)*Brobdingnag::as.brob(pi^(k*(k - 1)/4))*producto)^(-1) *
    Brobdingnag::as.brob(det((1/nu)*S))^(-nu/2)*Brobdingnag::as.brob(det(x))^((nu - k - 1)/2) *
    exp(Brobdingnag::as.brob(-0.5*sum(diag(solve((1/nu)*S) %*% x))))
  return(densidades)
}
