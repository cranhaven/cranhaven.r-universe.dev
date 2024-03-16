cor.arma = function(mod) {
  aa <- mod$var.coef
  bb <- diag(diag(aa) ^ (-.5))
  cc <- bb %*% aa %*% bb
  dimnames(cc) <- dimnames(aa)
  cc
}
