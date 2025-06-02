check.zero.order.homog <- function(S.fct, Z, tol = 1e-9) {
  # This program checks whether S(.) is zero-order Z-homogeneous by checking whether
  # S(Diag(Z%*%gamma)%*%x) = S(x). Here gamma and x are randomly generated.
  Z <- as.matrix(Z)
  nr <- nrow(Z)
  nc <- ncol(Z)
  x <- runif(nr, 1, 100)
  gam <- runif(nc, 1, 100)
  LHS.b <- x * c((Z%*%gam))   # = diag(Z%*%gam)%*%x
  diff.LRHS <- S.fct(LHS.b) - S.fct(x)
  norm.diff.LRHS <- sqrt(sum(diff.LRHS^2))
  if (norm.diff.LRHS > tol) {
    check.result <- paste("S(.) is not zero-order Z-homogeneous [based on tol =", tol, "]!")
  }
  else {
    check.result <- ""
  }
  check.result
}