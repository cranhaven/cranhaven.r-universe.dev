######################   begin num.deriv.fct ######################################
num.deriv.fct <- function(f.fct,m) {
  #
  #   Author: Joseph B. Lang, Univ of Iowa
  #   Created:  c. 8/25/00 (last update: 3/30/04)
  #
  #   The numerical derivative of the transpose of function f.fct is
  #   computed at the value m.  If f.fct is a mapping from
  #   Rp to Rq then the result is a pxq matrix.
  #     I.e. Result is approximation to
  #         d f.fct(m)^T/d m.
  #
  #   This program is used in mph.fit.
  #
  eps <- (.Machine$double.eps)^(1/3)
  d <- eps * m + eps
  lenm <- length(m)
  E <- diag(c(d))
  f1 <- f.fct(m+E[,1])
  lenf <- length(f1)
  Ft <- matrix(NA, nrow = lenf, ncol = lenm)
  Ft[, 1] <- (f1-f.fct(m-E[,1]))/(2*d[1])
  for (j in 2:lenm) {
    Ft[, j] <- (f.fct(m+E[,j])-f.fct(m-E[,j]))/(2*d[j])
  }
  dimnames(Ft) <- NULL
  t(Ft)
}
######################   end num.deriv.fct ######################################
