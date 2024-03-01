integ_cgamma <- function(u, event, base.dist, base.est, xbeta, cuts, kappa) {
  # correlated gamma frailties
  k0 <- kappa[3]
  k1 <- kappa[1]
  k2 <- kappa[2]
  w1 <- k0 + k1
  w2 <- k0 + k2
  haz1 <- hazards(u, dist=base.dist[1], parms=base.est[[1]], cuts=cuts) * exp(xbeta[1])
  haz2 <- hazards(u, dist=base.dist[2], parms=base.est[[2]], cuts=cuts) * exp(xbeta[2])
  H1 <- cumhaz(base.dist[1], u, base.est[[1]], cuts=cuts)*exp(xbeta[1]) 
  H2 <- cumhaz(base.dist[2], u, base.est[[2]], cuts=cuts)*exp(xbeta[2]) 
  h <- (event==1)*haz1 + (event==2)*haz2
  integ_pen <- h * (1 + H1/w1)^(-k1) * (1 + H2/w2)^(-k2) * (1 + H1/w1 + H2/w2)^(-k0) *
    (k0/w1 * (1 + H1/w1 + H2/w2)^(-1) + k1/w1 * (1 + H1/w1)^(-1))
  return(integ_pen)
}
