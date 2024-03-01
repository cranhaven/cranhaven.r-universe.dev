integ_clognormal <- function(u, event, base.dist, base.est, xbeta, cuts, sigma, gh) {
  # correlated lognormal frailties
  haz1 <- hazards(u, dist=base.dist[1], parms=base.est[[1]], cuts=cuts) * exp(xbeta[1])
  haz2 <- hazards(u, dist=base.dist[2], parms=base.est[[2]], cuts=cuts) * exp(xbeta[2])
  H1 <- cumhaz(base.dist[1], u, base.est[[1]], cuts=cuts)*exp(xbeta[1]) 
  H2 <- cumhaz(base.dist[2], u, base.est[[2]], cuts=cuts)*exp(xbeta[2]) 
  h <- (event==1)*haz1 + (event==2)*haz2
  gfun <- function(x, d1, d2, H1, H2) exp(x[1]*d1+x[2]*d2-exp(x[1])*H1-exp(x[2])*H2)
  d1 <- ifelse(event == 1, 1, 0) 
  d2 <- ifelse(event == 2, 1, 0) 
  integ_pen <- h*c(apply(gh$points, 1, gfun, d1=d1, d2=d2, H1=H1, H2=H2) %*% gh$weights)
  return(integ_pen)
}
