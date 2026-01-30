augment.C.collapsed = function(w_sums, Vobs, kappa, theta1, r, g.fixed){
  
  n    = length(Vobs)
  cens = sapply(Vobs, function(x) is.infinite(x[length(x)]))
  m    = sapply(Vobs, length)
  
  theta0 = 1 - theta1
  
  p0 <- numeric(n)
  p0[] <- NA
  p0[!g.fixed]  = theta0[!g.fixed] * w_sums[!g.fixed]
  
  p1 <- numeric(n)
  p1[] <- NA
  p1[!g.fixed]  = theta1[!g.fixed] * kappa^(1-cens[!g.fixed]) * (1-kappa)^{(m-2+r)[!g.fixed]}
  
  psum = apply( cbind(p0, p1), 1, sum )
  p1 = p1 / psum
  
  g <- numeric(n)
  g[] <- NA
  g[!g.fixed] <- rbinom(sum(!g.fixed),1, p1[!g.fixed])
  g[g.fixed] <- 1
  g
}