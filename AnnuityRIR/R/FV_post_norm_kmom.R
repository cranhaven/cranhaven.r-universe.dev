norm_mom = function(data, order) {
  r = order
  U = 1 + data
  sigma2 = function(x) {
    var(x) * (length(x) - 1) / length(x)
  }
  u = mean(U)
  s = sqrt(sigma2(U))
  f = function(x) {
    (x ^ (r)) * (1 / (s * sqrt(2 * pi))) * exp(-((x - u) ^ 2) / (2 * (s) ^ 2))
  }
  mom = integrate(f, min(U), max(U))
  return(mom$value)
}

FV_post_norm_kmom = function(data, years=10) {
  app = rep(NA, years)
  for (i in 1:years)
    app[i] = norm_mom(data, i)
  FV = sum(app[1:years - 1]) + 1
  return(FV)
}




