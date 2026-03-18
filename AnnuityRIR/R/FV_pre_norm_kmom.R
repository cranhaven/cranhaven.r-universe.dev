FV_pre_norm_kmom = function(data, years=10) {
  app = rep(NA, years)
  for (i in 1:years)
    app[i] = norm_mom(data, i)
  FV = sum(app)
  return(FV)
}
