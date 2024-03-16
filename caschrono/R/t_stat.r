t_stat = function(modarima, decim = 6) {
  if(!is(modarima, "Arima")) 
    stop("modarima may be an Arima object from package forecast")
  t.stat <- modarima$coef[modarima$mask == TRUE] / sqrt(diag(modarima$var.coef))
  p.val <- rep(0, length(t.stat))
  p.val[t.stat < 0] <- 2 * pnorm(t.stat[t.stat < 0])
  p.val[t.stat > 0] <- 2 * (1 - pnorm(t.stat[t.stat > 0]))
  round(rbind(t.stat, p.val), digits = decim)
}
