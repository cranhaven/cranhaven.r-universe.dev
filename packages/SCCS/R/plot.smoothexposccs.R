plot.smoothexposccs <- function(x, type="l", conf.int=0.95, ...) {
  
  fit <- x
  z <- qnorm((1 + conf.int)/2, 0, 1)
  
  #lci <- fit$exposure - z*fit$se
  #uci <- fit$exposure + z*fit$se
  rho <- fit$exposure
  # Use exponential version of confidence bands 
  
  #lci <- fit$exposure*exp(-z*fit$se*(1/fit$exposure))
  #uci <- fit$exposure*exp(z*fit$se*(1/fit$exposure))
  
  lci <- rho*exp(-z*fit$se*(1/rho))
  uci <- rho*exp(z*fit$se*(1/rho))
  
  
  
  # Exposure related relative incidence function
  plot(fit$timesinceexpo, rho, type=type, ylab= "relative incidence", xlab= "days since start of risk period", ylim = c(0, (max(uci)+2)))
  # plot(fit$timesinceexpo, fit$exposure, type=type, ylab= "Relative incidence", xlab= "Days since start of risk period", ylim = c(min(lci), (max(uci)+2)))
  #plot(fit$timesbiceexpo, fit$exposure, type=type, ylab= "Relative incidence", xlab= "Days since start of risk period", ylim = c((min(fit$lci)-0.5), (max(fit$uci)+2)))
  lines(fit$timesinceexpo, lci, lty=2)
  lines(fit$timesinceexpo, uci, lty=2)
}




