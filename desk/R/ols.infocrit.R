ols.infocrit = function(mod, which = "all", scaled = FALSE){
  n = mod$nobs
  if (which == "aic"){
    val = log(deviance(mod)/n) + 2 * mod$ncoef/n
  }
  if (which == "sic"){
    val = log(deviance(mod)/n) + log(n) * mod$ncoef/n
  }
  if (which == "pc"){
    val = deviance(mod)/n * (n + mod$ncoef)/(n - mod$ncoef)
  }
  if (which == "all"){
    val = c(log(deviance(mod)/n) + 2 * mod$ncoef/n,
            log(deviance(mod)/n) + log(n) * mod$ncoef/n,
            deviance(mod)/n * (n + mod$ncoef)/(n - mod$ncoef))
    names(val) = c("AIC","SIC","PC")
  }
  if (scaled == T){val = n*val}
  return(val)
}
