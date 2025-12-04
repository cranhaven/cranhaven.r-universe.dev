h.boot.sample <-
  function(x, b, type)
  {
    if (type == "stationary") {type = 0}
    else if (type == "circular") {type = 1}
    else {stop("only stationary and circular bootstrap implemented")}
    return(f_bootstrap(x, b, type))
  }

.f.bootstrap <- function(x, nb = 1, statistic = NULL, b = NULL, type, ...)
{
  y <- stats::embed(x, 1)
  if(is.null(statistic)) {
    n = NROW(y)
    boot <- matrix(y, nrow=n, ncol=nb)
    out <- apply(boot, 2, h.boot.sample, b, type)
    return(drop(out))
  }
  else {
    
    yi <- 1:NROW(y)
    orig.statistic <- statistic(y,...)
    l.stat <- length(orig.statistic)
    stat <- matrix(0, nb, l.stat)
    for(i in 1:nb){
      stat[i,] <- statistic(as.matrix(y[h.boot.sample(yi, b, type),]),...)
    }
    out <- list(statistic = stat)
    
    return(out)
  }
}
f.bootstrap = compiler::cmpfun(.f.bootstrap)
