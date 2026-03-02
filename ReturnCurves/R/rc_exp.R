rc_exp <- function(margdata, w, p, method, q_minproj, qalphas, k, constrained, tol, par_init){
  n <- length(w)
  xp <- qexp(1 - p)
  lambda <- adf_est(margdata = margdata, w = w, method = method, q = q_minproj, qalphas = qalphas, k = k, constrained = constrained, tol = tol, par_init = par_init)
  thresh <- sapply(w, function(i) minproj_lambda(lambda@dataexp, i, q_minproj = q_minproj)$thresh)
  r <- sapply(1:n, function(i) thresh[i] - log(p/(1 - q_minproj))/lambda@adf[i])
  x <- sapply(1:n, function(i) r[i] * w[i])
  y <- sapply(1:n, function(i) r[i] * (1 - w[i]))
  for(i in 1:length(x)){
    if(x[i] > xp){
      x[i] <- xp
    }
    if(x[i] < 0){
      x[i] <- 0
    }
    if(y[i] > xp){
      y[i] <- xp
    }
    if(y[i] < 0){
      y[i] <- 0
    }
  }
  x[1] <- 0
  y[1] <- xp
  x[n] <- xp
  y[n] <- 0
  for(i in length(w[w < 0.5]):1){
    if(x[i] > x[i + 1]){
      x[i] <- x[i + 1]
    }
    if(y[i] < y[i + 1]){
      y[i] <- y[i + 1]
    }
  }
  for(i in length(w[w > 0.5]):(length(w))){
    if(x[i] < x[i - 1]){
      x[i] <- x[i - 1]
    }
    if(y[i] > y[i - 1]){
      y[i] <- y[i - 1]
    }
  }
  rc <- cbind(x, y)
  interval <- lambda@interval
  return(list("rc" = rc, "alphas" = interval))
}