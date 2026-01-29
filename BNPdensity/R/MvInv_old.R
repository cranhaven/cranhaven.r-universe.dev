MvInv_old <-
  function(eps, u = 0.5, alpha = 1, beta = 1, gama = 1 / 2, N = 3001) {
    x <- -log(seq(from = exp(-1e-05), to = exp(-10), length = N))
    f <- alpha / gamma(1 - gama) * x^(-(1 + gama)) * exp(-(u +
      beta) * x)
    dx <- diff(x)
    h <- (f[-1] + f[-N]) / 2
    Mv <- c(rev(cumsum(rev(dx[-N] * h[-N]))), 0)
    err <- 1
    w <- 0
    v <- NULL
    while (err > eps) {
      w <- w + rgamma(1, 1, 1)
      v <- c(v, x[which.min(Mv > w)])
      err <- min(v) / sum(v)
    }
    return(v)
  }
