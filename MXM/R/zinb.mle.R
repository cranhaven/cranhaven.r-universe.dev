zinb.mle <- function(y) {
  id <- which(y == 0) 
  y1 <- y[-id]
  y0 <- y[id]
  n0 <- length(id)
  n1 <- length(y1)
  sy1 <- sum(y1)
  lgy1 <- sum( lgamma(y1 + 1) )
  mle.zinb <- function(par, y1, y0, sy1, n1, n0) {
     p <- 1 / ( 1 + exp(- par[1] ) )
     k <- exp( par[2] )
     lam <- exp( par[3] )
     -( n0 * ( log( p + (1 - p) * (1 + k * lam)^(-1/k) ) ) +
     n1 * log(1 - p) + sum( lgamma(y1 + 1/k) ) - n1 * lgamma(1/k) +
     sy1 * log(k * lam) - sum( y1 + 1/k) * log1p(k * lam)  )
  }
  mod <- optim( rnorm(3), mle.zinb, y1 = y1, y0 = y0, sy1 = sy1, n1 = n1, n0 = n0)
  prop <- 1 / ( 1 + exp(- mod$par[1] ) )
  k <- exp( mod$par[2] )
  lam <- exp( mod$par[3] )
  list( be = lam, prop = prop, theta = 1/k, loglik = -mod$value - lgy1 )
}