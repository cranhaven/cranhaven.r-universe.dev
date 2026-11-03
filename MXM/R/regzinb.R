regzinb <- function(par, y1, y0, x, poia, n1) {
   p <- 1 / ( 1 + exp(- par[1] ) )
   k <- exp( par[2] )
   be <- par[-c(1:2)]
   lam <- exp(x %*% be)
   eklam <- k * lam
   -( sum( log( p + (1 - p) * (1 + eklam[poia])^(-1/k) ) ) +
   n1 * log(1 - p) + sum( lgamma(y1 + 1/k) ) - n1 * lgamma(1/k) +
   sum( y1 * log(eklam[-poia]) ) - sum( (y1 + 1/k) * log1p(eklam[-poia]) ) )
}
  