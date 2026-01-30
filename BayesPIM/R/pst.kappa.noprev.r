pst.kappa.noprev = function( Vobs, j_, a = 1, b = 1){
  n_ = sum( sapply( Vobs, function(x) is.finite( x[length(x)] )) )
  m  = sapply(Vobs, length)
  alpha = n_ + a
  beta  = sum(m - j_ - 1) + b
  rbeta(1, shape1= alpha, shape2= beta)
}
