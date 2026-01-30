pst.kappa = function( Vobs, j_, a = 1, b = 1, g, r, g.fixed){
  delta = as.numeric(sapply( Vobs, function(x) is.finite( x[length(x)] )) ) + g.fixed
  rho = sum( delta )
  m  = sapply(Vobs, length) - g.fixed
  L0 = sum( (m - j_ - 1)[g==0] )
  C1 = sum( m[g==1] + r[g==1] - 2)
  alpha = rho + a
  beta  = L0 + C1 + b
  rbeta(1, shape1= alpha, shape2= beta)
}
