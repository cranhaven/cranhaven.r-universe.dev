bicfun.aft <-
function(penalty,yy,delta,B,quantile,DD,nb,constmat)
{
  aa <- asyregpen.aft(yy,delta, B, quantile, abs(penalty), DD, nb, constmat)

  score = -2*likeli.ald(c(aa$sigma,aa$a),quantile,yy,delta,B,abs(penalty)*t(DD)%*%DD) + log(length(yy))*(1+sum(aa$diag.hat.ma))

  score
}
