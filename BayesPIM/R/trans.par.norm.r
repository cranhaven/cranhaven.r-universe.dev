trans.par.norm = function(p1, p2, v1, v2, cr, Z1, Z2, a){
  mu1 = Z1 %*% as.matrix(p1)
  mu2 = Z2 %*% as.matrix(p2)
  sd1 = exp(v1)
  sd2 = exp(v2)
  a  = logrob(a, tol=1e-300)
  m  = mu1 + sd1/sd2*cr*(a-mu2)
  v  = sqrt( (1-cr^2)*sd1^2 )
  cbind(m,v)
}