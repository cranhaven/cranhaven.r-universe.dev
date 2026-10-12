isqrtfa <-
function(Psi,B){
  m = length(Psi)
  nbf = ncol(B)
  sing = B/(sqrt(Psi)%*%t(rep(1,nbf)))
  sing = fast.svd(sing)
  tmp = rep(1,nbf) - 1/sqrt(rep(1,nbf) + sing$d^2)
  tmp = sing$u*(rep(1,m)%*%t(tmp))
  tmp = diag(m)-tmp%*%t(sing$u)
  tmp = tmp/(sqrt(Psi)%*%t(rep(1,m)))
  return(tmp)
}
