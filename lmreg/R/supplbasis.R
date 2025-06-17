supplbasis <-
function(A,B,tol=sqrt(.Machine$double.eps)) {
  if (qr(A,tol=tol)$rank < qr(cbind(A,B),tol=tol)$rank) return(basis(B-projector(A,tol=tol)%*%B))
  else return(NULL)
}
