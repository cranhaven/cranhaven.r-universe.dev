compbasis <-
function(M,tol=sqrt(.Machine$double.eps)) {
  if (qr(M,tol=tol)$rank < dim(M)[1]) {
    return(as.matrix(svd(diag(1,dim(M)[1]) - projector(M,tol=tol))$
                       u[,1:(dim(M)[1] - qr(M,tol=tol)$rank)]))}
  else return(NULL)
}
