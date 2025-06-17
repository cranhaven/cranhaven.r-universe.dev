basis <-
function(M,tol=sqrt(.Machine$double.eps)) {
  if (qr(M,tol=tol)$rank > 0) return(as.matrix(svd(M)$u[,1:qr(M)$rank]))
  else return(NULL)
}


