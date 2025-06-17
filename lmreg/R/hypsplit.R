hypsplit <-
function(X,A,xi,tol=sqrt(.Machine$double.eps)){
  T1 <- t(X)%*%X%*%ginv(t(X)%*%X+t(A)%*%A, tol=tol)%*%t(A)
  T2 <- (diag(1,dim(X)[2]) - projector(t(X), tol=tol))%*%t(A)
  if (qr(T1,tol=tol)$rank>0) {
    v1 <- svd(T1)$v[,1:qr(T1,tol=tol)$rank]
    testable <- t(v1)%*%cbind(A,xi)
  }
  else testable <- NULL
  if (qr(T2,tol=tol)$rank>0) {
    v2 <- svd(T2)$v[,1:qr(T2,tol=tol)$rank]
    untestable <- t(v2)%*%cbind(A,xi)
  }
  else untestable <- NULL
  return(list(testable,untestable))
}
