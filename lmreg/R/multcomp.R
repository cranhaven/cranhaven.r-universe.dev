multcomp <-
function(y,X,A,xi,tol=sqrt(.Machine$double.eps)) {
  R02 <- sum((y - X%*%ginv(X, tol=tol)%*%y)^2)
  errordf <- length(y) - qr(X, tol=tol)$rank
  ws <- R02 / errordf
  Ab <- A%*%ginv(t(X)%*%X, tol=tol)%*%t(X)%*%y
  DAbscaled <- A%*%ginv(t(X)%*%X, tol=tol)%*%t(A)
  hypdf <- qr(A)$rank
  Fstat <- as.vector(((Ab - xi)^2)/(ws*diag(DAbscaled)))
  Bonferroni.p <- length(xi)*pf(Fstat,1,errordf,lower.tail=F)
  Scheffe.p <- pf(Fstat/hypdf,hypdf,errordf,lower.tail=F)
  return(data.frame(cbind(A,xi,Fstat,Bonferroni.p,Scheffe.p)))
}
