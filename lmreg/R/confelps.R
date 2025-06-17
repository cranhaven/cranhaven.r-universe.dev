confelps <-
function(y,X,A,alpha,tol=sqrt(.Machine$double.eps)) {
  CenterOfEllipse <- A%*%ginv(t(X)%*%X, tol=tol)%*%t(X)%*%y
  DAbscaled <- A%*%ginv(t(X)%*%X, tol=tol)%*%t(A)
  MatrixOfEllipse <- ginv(DAbscaled, tol=tol)
  hypdf <- qr(A)$rank
  errordf <- (length(y) - qr(X)$rank)
  ws <- sum((y - X%*%ginv(X)%*%y)^2) / errordf
  threshold <- ws * hypdf * qf(alpha,hypdf,errordf,lower.tail=F)
  return(list(CenterOfEllipse, MatrixOfEllipse, threshold))
}
