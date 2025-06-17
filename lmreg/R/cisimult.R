cisimult <-
function(y,X,A,alpha,tol= sqrt(.Machine$double.eps)) {
  Ab <- A%*%ginv(t(X)%*%X, tol= tol)%*%t(X)%*%y
  DAbscaled <- A%*%ginv(t(X)%*%X, tol=tol)%*%t(A)
  MatrixOfEllipse <- ginv(DAbscaled, tol=tol)
  hypdf <- qr(A)$rank
  errordf <- (length(y) - qr(X)$rank)
  ws <- sum((y - X%*%ginv(X)%*%y)^2) / errordf
  bonferronihw <- qt(alpha/(2*dim(A)[1]),errordf,lower.tail=F) *
    sqrt(ws * diag(DAbscaled))
  scheffehw <- sqrt(hypdf * qf(alpha,hypdf,errordf,lower.tail=F) *
                      ws * diag(DAbscaled))
  singlehw <- qt(alpha/2,errordf,lower.tail=F) *
    sqrt(ws * diag(DAbscaled))
  BFCB <- cbind(Ab - bonferronihw, Ab + bonferronihw)
  SFCB <- cbind(Ab - scheffehw, Ab + scheffehw)
  SNCB <- cbind(Ab - singlehw, Ab + singlehw)
  return(cbind(BFCB, SFCB, SNCB))
}
