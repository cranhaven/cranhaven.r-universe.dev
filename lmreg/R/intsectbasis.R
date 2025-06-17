intsectbasis <- 
function(A, B, tol1=sqrt(.Machine$double.eps), tol2=sqrt(.Machine$double.eps)) {
  parsum <- A%*%t(A)%*%ginv(A%*%t(A)+B%*%t(B),tol = tol1)%*%B%*%t(B)
  rho <- sum(which(svd(parsum)$d > tol2))
  if (rho>0) return(svd(parsum)$u[,1:rho])
  else return(NULL)
}
