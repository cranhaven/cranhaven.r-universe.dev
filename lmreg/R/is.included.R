is.included <-
function(B, A, tol1=sqrt(.Machine$double.eps), tol2=sqrt(.Machine$double.eps)) {
  return(frob(B-A%*%ginv(A, tol=tol1)%*%B) / frob(B) < tol2)
}
