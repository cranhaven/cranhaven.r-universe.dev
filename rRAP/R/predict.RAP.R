predict.RAP <-
function(object, Xnew, ...){
  # function to predict based on current estimate of regression coefficients
  return( Xnew %*% t(object$beta))
}
