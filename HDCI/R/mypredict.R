mypredict <- function(object, newx) {
  
  p <- length(object$meanx)
  drop(scale(newx, object$meanx, FALSE) %*% matrix(object$beta, nrow = p)) + object$mu
  
}
