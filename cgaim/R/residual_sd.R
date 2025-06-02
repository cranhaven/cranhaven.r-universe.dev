residual_sd <- function(object){
  r <- object$residuals
  w <- object$weights
  n <- length(r)
  sqrt(sum(w * r^2) / (n - object$edf))
}