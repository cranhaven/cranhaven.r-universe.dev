################################################################################
#
# Function to extract unconstrained vcov of alpha
#
################################################################################

vcov_alpha <- function(object){
  # Compute Jacobian
  dgz <- object$dg[,object$index]
  Vmat <- object$x * dgz
  
  # Unscaled covariance matrix
  unsc.vcov <- qr.solve(crossprod(Vmat * sqrt(object$weights)))
  
  # Sigma estimator
  sigsq <- residual_sd(object)
  
  # Compute vcov matrix
  unsc.vcov * sigsq
}