################################################################################
#
# Function to compute vcov of betas
#
################################################################################

vcov_beta <- function(object){
  # Compute unscaled covariance matrix
  X <- cbind(1, object$gfit)
  unsc.vcov <- qr.solve(crossprod(X * sqrt(object$weights)))
  
  # Deal with intercept
  unsc.vcov[1,] <- unsc.vcov[1,] * attr(stats::terms(object), "intercept")
  unsc.vcov[,1] <- unsc.vcov[,1] * attr(stats::terms(object), "intercept")
  
  # Sigma estimator
  sigsq <- residual_sd(object)
  
  # Compute vcov matrix
  unsc.vcov * sigsq
}
