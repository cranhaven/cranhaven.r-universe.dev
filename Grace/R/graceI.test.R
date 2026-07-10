# This function calculates GraceI coefficients and p-values.
# Author:             Sen Zhao
# Email:              sen-zhao@sen-zhao.com
# ----------------------------------------------------------------------------------------------
# Arguments:
# Y:                  n by 1 vector of the response variable.
# X:                  n (number of rows) by p (number of columns) design matrix.
# lambda.2:           tuning parameters of the ridge penalty.
#                     needs to be normalized beforehand.
# K:                  number of folds in cross-validation.
# sigma.error:        error standard deviation. If NULL, scaled lasso is applied.
# enable.group.test:  binary parameter indicating whether group tests should be enabled.
# eta, C:             parameters of the grace test; see Zhao & Shojaie (2016) for reference.
# verbose:            whether computation progress should be printed.
# ----------------------------------------------------------------------------------------------
# Outputs:
# intercept:          intercept of the linear regression model.
# beta:               regression coefficients (slopes) of the linear regression model.
# pvalue:             p-values of individual hypothesis tests.
# group.test:         function to perform group-wise hypothesis test.
# ----------------------------------------------------------------------------------------------


graceI.test <- function(Y, X, lambda.2, eta = 0.05, C = 4 * sqrt(3), K = 10, sigma.error = NULL, verbose = FALSE){
  graceI.result <- grace.test(Y = Y, X = X, lambda.2 = lambda.2, eta = eta, C = C, K = K, sigma.error = sigma.error, verbose = verbose)
  return(graceI.result)
}