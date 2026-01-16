#' Select the Penalty Parameter of LASSO-type Linear Regression
#'
#' Use out-of-sample Root Mean Square Error to select the penalty parameter of
#' LASSO-type linear regression.
#'
#' @param X Matrix of predictors of the estimation sample.
#' @param y Dependent variables of the estimation sample.
#' @param newX Design matrix in the forecasting subsample.
#' @param newY Dependent variable in the forecasting subsample.
#' @param family Response type. See the glmnet command in R. Possible types are "gaussian", "binomial",
#' "poisson", "multinomial", "cox", "mgaussian". Default is "gaussian".
#' @param alpha The elasticnet mixing parameter, with \eqn{0 \leq \alpha \leq 1}. See the glmnet command in R. Default value is 1.
#'
#' @return A list containing:
#' \itemize{
#'    \item lambda.min - lambda that achieves the minimum mean square error.
#'    \item beta - estimated coefficients for lambda.min.
#'    \item mse - mean squared error.
#'    \item lambda - the actual sequence of lambda values used.
#' }
#'
#' @importFrom glmnet glmnet
#'
#' @export
#'
#' @examples
#' X <- cbind(rnorm(200),rnorm(200,2,1),rnorm(200,4,1))
#' y <- rnorm(200)
#' newX <- cbind(rnorm(200),rnorm(200,2,1),rnorm(200,4,1))
#' newy <- rnorm(200)
#' output <- Lambda.sel(X, y, newX, newy)
"Lambda.sel" <- function(X, y, newX, newY, family="gaussian", alpha=1){

  X <- as.matrix(X)
  y <- as.vector(y)
  m1 <- glmnet(X,y,family=family,alpha=alpha)
  pm1 <- predict(m1,newx=newX)
  k1 <- length(m1$lambda)
  npt <- length(newY)
  Er <- matrix(newY,npt,1)%*%matrix(rep(1,k1),1,k1)-pm1
  mse <- apply(Er^2,2,mean)
  idx <- which.min(mse)
  beta <- coef(m1,s=m1$lambda[idx])
  message("selected penalty: ",m1$lambda[idx],"\n")
  message("coef-estimates: ",round(beta[,1],3),"\n")
  plot(log(m1$lambda),mse,xlab="log(lambda)",ylab="mean-squared error")

  return(list(lambda.min=m1$lambda[idx], beta=beta[,1], mse=mse,lambda=m1$lambda))
}
