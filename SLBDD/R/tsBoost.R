#' Boosting with Simple Linear Regression
#'
#' It uses simple linear regression as the weak learner to perform L2 Boosting for time series data.
#'
#' @param y T by 1 scalar dependent variable.
#' @param X T by k data matrix of predictors: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param v Learning rate of boosting. Default value is 0.01.
#' @param m Maximum number of boosting iterations. Default is 1000.
#' @param rm.mean a logical command. Default is TRUE. If rm.mean=TRUE, both the dependent
#' and predictors are mean-adjusted. If rm.mean=FALSE, no mean adjustment is made.
#'
#' @return A list containing:
#' \itemize{
#'    \item beta - the estimates of coefficient vector.
#'    \item residuals - residuals after the boosting fit.
#'    \item m - the maximum number of boosting iterations (from input).
#'    \item v - learning rate (from input).
#'    \item selection - the indexes for selected predictors. That is, the indexes
#'    for large beta estimates.
#'    \item count: the number of selected predictors.
#'    \item yhat - the fitted value of y.
#' }
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- tsBoost(TaiwanAirBox032017[,1], TaiwanAirBox032017[,2])
"tsBoost" <- function(y, X, v=0.01, m=1000, rm.mean=TRUE){

  if(!is.matrix(X))X <- as.matrix(X)
  k <- ncol(X)
  n <- length(y)
  n <- min(n,nrow(X))
  y <- y[1:n]
  X <- X[1:n,]
  if(v > 1)v <- 1
  if(v <= 0)v <- 0.01

  y <- scale(y,center=TRUE,scale=FALSE)
  X <- scale(X,center=TRUE,scale=FALSE)

  yhat <- rep(0,n)
  beta <- rep(0,k)
  y1 <- y
  for (iter in 1:m){
    c1 <- cor(y1,X)
    idx <- which.max(abs(c1))
    m1 <- lm(y1~-1+X[,idx])
    yhat <- yhat+v*m1$fitted.values
    y1 <- y1-v*m1$fitted.values
    beta[idx] <- beta[idx]+v*m1$coefficients
  }

  resi <- y-yhat
  idx <- c(1:k)[abs(beta) > 0.0000001]
  icnt <- length(idx)

  message("ell-2 boosting via simple linear regression: ","\n")
  message("Both Y and X are mean-adjusted.","\n")
  message("v and m: ",c(v,m),"\n")
  message("number of predictors selected and number of predictors: ", paste(c(icnt,k), collapse = " "),"\n")

  return(list(beta=beta,residuals=resi,m=m,v=v,selection=idx,count=icnt,yhat=yhat))
}
