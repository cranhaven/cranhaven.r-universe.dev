#' @export

PerfsRegression <- function(pred,actual) {
  sse <- sum((actual-pred)^2)
  mse <- mean((actual-pred)^2)
  rmse <- sqrt(mse)
  R2 <- cor(actual,pred,method = "pearson")^2
  tau <- cor(actual,pred,method = "kendall")
  res <- c(sse,mse,rmse,R2,tau)
  res <- round(res,3)
  names(res) <- c("sse","mse","rmse","R2","tau")
  return(res)
}
