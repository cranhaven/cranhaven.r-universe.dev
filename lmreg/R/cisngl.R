cisngl <-
function(y,X,p,alpha,type,tol= sqrt(.Machine$double.eps)) {
  R02 <- sum((y - X%*%ginv(X)%*%y)^2)
  errordf <- length(y) - qr(X)$rank
  estimate <- t(p)%*%ginv(t(X)%*%X, tol = tol)%*%t(X)%*%y
  sd <- sqrt((R02 / errordf) * t(p)%*%ginv(t(X)%*%X)%*%p)
  ci <- c(-Inf,Inf)
  if (type == "lower") ci[1] <- estimate - sd * qt(1-alpha, df = errordf)
  if (type == "upper") ci[2] <- estimate + sd * qt(1-alpha, df = errordf)
  if (type == "both") {
    ci[1] <- estimate - sd * qt(1-alpha/2, df = errordf)
    ci[2] <- estimate + sd * qt(1-alpha/2, df = errordf)
  }
  return(list(estimate,ci)) # point estimate and confidence interval
}
