PartRidge <- function(x, y, lambda2 = 0, varset, standardize = TRUE, intercept = TRUE){ 
  
  x <- as.matrix(x)
  n <- nrow(x)
  m <- ncol(x)
  one <- rep(1, n)
  if (intercept) {
    meanx <- drop(one %*% x) / n
    x <- scale(x, meanx, FALSE)
    mu <- mean(y)
    y <- drop(y - mu)
  } else {
    meanx <- rep(0, m)
    mu <- 0
    y <- drop(y)
  }
  if (standardize) {
    normx <- sqrt(drop(one %*% (x^2)))
    x <- scale(x, FALSE, normx)
  } else {
    normx <- rep(1, m)
  } 
  
  penalty.factor <- rep(1, m)
  if (sum(varset) > 0) {
    penalty.factor[varset] <- 0
  }
  xy <- t(x) %*% y / n
  xx <- t(x) %*% x / n
  diag(xx) <- diag(xx) + lambda2 * penalty.factor
  beta <- solve(xx, xy, tol = 1e-64)
  beta <- drop(beta / normx)  
  
  object <- list()
  object$beta <- beta
  object$beta0 <- mu - drop(meanx %*% beta)
  object$meanx <- meanx
  object$mu <- mu
  object$normx <- normx
  object$lambda2 <- lambda2
  
  object    
}





