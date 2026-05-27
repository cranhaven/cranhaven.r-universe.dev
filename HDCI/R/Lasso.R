Lasso <- function(x, y, lambda = NULL, fix.lambda = TRUE, cv.method = "cv", nfolds = 10, foldid, cv.OLS = FALSE, tau = 0,
                parallel = FALSE, standardize = TRUE, intercept = TRUE, ...){
  
 
  x <- as.matrix(x)
  n <- dim(x)[1]
  p <- dim(x)[2]
  
  if (is.null(lambda) & fix.lambda) {
    stop("Should give a value of lambda for fix.lambda=TRUE")
  }
  if (length(lambda) > 1 & fix.lambda) {
    stop("The length of lambda should be 1 if fix.lambda=TRUE")
  }
  
  if (fix.lambda) {
    globalfit <- glmnet(x, y, standardize = standardize, intercept = intercept, ...)
    fitlasso <- predict(globalfit, type = "coefficients", s = lambda)
    beta0 <- fitlasso[1]
    beta <- fitlasso[-1]
  } else {
    globalfit <- glmnet(x, y, lambda = lambda, standardize = standardize, intercept = intercept, ...)
    cvobj <- escv.glmnet(x = x, y = y, lambda = lambda, nfolds = nfolds, foldid = foldid, tau = tau,
                       cv.OLS = cv.OLS, parallel = parallel, standardize = standardize, 
                       intercept = intercept, ...)
    if (cv.method == "cv") {
      lambda.opt <- cvobj$lambda.cv
    }
    if (cv.method == "cv1se") {
      lambda.opt <- cvobj$lambda.cv1se
    }
    if (cv.method == "escv") {
      lambda.opt <- cvobj$lambda.escv
    } 
    fitlasso <- predict(globalfit, type = "coefficients", s = lambda.opt)
    beta0 <- fitlasso[1]
    beta <- fitlasso[-1]  
    lambda <- lambda.opt
  }
  
  if (intercept) {
    meanx <- apply(x,2,mean)
    mu <- mean(y)
  } else {
    meanx <- rep(0,p)
    mu <- 0
  }

  object <- list()
  object$beta0 <- beta0
  object$beta <- beta
  object$lambda <- lambda
  object$meanx <- meanx
  object$mu <- mu
  object
}

