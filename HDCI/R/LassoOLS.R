LassoOLS <- function(x, y, OLS = TRUE, lambda = NULL, fix.lambda = TRUE, cv.method = "cv", nfolds = 10, foldid, 
                   cv.OLS = TRUE, tau = 0, parallel = FALSE, standardize = TRUE, intercept = TRUE, ...){
  

  x <- as.matrix(x)
  n <- dim(x)[1]
  p <- dim(x)[2]
  
  if (is.null(lambda) & fix.lambda) {
    stop("Should given a value of lambda for fix.lambda=TRUE")
  }
  if (length(lambda) > 1 & fix.lambda) {
    stop("The length of lambda should be 1 if fix.lambda=TRUE")
  }
  
  if (fix.lambda) {
    globalfit <- glmnet(x, y, standardize = standardize, intercept = intercept, ...)
    fitlasso <- predict(globalfit, type = "coefficients", s = lambda)
    betalasso <- fitlasso[-1]
    selectvar <- betalasso != 0
    beta0 <- fitlasso[1]
    beta <- fitlasso[-1]
    if (OLS & sum(selectvar) > 0) {
      ls.obj <- mls(x[, selectvar, drop = FALSE], y, tau, standardize, intercept)
      beta0 <- ls.obj$beta0
      beta[selectvar] <- ls.obj$beta
    }
  } else {
    globalfit <- glmnet(x, y, lambda = lambda, standardize = standardize, intercept = intercept, ...)
    cvobj <- escv.glmnet(x = x, y = y, lambda = lambda, nfolds = nfolds, foldid = foldid, tau = tau,
                       cv.OLS = cv.OLS, parallel = parallel, standardize = standardize,
                       intercept = intercept, ...)
    
    if (cv.method == "cv") {
      lambda.opt <- cvobj$lambda.cv
    }
    if(cv.method == "cv1se") {
      lambda.opt <- cvobj$lambda.cv1se
    }
    if (cv.method == "escv") {
      lambda.opt <- cvobj$lambda.escv
    } 
    fitlasso <- predict(globalfit, type = "coefficients", s = lambda.opt)
    betalasso <- fitlasso[-1]
    selectvar <- betalasso!=0
    beta0 <- fitlasso[1]
    beta <- fitlasso[-1]
    if (OLS & sum(selectvar) > 0) {
      ls.obj <- mls(x[, selectvar, drop = FALSE], y, tau, standardize, intercept)
      beta0 <- ls.obj$beta0
      beta[selectvar] <- ls.obj$beta
    }    
  }

  if (intercept) {
    meanx <- apply(x, 2, mean)
    mu <- mean(y)
  } else {
    meanx <- rep(0, p)
    mu <- 0
  }
  
  object <- list()
  object$beta0 <- beta0
  object$beta <- beta
  object$lambda <- lambda
  object$meanx <- meanx
  object$mu <- mu
  object$tau <- tau
  object
}

