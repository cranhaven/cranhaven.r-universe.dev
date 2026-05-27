LPR <- function(x, y, lambda = NULL, fix.lambda = TRUE, lambda2, cv.method = "cv", nfolds = 10, foldid,  
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
  
  if (missing(lambda2)) {
    lambda2 <- 1/n
  }
  
  if (fix.lambda) {
    globalfit <- glmnet(x, y, standardize = standardize, intercept = intercept, ...)
    fitlasso <- predict(globalfit, type = "coefficients", s = lambda)
    betalasso <- fitlasso[-1]
    selectvar <- betalasso != 0
    PR.obj <- PartRidge(x = x, y = y, lambda2 = lambda2, varset = selectvar, standardize = standardize, intercept = intercept)
    beta0 <- PR.obj$beta0
    beta <- PR.obj$beta
    meanx <- PR.obj$meanx
    normx <- PR.obj$normx
    mu <- PR.obj$mu
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
    betalasso <- fitlasso[-1]
    selectvar <- betalasso!=0
    PR.obj <- PartRidge(x = x, y = y, lambda2 = lambda2, varset = selectvar, standardize = standardize, intercept = intercept)
    beta0 <- PR.obj$beta0
    beta <- PR.obj$beta
    meanx <- PR.obj$meanx
    normx <- PR.obj$normx
    mu <- PR.obj$mu    
  }
  
  object <- list()
  object$beta0 <- beta0
  object$beta <- beta
  object$lambda <- lambda
  object$lambda2 <- lambda2
  object$meanx <- meanx
  object$normx <- normx
  object$mu <- mu
  object$tau <- tau
  object
}

