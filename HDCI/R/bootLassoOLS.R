bootLassoOLS <- function(x, y, B = 500, type.boot = "residual", alpha = 0.05, OLS = TRUE,
                  cv.method = "cv", nfolds = 10, foldid, cv.OLS = TRUE, tau = 0,
                  parallel = FALSE, standardize = TRUE, intercept = TRUE, 
                  parallel.boot = FALSE, ncores.boot = 1, ...){
  
  x <- as.matrix(x)
  y <- as.numeric(y)
  n <- dim(x)[1]
  p <- dim(x)[2]
  
  if ((type.boot != "residual") & (type.boot != "paired")) {
    stop("type.boot should take value of 'residual' or 'paired'.")
  }

  selectset <- rep(0, p)
  Beta.Lasso <- rep(0, p)
  Beta.LassoOLS <- rep(0, p) 
  
  
  globalfit <- glmnet(x, y, standardize = standardize, intercept = intercept, ...)	
  lambda <- globalfit$lambda
  cvfit <- escv.glmnet(x, y, lambda = lambda, nfolds = nfolds, tau = tau, cv.OLS = cv.OLS, parallel = parallel, 
                    standardize = standardize, intercept = intercept, ...)
  
  if (cv.method == "cv") {
    lambda.opt <- cvfit$lambda.cv
  }
  if (cv.method == "escv") {
    lambda.opt <- cvfit$lambda.escv
  } 
  if (cv.method == "cv1se") {
    lambda.opt <- cvfit$lambda.cv1se
  }
  
  fitlasso <- predict(globalfit, type = "coefficients", s = lambda.opt)
  fit.value <- predict(globalfit, newx = x, s = lambda.opt)
  betalasso <- fitlasso[-1]
  Beta.Lasso <- betalasso
  Beta.LassoOLS <- betalasso
  selectvar <- betalasso != 0
  if (OLS & sum(selectvar) > 0) {
    mls.obj <- mls(x[, selectvar, drop = FALSE], y, tau = tau, standardize = standardize, intercept = intercept)
    Beta.LassoOLS[selectvar] <- mls.obj$beta	  
    fit.value <- mypredict(mls.obj, newx = x[, selectvar, drop = FALSE])  		
  }  
  
  if (type.boot == "residual") {
    fit <- fit.value
    residual <- y - fit
    residual_center <- residual - mean(residual)
    Beta.boot.Lasso <- matrix(0, nrow = B, ncol = p)
    Beta.boot.LassoOLS <- matrix(0, nrow = B, ncol = p)
    
    out <- list()
    if (!parallel.boot) {
      for (i in 1:B) {
        out[[i]] <- list()
        resam <- sample(1:n, n, replace = TRUE)   			
        ystar <- fit + residual_center[resam]
        boot.obj <- Lasso(x = x, y = ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        out[[i]][[1]] <- boot.obj$beta
        if (OLS) {
          boot.obj <- LassoOLS(x = x, y = ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        }	
        out[[i]][[2]] <- boot.obj$beta
      }
    } else {
      resams <- matrix(sample(1:n, n*B, replace = TRUE), nrow = n)
  #    registerDoParallel(ncores.boot)
      out <- foreach(i = 1:B) %dopar% {
        resam <- resams[, i]   			
        ystar <- fit + residual_center[resam]
        boot.obj <- Lasso(x = x, y = ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        beta1 <- boot.obj$beta
        if (OLS) {
          boot.obj <- LassoOLS(x = x, y = ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        }	
        beta2 <- boot.obj$beta
        list(beta1 = beta1, beta2 = beta2)
      }
    }
    
    for (i in 1:B) {
      Beta.boot.Lasso[i,] <- out[[i]][[1]]
      Beta.boot.LassoOLS[i,] <- out[[i]][[2]]
      out[[i]] <- 0
    }
    out <- NULL
    
    interval.Lasso <- ci(Beta.Lasso, Beta.boot.Lasso, alpha = alpha, type = "basic")
    interval.LassoOLS <- ci(Beta.LassoOLS, Beta.boot.LassoOLS, alpha = alpha, type = "basic")
  }
  
  if (type.boot == "paired") {
    Beta.boot.Lasso <- matrix(0, nrow = B, ncol = p)
    Beta.boot.LassoOLS <- matrix(0, nrow = B, ncol = p)
    
    out <- list()
    if (!parallel.boot) {
      for (i in 1:B) {
        out[[i]] <- list()
        resam <- sample(1:n, n, replace = TRUE)		
        rx <- x[resam,]
        ry <- y[resam]				
        boot.obj <- Lasso(x = rx, y = ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        out[[i]][[1]] <- boot.obj$beta
        if (OLS) {
          boot.obj <- LassoOLS(x = rx, y = ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        }	
        out[[i]][[2]] <- boot.obj$beta
      }
    } else {
      resams <- matrix(sample(1:n, n*B, replace = TRUE), nrow = n)
 #     registerDoParallel(ncores.boot)
      out <- foreach(i = 1:B) %dopar% {
        resam <- resams[, i] 			
        rx <- x[resam, ]
        ry <- y[resam]				
        boot.obj <- Lasso(x = rx, y = ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        beta1 <- boot.obj$beta
        if (OLS) {
          boot.obj <- LassoOLS(x = rx, y = ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        }	
        beta2 <- boot.obj$beta
        list(beta1 = beta1, beta2 = beta2)
      }
    }
    
    for (i in 1:B) {
      Beta.boot.Lasso[i,] <- out[[i]][[1]]
      Beta.boot.LassoOLS[i,] <- out[[i]][[2]]
      out[[i]] <- 0
    }  
    out <- NULL
    
    interval.Lasso <- ci(Beta.Lasso, Beta.boot.Lasso, alpha = alpha, type = "quantile")
    interval.LassoOLS <- ci(Beta.LassoOLS, Beta.boot.LassoOLS, alpha = alpha, type = "quantile")
  }			
  
  object <- list(lambda.opt = lambda.opt, Beta.Lasso = Beta.Lasso, Beta.LassoOLS = Beta.LassoOLS, 
               interval.Lasso = interval.Lasso, interval.LassoOLS = interval.LassoOLS)	 
  object
}






