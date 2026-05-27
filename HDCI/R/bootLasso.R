bootLasso <- function(x, y, B = 500, type.boot = "residual", alpha = 0.05, 
                  cv.method = "cv", nfolds = 10, foldid, cv.OLS = FALSE, tau = 0, 
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
  Beta <- rep(0, p)
  
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
  Beta <- betalasso
  
  if (type.boot == "residual") {
    fit <- fit.value
    residual <- y - fit
    residual_center <- residual - mean(residual)
    Beta.boot <- matrix(0, nrow = B, ncol = p)
    
    out <- list()
    if (!parallel.boot) {
      for (i in 1:B) {
        resam <- sample(1:n, n, replace = TRUE)  			
        ystar <- fit + residual_center[resam]
        boot.obj <- Lasso(x = x, y = ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        out[[i]] <- boot.obj$beta
      }
    } else {
      resams <- matrix(sample(1:n, n*B, replace = TRUE), nrow = n)
   #   registerDoParallel(ncores.boot)
      out <- foreach(i = 1:B) %dopar% {
        resam <- resams[, i]   			
        ystar <- fit + residual_center[resam]
        boot.obj <- Lasso(x = x, y = ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        boot.obj$beta
      }
    }
    
    for (i in 1:B) {
      Beta.boot[i, ] <- out[[i]]
      out[[i]] <- 0
    }
    out <- NULL
    
    interval <- ci(Beta, Beta.boot, alpha = alpha, type = "basic")
  }
  
  if (type.boot == "paired") {
    Beta.boot <- matrix(0, nrow = B, ncol = p)
    
    out <- list()
    if (!parallel.boot) {
      for (i in 1:B) {
        resam <- sample(1:n, n, replace = TRUE)		
        rx <- x[resam,]
        ry <- y[resam]				
        boot.obj <- Lasso(x = rx, y = ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        out[[i]] <- boot.obj$beta
      }
    } else {
      resams <- matrix(sample(1:n, n*B, replace = TRUE), nrow = n)
  #    registerDoParallel(ncores.boot)
      out <- foreach(i = 1:B) %dopar% {
        resam <- resams[, i]			
        rx <- x[resam, ]
        ry <- y[resam]				
        boot.obj <- Lasso(x = rx, y = ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        boot.obj$beta
      }
    }
    
    for (i in 1:B) {
      Beta.boot[i, ] <- out[[i]]
      out[[i]] <- 0
    }
    out <- NULL
    
    interval <- ci(Beta, Beta.boot, alpha = alpha, type = "quantile")
  }			
  
  object <- list(lambda.opt = lambda.opt, Beta = Beta, interval = interval)	 
  object
}






