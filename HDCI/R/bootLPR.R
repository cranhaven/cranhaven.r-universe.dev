bootLPR <- function(x, y, lambda2, B = 500, type.boot = "paired", alpha = 0.05, OLS = TRUE,
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
  
  if (missing(lambda2)) {
    lambda2 <- 1/n
  }
  
  selectset <- rep(0,p)
  Beta <- rep(0, p)
  Beta.LPR <- rep(0, p) 
  
  
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
  selectvar <- betalasso != 0
  if (OLS & sum(selectvar) > 0) {
    mls.obj <- mls(x[, selectvar, drop = FALSE], y, tau = tau, standardize = standardize, intercept = intercept)
    Beta[selectvar] <- mls.obj$beta	  
    fit.value <- mypredict(mls.obj, newx = x[, selectvar, drop = FALSE])  		
  }  
  
  PR.obj <- PartRidge(x = x, y = y, lambda2 = lambda2, varset = selectvar, standardize = standardize, intercept = intercept)
  Beta.LPR <- PR.obj$beta	
  
    
  if (type.boot == "residual") {
    fit <- fit.value
    residual <- y - fit
    residual_center <- residual-mean(residual)
    Beta.boot <- matrix(0, nrow = B, ncol = p)
    Beta.boot.LPR <- matrix(0, nrow = B, ncol = p)
      
    out <- list()
    if (!parallel.boot) {
      for (i in 1:B) {
        out[[i]] <- list()
        resam <- sample(1:n, n, replace = TRUE)    			
        ystar <- fit + residual_center[resam]
        if (!OLS) {
          boot.obj <- Lasso(x, ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        } 
        if (OLS) {
          boot.obj <- LassoOLS(x, ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        }	
        out[[i]][[1]] <- boot.obj$beta
        boot.selectvar <- boot.obj$beta != 0			
        boot.PR.obj <- PartRidge(x = x, y = ystar, lambda2 = lambda2, varset = boot.selectvar, standardize = standardize, intercept = intercept)
        out[[i]][[2]] <- boot.PR.obj$beta
      }
    } else {
      resams <- matrix(sample(1:n, n*B, replace = TRUE), nrow = n)
   #   registerDoParallel(ncores.boot)
      out <- foreach(i = 1:B) %dopar% {
        resam <- resams[, i]   			
        ystar <- fit + residual_center[resam]
        if (!OLS) {
          boot.obj <- Lasso(x, ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        } 
        if (OLS) {
          boot.obj <- LassoOLS(x, ystar, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        }	
        beta1 <- boot.obj$beta
        boot.selectvar <- boot.obj$beta != 0			
        boot.PR.obj <- PartRidge(x = x, y = ystar, lambda2 = lambda2, varset = boot.selectvar, standardize = standardize, intercept = intercept)
        beta2 <- boot.PR.obj$beta
        list(beta1 = beta1, beta2 = beta2)
      }
    }
    
    for (i in 1:B) {
      Beta.boot[i,] <- out[[i]][[1]]
      Beta.boot.LPR[i,] <- out[[i]][[2]]
      out[[i]] <- 0
    }
    out <- NULL
    
    interval <- ci(Beta, Beta.boot, alpha = alpha, type = "basic")
    interval.LPR <- ci(Beta.LPR, Beta.boot.LPR, alpha = alpha, type = "basic2", Beta2 = Beta)
  }
    
  if (type.boot == "paired") {
    Beta.boot <- matrix(0, nrow = B, ncol = p)
    Beta.boot.LPR <- matrix(0, nrow = B, ncol = p)
    
    out <- list()
    if (!parallel.boot) {
      for (i in 1:B) {
        out[[i]] <- list()
        resam <- sample(1:n, n, replace = TRUE)		
        rx <- x[resam,]
        ry <- y[resam]				
        if (!OLS) {
          boot.obj <- Lasso(rx, ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        } 
        if (OLS) {
          boot.obj <- LassoOLS(rx, ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        }	
        out[[i]][[1]] <- boot.obj$beta
        boot.selectvar <- boot.obj$beta != 0			
        boot.PR.obj <- PartRidge(x = rx, y = ry, lambda2 = lambda2, varset = boot.selectvar, standardize = standardize, intercept = intercept)
        out[[i]][[2]] <- boot.PR.obj$beta
      }
    } else {
      resams <- matrix(sample(1:n, n*B, replace = TRUE), nrow = n)
   #   registerDoParallel(ncores.boot)
      out <- foreach(i = 1:B) %dopar% {
        resam <- resams[, i]			
        rx <- x[resam, ]
        ry <- y[resam]				
        if (!OLS) {
          boot.obj <- Lasso(rx, ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        } 
        if (OLS) {
          boot.obj <- LassoOLS(rx, ry, lambda = lambda.opt, standardize = standardize, intercept = intercept, ...)
        }	
        beta1 <- boot.obj$beta
        boot.selectvar <- boot.obj$beta != 0			
        boot.PR.obj <- PartRidge(x = rx, y = ry, lambda2 = lambda2, varset = boot.selectvar, standardize = standardize, intercept = intercept)
        beta2 <- boot.PR.obj$beta
        list(beta1 = beta1, beta2 = beta2)
      }
    }
    
    for (i in 1:B) {
      Beta.boot[i,] <- out[[i]][[1]]
      Beta.boot.LPR[i,] <- out[[i]][[2]]
      out[[i]] <- 0
    }
    out <- NULL
    
    interval <- ci(Beta, Beta.boot, alpha = alpha, type = "quantile")
    interval.LPR <- ci(Beta.LPR, Beta.boot.LPR, alpha = alpha, type = "quantile")
  }			
    
  object <- list(lambda.opt = lambda.opt, Beta = Beta, Beta.LPR = Beta.LPR, interval = interval, interval.LPR = interval.LPR)	 
  object
}






