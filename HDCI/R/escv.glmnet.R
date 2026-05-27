escv.glmnet <- function(x, y, lambda = NULL, nfolds = 10, foldid, cv.OLS = FALSE, tau = 0, parallel = FALSE,
                      standardize = TRUE, intercept = TRUE, ...){
  

  if (!is.null(lambda) && length(lambda) < 2){
    stop("Need more than one value of lambda for escv.glmnet")
  }
  n <- nrow(x)
  p <- ncol(x)
  y <- drop(y)
  glmnet.call <- match.call(expand.dots = TRUE)
  which <- match(c("nfolds", "foldid"), names(glmnet.call), F)
  if (any(which)) {
    glmnet.call <- glmnet.call[-which]
  }
  glmnet.call[[1]] <- as.name("glmnet")
  glmnet.object <- glmnet(x, y, lambda = lambda, standardize = standardize, 
                          intercept = intercept, ...)
  lambda <- glmnet.object$lambda
  glmnet.object$call <- glmnet.call
  if (missing(foldid)) {
    foldid <- sample(rep(seq(nfolds), length = n))
  } else {
    nfolds <- max(foldid)
  } 
  if (nfolds < 3) {
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  }
  
  out<-list()
  if (!parallel) {
    for (k in 1:nfolds) {
      test <- foldid == k
      train <- foldid != k
      obj <- glmnet(x[train, , drop = FALSE], y[train], lambda = lambda, 
                    standardize = standardize, intercept = intercept, ...)
      fitmat <- predict(obj, newx = x)
      predtest <- predict(obj, newx = x[test, , drop = FALSE])				
      residmat <- apply((y[test] - predtest)^2, 2, mean)
      out[[k]] <- list(residmat = residmat, fitmat = fitmat)
    }
  } else {
   #  registerDoParallel(nfolds)
    out <- foreach(k = 1:nfolds, .packages = c("glmnet")) %dopar% {
      test <- foldid == k
      train <- foldid != k
      obj <- glmnet(x[train, , drop=FALSE], y[train], lambda = lambda,
                    standardize = standardize, intercept = intercept, ...)
      fitmat <- predict(obj, newx = x)
      predtest <- predict(obj, newx = x[test, , drop = FALSE])
      residmat <- apply((y[test] - predtest)^2, 2, mean)
      list(residmat = residmat, fitmat = fitmat)			
    }
  }
  
  residmat <- matrix(0, length(lambda), nfolds)
  residmates <- matrix(0, length(lambda), nfolds)
  fitmat <- array(0, dim = c(n, length(lambda), nfolds))
  
  for (k in 1:nfolds) {
    residmat[,k] <- out[[k]]$residmat
    fitmat[,,k] <- out[[k]]$fitmat
    out[[k]]$residmat <- NULL
    out[[k]]$fitmat <- NULL
  }
  
  meanfit <- apply(fitmat, c(1,2), mean)
  meanfit2 <- apply(meanfit^2, 2, sum)
  for (k in 1:nfolds) {
    residmates[,k] <- apply((fitmat[, , k] - meanfit)^2, 2, sum) / meanfit2
  }
  residmates[is.na(residmates)] <- Inf
  
  cv <- apply(residmat, 1, mean)
  cv.error <- sqrt(apply(residmat, 1, var) / nfolds)	
  es <- apply(residmates, 1, mean)
  es.error <- sqrt(apply(residmates, 1, var) / nfolds)
  
  indcv <- which.min(cv)
  lambda.cv <- lambda[indcv]
  indcv0 <- indcv
  
  cv1se <- cv
  cv1se[cv <= (cv[indcv] + cv.error[indcv])] <- cv[indcv] + cv.error[indcv]
  indcv1se <- which.min(cv1se)
  lambda.cv1se <- lambda[indcv1se]	
  
  indescv <- which.min(es[1:indcv])
  lambda.escv <- lambda[indescv]
  
  # if cv.OLS, refitting the model by OLS or modified Least Squares
  if (cv.OLS) {
    out <- list()
    if (!parallel) {
      for (k in 1:nfolds) {
        test <- foldid == k
        train <- foldid != k
        obj <- glmnet(x[train, , drop = FALSE], y[train], lambda = lambda[1:indcv], 
                    standardize = standardize, intercept = intercept, ...)
        fitmat <- predict(obj, newx = x)
        predtest <- predict(obj, newx = x[test, , drop = FALSE])

        selectset0 <- rep(0, p)
        for (i in 1:indcv) {
          selectset <- abs(obj$beta[,i]) > 0
          if (sum(selectset) > 0) {
            if (sum(abs(selectset - selectset0)) > 0) {
              mls.obj <- mls(x[train, selectset, drop = FALSE], y[train], tau = tau,
                             standardize = standardize, intercept = intercept)
              fitmat[,i] <- mypredict(mls.obj, newx = x[, selectset, drop = FALSE])
              predtest[,i] <- mypredict(mls.obj, newx = x[test, selectset, drop=FALSE])
            } else {
              fitmat[,i] <- fitmat[, i - 1]
              predtest[,i] <- predtest[, i - 1]
            }
            selectset0 <- selectset
          }		
        }
        residmat <- apply((y[test] - predtest)^2, 2, mean)
        out[[k]] <- list(residmat = residmat, fitmat = fitmat)
      }			
    } else {
  #    registerDoParallel(nfolds)
      out <- foreach(k = 1:nfolds) %dopar% {
        test <- foldid == k
        train <- foldid != k
        obj <- glmnet(x[train, , drop = FALSE], y[train], lambda = lambda[1:indcv], 
                    standardize = standardize, intercept = intercept, ...)
        fitmat <- predict(obj, newx = x)
        predtest <- predict(obj, newx = x[test, , drop = FALSE])
        
        selectset0 <- rep(0, p)
        for (i in 1:indcv) {
          selectset <- abs(obj$beta[,i]) > 0
          if (sum(selectset) > 0) {
            if (sum(abs(selectset - selectset0)) > 0) {
              mls.obj <- mls(x[train, selectset, drop = FALSE], y[train], tau = tau,
                           standardize = standardize, intercept = intercept)
              fitmat[,i] <- mypredict(mls.obj, newx = x[, selectset, drop = FALSE])
              predtest[,i] <- mypredict(mls.obj, newx = x[test, selectset, drop = FALSE])
            } else {
              fitmat[,i] <- fitmat[, i - 1]
              predtest[,i] <- predtest[, i - 1]
            }
            selectset0 <- selectset
          }
        }
        residmat <- apply((y[test] - predtest)^2, 2, mean)
        list(residmat = residmat, fitmat = fitmat)			
      }
    }
    
    residmat <- matrix(0, indcv, nfolds)
    residmates <- matrix(0, indcv, nfolds)
    fitmat <- array(0, dim = c(n, indcv, nfolds))
    
    for (k in 1:nfolds) {
      residmat[,k] <- out[[k]]$residmat
      fitmat[,,k] <- out[[k]]$fitmat
      out[[k]]$residmat <- NULL
      out[[k]]$fitmat <- NULL
    }
    
    meanfit <- apply(fitmat, c(1,2), mean)
    meanfit2 <- apply(meanfit^2, 2, sum)
    for (k in 1:nfolds) {
      residmates[,k] <- apply((fitmat[,,k] - meanfit)^2, 2, sum) / meanfit2
    }
    residmates[is.na(residmates)] <- Inf
    
    cv <- apply(residmat, 1, mean)
    cv.error <- sqrt(apply(residmat, 1, var) / nfolds)	
    es <- apply(residmates, 1, mean)
    es.error <- sqrt(apply(residmates, 1, var) / nfolds)
    
    indcv <- which.min(cv)
    lambda.cv <- lambda[indcv]
    
    cv1se <- cv
    cv1se[cv <= (cv[indcv] + cv.error[indcv])] <- cv[indcv] + cv.error[indcv]
    indcv1se <- which.min(cv1se)
    lambda.cv1se <- lambda[indcv1se]	
    
    indescv <- which.min(es[1:indcv0])
    lambda.escv <- lambda[indescv]
    
  }
  
  object <- list(lambda = lambda, glmnet.fit = glmnet.object, cv = cv, cv.error = cv.error, es = es, es.error = es.error, 
                 lambda.cv = lambda.cv, lambda.cv1se = lambda.cv1se, lambda.escv = lambda.escv) 
  object
}