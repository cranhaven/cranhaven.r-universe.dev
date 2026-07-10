# This function calculates Grace coefficients and p-values.
# Author:             Sen Zhao
# Email:              sen-zhao@sen-zhao.com
# ----------------------------------------------------------------------------
# Arguments:
# Y:                  n by 1 vector of the response variable.
# X:                  n (number of rows) by p (number of columns) design matrix.
# L:                  p by p symmetric matrix of the penalty weight matrix.
# lambda.L:           tuning parameters of the penalty weight matrix.
# lambda.2:           tuning parameters of the ridge penalty.
# normalize.L:        binary parameter indicating whether the penalty weight matrix 
#                     needs to be normalized beforehand.
# K:                  number of folds in cross-validation.
# sigma.error:        error standard deviation. If NULL, scaled lasso is applied.
# enable.group.test:  binary parameter indicating whether group tests should be enabled.
# eta, C:             parameters of the grace test; see Zhao & Shojaie (2016) for reference.
# verbose:            whether computation progress should be printed.
# ----------------------------------------------------------------------------
# Outputs:
# intercept:          intercept of the linear regression model.
# beta:               regression coefficients (slopes) of the linear regression model.
# pvalue:             p-values of individual hypothesis tests.
# group.test:         function to perform group-wise hypothesis test.

grace.test <- function(Y, X, L = NULL, lambda.L = NULL, lambda.2 = 0, normalize.L = FALSE, eta = 0.05, C = 4 * sqrt(3), K = 10, sigma.error = NULL, verbose = FALSE){
  checkdata(X, Y, L)
  
  n <- nrow(X)
  p <- ncol(X)
  if(is.null(L)){
    L <- matrix(0, nrow = p, ncol = p)
    lambda.L <- 0
  }
  lambda.L <- unique(sort(lambda.L, decreasing = TRUE))
  lambda.2 <- unique(sort(lambda.2, decreasing = TRUE))
  
  ori.Y <- Y
  ori.X <- X
  if(min(lambda.L) < 0 | min(lambda.2) < 0){
    stop("Error: Tuning parameters must be non-negative.")
  }
  if(min(lambda.L) == 0 & min(lambda.2) == 0 & length(lambda.L) == 1 & length(lambda.2) == 1){
    stop("Error: At least one of the tuning parameters must be positive.")
  }
  if(is.numeric(C) & C < 2 * sqrt(2)){
    stop("Error: the initial tuning parameter C is too small. It should be at least 2 * sqrt(2).")
  }
  if(eta <= 0 | eta >= 0.5){
    stop("Error: the sparsity parameter needs to be 0 < eta < 0.5.")
  }
    
  # ----------------------
  # | Data Preprocessing |
  # ----------------------

  Y <- Y - mean(Y)
  scale.fac <- attr(scale(X), "scaled:scale")
  X <- scale(X)
  
  if(normalize.L & !is.null(L)){
    diag(L)[diag(L) == 0] <- 1
    L <- diag(1 / sqrt(diag(L))) %*% L %*% diag(1 / sqrt(diag(L)))
  }
  
  emin <- min(eigen(min(lambda.L) * L + min(lambda.2) * diag(p))$values)
  if(emin < 1e-5){
    stop("Error: The penalty matrix (lambda.L * L + lambda.2 * I) is not always positive definite for all tuning parameters. Consider increase the values of lambda.2.")
  }

  # --------------------------------------------------------
  # | Grace Estimation: see Li and Li (2008) for reference |
  # --------------------------------------------------------
  
  # If more than one tuning parameter is provided, perform K-fold cross-validation  
  if((length(lambda.L) > 1) | (length(lambda.2) > 1)){
    tun <- cvGrace(X, Y, L, lambda.L, 0, lambda.2, K = K)
    lambda.L <- tun[1]
    lambda.2 <- tun[3]
    if(!verbose){
      print(paste("Tuning parameters selected by ", K, "-fold cross-validation:", sep = ""))
      print(paste("lambda.L = ", lambda.L, sep = ""))
      print(paste("lambda.2 = ", lambda.2, sep = ""))
    }
  }
  
  H <- solve(t(X) %*% X + lambda.L * L + lambda.2 * diag(p))
  betahat <- c(H %*% t(X) %*% Y)
  truebetahat <- betahat / scale.fac  # Scale back coefficient estimate
  truealphahat <- mean(ori.Y - ori.X %*% truebetahat)
  
  # ---------------------------------------------------------
  # | Grace Test: see Zhao and Shojaie (2016) for reference |
  # ---------------------------------------------------------
  
  # Error standard deviation
  if(is.null(sigma.error)){
    sig.L <- scalreg(X, Y)$hsigma
  }else{
    sig.L <- sigma.error
  }
  
  # Initial estimator
  if(is.numeric(C)){
    lam <- sig.L * C * sqrt(log(p) / n) / 2
    beta.init <- glmnet(X, Y, lambda = lam, intercept = FALSE)$beta[, 1]
  }else if(C == "cv"){
    lam <- cv.glmnet(X, Y, intercept = FALSE)$lambda.min
    beta.init <- glmnet(X, Y, intercept = FALSE, lambda = lam)$beta[, 1]
  }else if(C == "scaled.lasso"){
    beta.init <- scalreg(X, Y)$coefficients
  }
  
  # Standard error
  covmatrix <- sig.L^2 * H %*% t(X) %*% X %*% H
  se <- sqrt(diag(covmatrix))

  # One step correction
  bias <- H %*% (lambda.L * L + lambda.2 * diag(p)) %*% beta.init
  targ <- abs(H %*% (lambda.L * L + lambda.2 * diag(p)))
  diag(targ) <- 0
  correct <- apply(targ, 1, max) * (log(p) / n)^(0.5 - eta)  # Bias bound
  teststat <- betahat + bias
  
  # Test statistic and p-value
  Tstat <- (abs(teststat) - correct) * (abs(teststat) > correct)
  pval <- 2 * (1 - pnorm(abs(Tstat / se)))
  
  # Group test
  group.testing.function <- function(group, method = "holm"){
    if(!is.vector(group)){
      stop("Error: the argument needs to be an index vector of variables.")
    }else if(length(group) < 2){
      stop("Error: the size of the group needs to be at least two.")
    }
    
    if(method == "max"){
      test.stat <- max(abs(Tstat[group] / se[group]))
      subcormatrix <- diag(1 / sqrt(diag(covmatrix[group, group]))) %*% covmatrix[group, group] %*% diag(1 / sqrt(diag(covmatrix[group, group])))
      absZ <- abs(mvrnorm(n = 100000, mu = rep(0, length(group)), Sigma = subcormatrix))
      maxZ <- apply(absZ, 1, max)
      groupp <- 1 - mean(test.stat > maxZ)
    }else if(method == "holm"){
      groupp <- min(p.adjust(pval[group], method = "holm"))
    }else{
      stop("Error: wrong group testing method.")
    }
    
    return(groupp)
  }

  return(list(intercept = truealphahat, beta = truebetahat, pvalue = pval, group.test = group.testing.function))
}
