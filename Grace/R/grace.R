# This function calculates Grace coefficient estimates.
# Author:       Sen Zhao
# Email:        sen-zhao@sen-zhao.com
# ----------------------------------------------------------------------------
# Arguments:
# Y:            centered n by 1 vector of the response variable.
# X:            standardized n (number of rows) by p (number of columns) design matrix.
# L:            p by p symmetric matrix of the penalty weight matrix.
# lambda.L:     tuning parameters of the penalty weight matrix.
# lambda.1:     tuning parameters of the L_1 penalty.
# lambda.2:     tuning parameters of the ridge penalty.
# normalize.L:  binary parameter indicating whether the penalty weight matrix 
#               needs to be normalized beforehand.
# K:            number of folds in cross-validation.
# verbose:      whether computation progress should be printed.
# ----------------------------------------------------------------------------
# Outputs:
# intercept:    intercept of the linear regression model.
# beta:         regression coefficients (slopes) of the linear regression model.
# ----------------------------------------------------------------------------




grace <- function(Y, X, L, lambda.L, lambda.1 = 0, lambda.2 = 0, normalize.L = FALSE, K = 10, verbose = FALSE){
  checkdata(X, Y, L)
  
  lambda.L <- unique(sort(lambda.L, decreasing = TRUE))
  lambda.1 <- unique(sort(lambda.1, decreasing = TRUE))
  lambda.2 <- unique(sort(lambda.2, decreasing = TRUE))
  
  ori.Y <- Y
  ori.X <- X
  if(min(lambda.L) < 0 | min(lambda.2) < 0 | min(lambda.1) < 0){
    stop("Error: Tuning parameters must be non-negative.")
  }
  if(min(lambda.L) == 0 & min(lambda.2) == 0 & min(lambda.1) == 0 & length(lambda.L) == 1 & length(lambda.2) == 1 & length(lambda.1) == 1){
    stop("Error: At least one of the tuning parameters must be positive.")
  }
  
  # ----------------------
  # | Data Preprocessing |
  # ----------------------
  
  Y <- Y - mean(Y)
  n <- nrow(X)
  p <- ncol(X)
  scale.fac <- attr(scale(X), "scaled:scale")
  X <- scale(X)
  
  if(normalize.L){
    diag(L)[diag(L) == 0] <- 1
    L <- diag(1 / sqrt(diag(L))) %*% L %*% diag(1 / sqrt(diag(L)))  # Normalize L
  }
  
  emin <- min(eigen(min(lambda.L) * L + min(lambda.2) * diag(p))$values)
  if(emin < 1e-5){
    stop("Error: The penalty matrix (lambda.L * L + lambda.2 * I) is not always positive definite for all tuning parameters. Consider increase the value of lambda.2.")
  }
  
  # --------------------------------------------------------
  # | Grace Estimation: see Li and Li (2008) for reference |
  # --------------------------------------------------------
  
  
  # If more than one tuning parameter is provided, perform K-fold cross-validation  
  if((length(lambda.L) > 1) | (length(lambda.1) > 1) | (length(lambda.2) > 1)){
    tun <- cvGrace(X, Y, L, lambda.L, lambda.1, lambda.2, K)
    lambda.L <- tun[1]
    lambda.1 <- tun[2]
    lambda.2 <- tun[3]
    if(!verbose){
      print(paste("Tuning parameters selected by ", K, "-fold cross-validation:", sep = ""))
      print(paste("lambda.L = ", lambda.L, sep = ""))
      print(paste("lambda.1 = ", lambda.1, sep = ""))
      print(paste("lambda.2 = ", lambda.2, sep = ""))
    }
  }
  
  Lnew <- lambda.L * L + lambda.2 * diag(p)
  eL <- eigen(Lnew)
  S <- eL$vectors %*% sqrt(diag(eL$values))
  l1star <- lambda.1
  Xstar <- rbind(X, t(S)) / sqrt(2)
  Ystar <- c(Y, rep(0, p))
  gammastar <- l1star / sqrt(2) / 2 / (n + p)
  betahatstar <- glmnet(Xstar, Ystar, lambda = gammastar, intercept = FALSE, standardize = FALSE, thresh = 1e-11)$beta[, 1]
  betahat <- betahatstar / sqrt(2)
  
  truebetahat <- betahat / scale.fac  # Scale back coefficient estimate
  truealphahat <- mean(ori.Y - ori.X %*% truebetahat)
  return(list(intercept = truealphahat, beta = truebetahat))
}
