# Regssion testing --------------------------------------------------------
gendata_Reg <- function(n=100, p = 20, s0=5, rho=1, seed=1){
  # set.seed(1)
  beta <- rep(0,p)
  beta[1:s0] <- runif(s0,0,2) *rho
  set.seed(seed)
  Sigma <- matrix(NA, p, p)
  for (i in 1:p) Sigma[i,] <- 0.8^(abs(i-(1:p)))
  X <- matrix(rnorm(n*p), n, p)
  X <- t(t(chol(Sigma))%*%t(X))

  # Only first three coefficients are not zeros
  Y <- X %*%beta+rt(n,4)/sqrt(2)
  return(list(Y=Y, X=X, beta0=beta, index_nz=1:s0))
}




score.partialnodewiselasso <- function (x, index_set,wantTheta = FALSE, verbose = FALSE, lambdaseq = "quantile",
                                        parallel = FALSE, ncores = 8, oldschool = FALSE, lambdatuningfactor = 1,
                                        cv.verbose = FALSE, do.ZnZ = TRUE)
{


  nodewise.getlambdasequence = getFromNamespace("nodewise.getlambdasequence", "hdi")
  nodewise.getlambdasequence.old = getFromNamespace("nodewise.getlambdasequence.old", "hdi")
  cv.nodewise.bestlambda = getFromNamespace("cv.nodewise.bestlambda", "hdi")
  improve.lambda.pick <- getFromNamespace("improve.lambda.pick", "hdi")
  score.getZforlambda <- getFromNamespace("score.getZforlambda", "hdi")

  lambdas <- switch(lambdaseq, quantile = nodewise.getlambdasequence(x),
                    linear = nodewise.getlambdasequence.old(x, verbose),
                    stop("invalid 'lambdaseq': ", lambdaseq))
  if (verbose) {
    cat("Using the following lambda values:", lambdas,
        "\n")
  }
  cvlambdas <- cv.nodewise.bestlambda(lambdas = lambdas, x = x,
                                      parallel = parallel, ncores = ncores, oldschool = oldschool,
                                      verbose = cv.verbose)
  if (verbose) {
    cat(paste("lambda.min is", cvlambdas$lambda.min),
        "\n")
    cat(paste("lambda.1se is", cvlambdas$lambda.1se),
        "\n")
  }
  if (do.ZnZ) {
    bestlambda <- improve.lambda.pick(x = x, parallel = parallel,
                                      ncores = ncores, lambdas = lambdas, bestlambda = cvlambdas$lambda.min,
                                      verbose = verbose)
    if (verbose) {
      cat("Doing Z&Z technique for picking lambda\n")
      cat("The new lambda is", bestlambda, "\n")
      cat("In comparison to the cross validation lambda, lambda = c * lambda_cv\n")
      cat("c=", bestlambda/cvlambdas$lambda.min,
          "\n")
    }
  }
  else {
    if (lambdatuningfactor == "lambda.1se") {
      if (verbose)
        cat("lambda.1se used for nodewise tuning\n")
      bestlambda <- cvlambdas$lambda.1se
    }
    else {
      if (verbose)
        cat("lambdatuningfactor used is", lambdatuningfactor,
            "\n")
      bestlambda <- cvlambdas$lambda.min * lambdatuningfactor
    }
  }
  if (verbose) {
    cat("Picked the best lambda:", bestlambda, "\n")
  }
  if (wantTheta) {
    out <- score.getpartialThetaforlambda(x = x, index_set=index_set,lambda = bestlambda,
                                          parallel = parallel, ncores = ncores, oldschool = TRUE,
                                          verbose = verbose)
  }
  else {
    Z <- score.getZforlambda(x = x, lambda = bestlambda,
                             parallel = parallel, ncores = ncores, oldschool = oldschool)
    out <- Z
  }
  return.out <- list(out = out, bestlambda = bestlambda)
  return(return.out)
}

score.getpartialThetaforlambda <- function (x,index_set, lambda, parallel = FALSE, ncores = 8, oldschool = FALSE,
                                            verbose = FALSE, oldtausq = TRUE)
{
  message("Calculating Thetahat by doing nodewise regressions and dropping the unpenalized intercept")
  n <- nrow(x)
  p <- ncol(x)
  p1 <- length(index_set)
  C <- matrix(0, p, p1)

  T2 <- numeric(p1)
  if (oldschool) {

    message("doing getThetaforlambda oldschool")
    for (ii in 1:p1) {
      i <- index_set[ii]
      C[i,ii] <- 1
      glmnetfit <- glmnet(x[, -i], x[, i])
      coeffs <- as.vector(predict(glmnetfit, x[, -i], type = "coefficients",
                                  s = lambda))[-1]
      C[-i, ii] <- -as.vector(coeffs)
      if (oldtausq) {
        T2[ii] <- as.numeric(crossprod(x[, i])/n - x[,
                                                     i] %*% (x[, -i] %*% coeffs)/n)
      }
      else {
        T2[ii] <- as.numeric((x[, i] %*% (x[, i] - predict(glmnetfit,
                                                           x[, -i], s = lambda)))/n)
      }
    }
  }
  else {
    stop("not implemented yet!")
  }
  if(p1==1){
    T2 <- matrix(1/T2, 1,1)
  }else{
    T2 <- diag(1/T2)
  }
  thetahat <- C %*% T2
  return(thetahat)
}

getTheta <- function(Xts, index_set){
  node2 <- score.partialnodewiselasso(Xts, index_set=index_set, wantTheta=TRUE, verbose=FALSE, lambdaseq="quantile",
                                      parallel=FALSE, ncores=2, oldschool = FALSE, lambdatuningfactor = 1)
  Theta <- node2$out
  return(Theta)
}


RegMax <- function(X, Y,  G1, Nsplit = 5, sub.frac=0.5, alpha=0.05, seed=1,  standardized=FALSE){
  # require(glmnet)
  # require(hdi)
  # require(SIS)
  # require(scalreg)
  n <- dim(X)[1]
  p <- dim(X)[2]

  n1 <- floor(sub.frac*n)
  n0 <- n-floor(n1)

  Pvec <- numeric(Nsplit)
  for(i in 1:Nsplit){
    # Devide sample into two parts
    set.seed(seed+i)
    S1 <- sample(1:n, n1, replace=FALSE)
    X.sub <- X[S1,]
    if(standardized){
      X.sub <- scale(X.sub)
    }
    Y.sub <- Y[S1]
    cvfit <- cv.glmnet(X.sub, Y.sub, intercept=FALSE)
    cf <- as.numeric(coef(cvfit, s="lambda.min"))[-1]
    cf_testset <- cf[G1]
    # sort the coefficients
    K <- min(1, length(G1) ) # only caputure the maximum coefs.
    id_test.set <- order(abs(cf_testset), decreasing = T)[1:K]
    test.set <- G1[id_test.set]
    tsXid <- setdiff(1:n, S1)
    Xts <- X[tsXid, ]; n0 <- nrow(Xts)
    Yts <- Y[tsXid]


    # score.nodewiselasso = getFromNamespace("score.nodewiselasso", "hdi")
    # node <- score.nodewiselasso(Xts, wantTheta=TRUE, verbose=FALSE, lambdaseq="quantile",
    #                             parallel=FALSE, ncores=2, oldschool = FALSE, lambdatuningfactor = 1)
    # Theta <- node$out
    Theta_index <- getTheta(Xts, index_set = test.set)
    Gram<- t(Xts)%*%Xts/n0

    sreg <- scalreg::scalreg(Xts,Yts)
    beta.hat <- sreg$coefficients
    sigma.sq <- sum((Yts-Xts%*%beta.hat)^2)/(n0-sum(abs(beta.hat)>0))

    index <- test.set

    # Omega <- (t(Theta[,index])%*%Gram%*%Theta[,index])*sigma.sq
    # beta.db <- beta.hat[index]+Theta[index,]%*%t(Xts)%*%(Yts-Xts%*%beta.hat)/n0
    Omega <- (t(Theta_index)%*%Gram%*%Theta_index)*sigma.sq
    beta.db <- beta.hat[index]+t(Theta_index)%*%t(Xts)%*%(Yts-Xts%*%beta.hat)/n0
    T1 <- n0 * beta.db^2 / Omega;
    Pvec[i] <-  1- pchisq(T1, 1)
    if(Nsplit==1){
      maxC1 <- qchisq(1-alpha, 1)
      PV <-  1- pchisq(T1, 1)
      res <- c(maxC1, T1, T1 > maxC1, PV)
      names(res) <- c('CriticalValue', 'TestStatistic', 'reject_status', 'p-value')
      return(res)
    }
  }

  Pvec <- p.adjust(Pvec, method="BH")
    gamma <- alpha
    reject <- 0
    if(mean(Pvec <= gamma) >= 1/Nsplit){
      reject <- 1
    }
  adj.pval <- min(Pvec)

  res <- c('reject_status'=reject,   'adjusted_p-value'=adj.pval)

  return(res)
}

RegMin <- function(X, Y,  G2, Nsplit = 5, sub.frac=0.5, alpha=0.05, seed=1,  standardized=FALSE){

  n <- dim(X)[1]
  p <- dim(X)[2]

  n1 <- floor(sub.frac*n)
  n0 <- n-floor(n1)
  Pvec <- numeric(Nsplit)
  for(i in 1:Nsplit){
    # Devide sample into two parts
    set.seed(i+seed)
    S1 <- sample(1:n, n1, replace=FALSE)
    X.sub <- X[S1,]
    if(standardized){
      X.sub <- scale(X.sub)
    }
    Y.sub <- Y[S1]
    lambda <- 1e-7
    glmnet1 <- glmnet(X.sub, Y.sub, lambda=lambda)
    cf <- coef(glmnet1)[-1]
    cf_testset <- cf[G2]
    # sort the coefficients
    K <- min(1, length(G2) ) # only caputure the minimum coef.
    id_test.set <- order(abs(cf_testset))[1:K]
    test.set <- G2[id_test.set]
    tsXid <- setdiff(1:n, S1)
    Xts <- X[tsXid, ]; n0 <- nrow(Xts)
    Yts <- Y[tsXid]


    # score.nodewiselasso = getFromNamespace("score.nodewiselasso", "hdi")
    # node <- score.nodewiselasso(Xts, wantTheta=TRUE, verbose=FALSE, lambdaseq="quantile",
    #                             parallel=FALSE, ncores=2, oldschool = FALSE, lambdatuningfactor = 1)
    # Theta <- node$out
    Theta_index <- getTheta(Xts, index_set = test.set)
    Gram<- t(Xts)%*%Xts/n0

    sreg <- scalreg::scalreg(Xts,Yts)
    beta.hat <- sreg$coefficients
    sigma.sq <- sum((Yts-Xts%*%beta.hat)^2)/(n0-sum(abs(beta.hat)>0))

    index <- test.set

    # Omega <- (t(Theta[,index])%*%Gram%*%Theta[,index])*sigma.sq
    # beta.db <- beta.hat[index]+Theta[index,]%*%t(Xts)%*%(Yts-Xts%*%beta.hat)/n0
    Omega <- (t(Theta_index)%*%Gram%*%Theta_index)*sigma.sq
    beta.db <- beta.hat[index]+t(Theta_index)%*%t(Xts)%*%(Yts-Xts%*%beta.hat)/n0
    T1 <- n0*beta.db^2 / Omega;
    Pvec[i] <-  1- pchisq(T1, 1)

    if(Nsplit==1){
      maxC1 <- qchisq(1-alpha, 1)
      PV <-  1- pchisq(T1, 1)

      res <- c(maxC1, T1, T1 > maxC1, PV)
      names(res) <- c('CriticalValue', 'TestStatistic', 'reject_status', 'p-value')
      return(res)
    }
  }
    Pvec <- p.adjust(Pvec, method="BH")
    gamma <- alpha
    reject <- 0
    if(mean(Pvec <= gamma) >= 1/Nsplit){
      reject <- 1
    }
    adj.pval <- min(Pvec)

    res <- c('reject_status'=reject,   'adjusted_p-value'=adj.pval)

    return(res)
}

