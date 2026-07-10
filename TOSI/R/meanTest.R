# Mean testing ------------------------------------------------------------
gendata_Mean <- function(n, p, s0= floor(p/2), seed=1, rho= 1, tau=1){
  mu <- rep(0, p)
  # set.seed(1)
  mu[1:s0] <- runif(s0)* rho
  set.seed(seed)
  X <- mvrnorm(n=n, mu=mu, Sigma = tau*cor.mat(p, rho=0.5))
  return(list(X=X, mu=mu, p0=s0))
}



MeanMax <- function(X, test.set, Nsplit = 5,frac.size=0.5, standardized=FALSE,alpha=0.05, seed=1){

  ## Conservative inference by combing p-values based on quantile.

  n <- nrow(X)

  ns <- round(n*frac.size)
  Pvec <- numeric(Nsplit)
  for(im in 1: Nsplit){

    set.seed(im+seed)
    ids <- sample(n, ns)
    hmu <- colMeans(X[ids,])
    abs_muG <- abs(hmu[test.set])
    if(standardized){
      std <- apply(as.matrix(X[ids, test.set], ncol=length(test.set)), 2, sd)
      abs_muG <- abs_muG / std
    }
    K <- min(1, length(test.set))
    id1 <- order(abs_muG, decreasing = T)[1:K]
    test.set1 <- test.set[id1]
    its <- setdiff(1:n, ids)
    n0 <- length(its)

    hmu <- mean(X[its, test.set1])
    hsd <- sd(X[its, test.set])
    T1 <- sqrt(n0) * abs(hmu / hsd)
    Pvec[im] <- 2*(1- pnorm(T1))

    if(Nsplit==1){
      maxC1 <- qchisq(1-alpha, 1)
      T1 <- n0 * (hmu^2)/ (hsd^2)
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


MeanMin <- function(X, test.set, Nsplit = 5, frac.size=0.5, standardized=FALSE, alpha=0.05, seed=1){

  ## Conservative inference by combing p-values based on quantile.
  n <- nrow(X)

  ns <- round(n*frac.size)
  Pvec <- numeric(Nsplit)
  for(im in 1: Nsplit){

    set.seed(im+seed)
    ids <- sample(n, ns)
    hmu <- colMeans(X[ids,])
    abs_muG <- abs(hmu[test.set])
    if(standardized){
      std <- apply(as.matrix(X[ids, test.set], ncol=length(test.set)), 2, sd)
      abs_muG <- abs_muG / std
    }
    K <- min(1, length(test.set))
    id1 <- order(abs_muG, decreasing = F)[1:K]
    test.set1 <- test.set[id1]
    its <- setdiff(1:n, ids)
    n0 <- length(its)

    hmu <- mean(X[its, test.set1])
    hsd <- sd(X[its, test.set])
    T1 <- sqrt(n0) * abs(hmu / hsd)
    Pvec[im] <-  2*(1- pnorm(T1))
    if(Nsplit==1){
      maxC1 <- qchisq(1-alpha, 1)
      T1 <- n0 * (hmu^2)/ (hsd^2)
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

