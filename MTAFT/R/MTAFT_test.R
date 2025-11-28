#' Perform score-type test for the presence of threshold effect in multi-threshold situations.
#'
#' This function performs a score-type test statistics for the presence of threshold effect in multi-threshold situations.
#'
#' @param Y Response variable.
#' @param X Covariates.
#' @param Tq Threshold variable.
#' @param delta Indicator vector for censoring.
#' @param nboots Number of bootstrap iterations.
#'
#' @return p-value result indicating the presence of threshold effect.
#' @export
#'
#' @examples
#' \donttest{
#' # Generate simulated data with 500 samples and normal error distribution
#' dataset <- MTAFT_simdata(n = 500, err = "normal")
#' Y <- dataset[, 1]
#' delta <- dataset[, 2]
#' Tq <- dataset[, 3]
#' X <- dataset[, -c(1:3)]
#'
#' # Perform score-type test with 500 bootstraps
#' pval <- MTAFT_test(Y, X, Tq, delta, nboots = 500)
#'
#' # Perform score-type test with 1000 bootstraps
#' pval <- MTAFT_test(Y, X, Tq, delta, nboots = 1000)
#' }

MTAFT_test <- function(Y,X,Tq,delta,nboots)
{
  X <- as.matrix(X)
  o <- order(Y)
  Y1 <- Y[o]
  Tq1 <- Tq[o]
  X1 <- X[o, ]
  delta1 <- delta[o]
  n = length(Y)

  Wn1 <- delta1
  n1 <- length(Y)
  Wn1[1] <- delta1[1]/n1
  tt <- 1
  for (i in 2:n1) {
    j <- i - 1
    tt <- tt * ((n1 - j)/(n1 - j + 1))^delta1[j]
    Wn1[i] <- delta1[i]/(n1 - i + 1) * tt
  }

  W <- sqrt(Wn1)
  Y2 <- W * Y1
  X2 <- W * X1
  lmr <- lm(Y2 ~ W+X2 - 1)
  beta.o <- lmr$coefficients
  eps.o <- lmr$residuals

  innv.mat <- matrix(rnorm(nboots*n,0,1),nrow=nboots)

  ##test construction
  WX2 <- cbind(W,X2)
  WX <- cbind(1,X1)
  p <- ncol(WX2)
  Qhat.inv <- solve(t(WX2) %*% WX2)

  Gamma <- seq(quantile(Tq1,0.1),quantile(Tq1,0.9),length=100)
  lG <- length(Gamma)
  Ln = rep(NA,lG)
  Ln.mat = matrix(NA,nboots,lG)

  for(ll in 1:lG){
    tau <- Gamma[ll]
    Xia <- W * WX * (Tq1 <= tau)
    Q1hat <- t(WX2) %*% Xia
    cR <- (Xia - WX2 %*% Qhat.inv %*% t(Q1hat))*eps.o
    Rns <- apply(cR,2,sum)/sqrt(n)
    Ln[ll] = norm(Rns,type = "2")

    for(bb in 1:nboots){
      xi <- innv.mat[bb,]
      Rnxi =  apply(cR*xi,2,sum)/sqrt(n)
      Ln.mat[bb,ll] = norm(Rnxi,type = "2")
    }

  }

  Ln.stat <- max(Ln)
  Lns.stat <- apply(Ln.mat,1,max)

  pval <- mean(Lns.stat >= Ln.stat)

  return(pval)

}

