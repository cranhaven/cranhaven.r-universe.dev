Step <- function(X, Y, M=500, alpha=0.05) {
  n <- dim(X)[1]
  p <- dim(X)[2]
  count.st <- count.nst <- rep(1,p)
  score.nodewiselasso = getFromNamespace("score.nodewiselasso", "hdi")

  Gram <- t(X)%*%X/n
  if (p > floor(n/2)) {
     node <- score.nodewiselasso(X, wantTheta=TRUE, verbose=FALSE, lambdaseq="quantile",
     parallel=FALSE, ncores=2, oldschool = FALSE, lambdatuningfactor = 1)
     Theta <- node$out
  } else {
     Theta <- solve(Gram)
  }

  sreg <- scalreg(X,Y)
  beta.hat <- sreg$coefficients
  sigma.sq <- sum((Y-X%*%beta.hat)^2)/(n-sum(abs(beta.hat)>0))
  beta.db <- beta.hat+Theta%*%t(X)%*%(Y-X%*%beta.hat)/n

  Omega <- diag(Theta%*%Gram%*%t(Theta))*sigma.sq
  margin.test.nst <- sqrt(n)*abs(beta.db)
  margin.test.st <- sqrt(n)*abs(beta.db)/sqrt(Omega)

  eta <- 1:p
  stop.sd <- 1
  while (stop.sd) {
     stat.boot.nst <- rep(NA,M)
     for (i in 1:M) {
       e <- rnorm(n)
       xi.boot <- Theta[eta,]%*%t(X)%*%e*sqrt(sigma.sq)/sqrt(n)
       stat.boot.nst[i] <- max(abs(xi.boot))
     }
     crit.eta.nst <- quantile(stat.boot.nst,1-alpha)
     rej.nst <- margin.test.nst[eta]<crit.eta.nst
     if (sum(rej.nst)==length(rej.nst)) stop.sd <- 0 else eta <- eta[rej.nst]
  }
  count.nst[eta] <- 0

  eta2 <- 1:p
  stop.sd2 <- 1
  while (stop.sd2) {
    stat.boot.st <- rep(NA,M)
     for (i in 1:M) {
       e <- rnorm(n)
       xi.boot <- Theta[eta2,]%*%t(X)%*%e*sqrt(sigma.sq)/sqrt(n)
       stat.boot.st[i] <- max(abs(xi.boot)/sqrt(Omega[eta2]))
     }
     crit.eta.st <- quantile(stat.boot.st,1-alpha)
     rej.st <- margin.test.st[eta2]<crit.eta.st
     if (sum(rej.st)==length(rej.st)) stop.sd2 <- 0 else eta2 <- eta2[rej.st]
  }
  count.st[eta2] <- 0

  rej.nst <- (1:p)[count.nst==1]
  rej.st <- (1:p)[count.st==1]
  result <- list(rej.nst, rej.st)
  names(result) <- c("non-studentized test", "studentized test")
  return(result)
}
