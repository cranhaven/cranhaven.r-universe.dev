Sim.CI <- function(X, Y, set, M=500, alpha=0.95) {

 n <- dim(X)[1]
 p <- dim(X)[2]
 Gram <- t(X)%*%X/n
 score.nodewiselasso = getFromNamespace("score.nodewiselasso", "hdi")
 if (p > floor(n/2)) {
    node <- score.nodewiselasso(X, wantTheta=TRUE, verbose=FALSE, lambdaseq="quantile",
    parallel=FALSE, ncores=2, oldschool = FALSE, lambdatuningfactor = 1)
    Theta <- node$out
 } else {
    Theta <- solve(Gram)
 }

  sreg <- scalreg(X, Y)
  beta.hat <- sreg$coefficients
  sigma.sq <- sum((Y-X%*%beta.hat)^2)/(n-sum(abs(beta.hat)>0))
  beta.db <- beta.hat+Theta%*%t(X)%*%(Y-X%*%beta.hat)/n
  Omega <- diag(Theta%*%Gram%*%t(Theta))*sigma.sq
  stat.boot.st <- stat.boot.nst <- rep(NA,M)

  for (i in 1:M) {
     e <- rnorm(n)
     xi.boot <- Theta[set,]%*%t(X)%*%e*sqrt(sigma.sq)/sqrt(n)
     stat.boot.nst[i] <- max(abs(xi.boot))
     stat.boot.st[i] <- max(abs(xi.boot)/sqrt(Omega[set]))
  }

  crit.nst <- quantile(stat.boot.nst, alpha)
  crit.st <- quantile(stat.boot.st, alpha)

  up.nst <- beta.db[set] + crit.nst/sqrt(n)
  low.nst <- beta.db[set] - crit.nst/sqrt(n)

  up.st <- beta.db[set] + crit.st*sqrt(Omega[set])/sqrt(n)
  low.st <- beta.db[set] - crit.st*sqrt(Omega[set])/sqrt(n)

  band.nst <- rbind(low.nst, up.nst)
  band.st <- rbind(low.st, up.st)
  result <- list(beta.db[set], band.nst, band.st)
  names(result) <- c("de-biased Lasso", "band.nst", "band.st")
  return(result)
}
