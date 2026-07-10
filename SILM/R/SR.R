SR <- function(X, Y) {
  n <- dim(X)[1]
  p <- dim(X)[2]
  Gram <- t(X)%*%X/n
  score.nodewiselasso = getFromNamespace("score.nodewiselasso", "hdi")

  if(p > floor(n/2)) {
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
  stat.st <- sqrt(n)*abs(beta.db)/sqrt(Omega)
  active.db <- (1:p)[stat.st>sqrt(2*log(p))]
  active.lasso <- (1:p)[abs(beta.hat)>0]
  result <- list(active.db, active.lasso)
  names(result) <- c("de-biased Lasso", "scaled Lasso")
  return(result)
}
