ST <- function(X.f, Y.f, sub.size, test.set, M=500, alpha=0.05) {
  n <- dim(X.f)[1]
  p <- dim(X.f)[2]

  n1 <- sub.size
  n0 <- n-floor(n1)
  S1 <- sample(1:n, floor(n1), replace=FALSE)
  X.sub <- X.f[S1,]
  Y.sub <- Y.f[S1]
  cvfit <- cv.glmnet(X.sub, Y.sub, intercept=FALSE)
  cf <- as.numeric(coef(cvfit, s="lambda.min"))[-1]
  set1 <- (1:p)[abs(cf)>0]
  resi <- Y.sub-X.sub%*%cf
  beta.m <- t(standardize(X.sub[,-set1]))%*%resi
  screen.set <- sort(order(abs(beta.m),decreasing=TRUE)[1:(n0-1-length(set1))])
  a <- (1:p)[-set1]
  screen.set <- union(a[screen.set],set1)
  X <- X.f[-S1,screen.set]
  Y <- Y.f[-S1]

  score.nodewiselasso = getFromNamespace("score.nodewiselasso", "hdi")
  node <- score.nodewiselasso(X, wantTheta=TRUE, verbose=FALSE, lambdaseq="quantile",
  parallel=FALSE, ncores=2, oldschool = FALSE, lambdatuningfactor = 1)
  Theta <- node$out
  Gram<-t(X)%*%X/n0

  sreg <- scalreg(X,Y)
  beta.hat <- sreg$coefficients
  sigma.sq <- sum((Y-X%*%beta.hat)^2)/(n0-sum(abs(beta.hat)>0))
  test.set.i <- intersect(screen.set,test.set)
  index <- screen.set%in%test.set.i

  Omega <- diag(Theta%*%Gram%*%t(Theta))*sigma.sq
  beta.db <- beta.hat+Theta%*%t(X)%*%(Y-X%*%beta.hat)/n0
  margin.st <- sqrt(n0)*abs(beta.db[index])/sqrt(Omega[index])
  margin.nst <- sqrt(n0)*abs(beta.db[index])
  stat.st <- max(margin.st)
  stat.nst <- max(margin.nst)

  stat.boot.st <- stat.boot.nst <- rep(NA,M)
  for (i in 1:M) {
     e <- rnorm(n0)
     xi.boot <- Theta[index,]%*%t(X)%*%e*sqrt(sigma.sq)/sqrt(n0)
     stat.boot.nst[i] <- max(abs(xi.boot))
     stat.boot.st[i] <- max(abs(xi.boot/sqrt(Omega[index])))
  }

  if (stat.nst>quantile(stat.boot.nst,1-alpha)) rej.nst <- "reject" else rej.nst <- "fail to reject"
  if (stat.st>quantile(stat.boot.st,1-alpha)) rej.st <- "rejct" else rej.st <- "fail to reject"

  result <- list(stat.nst, rej.nst, stat.st, rej.st)
  names(result) <- c("non-studentized test","non-studentized test","studentized test","studentized test")
  return(result)
}
