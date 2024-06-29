## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fastLaplace)

## -----------------------------------------------------------------------------
if(requireNamespace("mgcv")){
  sigma2 = 1
  phi = 0.2
  beta.true = c(1,1)
  n = 400
  n.pred = 100
  coords.all<- matrix(runif((n+n.pred)*2),ncol=2,nrow=n+n.pred) # simulate data locations
  X.all <- matrix(runif((n+n.pred)*2),ncol=2,nrow=(n+n.pred))
  suppressMessages(require(fields))
  dist.all <- fields::rdist(coords.all,coords.all) # compute distance matrix
  matern <- function(phi,mat.dist){
    K = (1+sqrt(5)/phi*mat.dist+ 5/(3*phi^2)*mat.dist^2)*exp(-sqrt(5)/phi*mat.dist)
    return(K)
  } # matern 2.5
  V.all <- sigma2*matern(phi,dist.all) # compute covariance matrix
  set.seed(1)
  r.e.all <- mgcv::rmvn(1,rep(0,nrow(coords.all)),V.all) # simulate random effects
  pi.all <- X.all%*%beta.true + r.e.all # linear model
  p.all <- exp(pi.all)/(1+exp(pi.all)) # compute the probability of z = 1 for binary process
  Y.all <- sapply(p.all, function(x) sample(0:1, 1, prob = c(1-x, x))) # simulate binary observations
} else{
  stop("Package \"mgcv\" needed to generate a simulated data set")
}

## -----------------------------------------------------------------------------
Y <- as.matrix(Y.all[1:n],nrow = n)
X <- X.all[1:n,]
coords <- coords.all[1:n,]
data <- data.frame(cbind(Y,X))
colnames(data) = c("Y","X1","X2")
mod.glm <- glm(Y~-1+X1+X2,family="binomial",data=data)
mod.glm.esp <- predict(mod.glm,data, type="response")
mod.glm.s2 <- var(Y - mod.glm.esp)
mod.glm.phi <- 0.1*max(dist(coords))
startinit <- c(mod.glm$coef,log(mod.glm.s2),log(mod.glm.phi))
names(startinit) <- c("X1","X2","logsigma2","logphi")

## -----------------------------------------------------------------------------
result.bin <- fsglmm(Y~-1+X1+X2, kappa=2.5, inits = startinit, data = data,coords = coords,  family = "binomial", ntrial = 1, offset = NA,method.optim = "CG", method.integrate = "NR", control = list(maxit=1000,ndeps=rep(1e-2,4),reltol=0.01),rank = 50)
result.bin$summary

## -----------------------------------------------------------------------------
X.pred <- X.all[(n+1):(n+n.pred),]
coords.pred <- coords.all[(n+1):(n+n.pred),]
pred.sglmm(result.bin,data=X.pred,coords=coords.pred)


## -----------------------------------------------------------------------------
if(requireNamespace("mgcv")){
  sigma2 = 1
  phi = 0.2
  prec = 2
  beta.true = c(1,1)
  n = 400
  n.pred = 100
  coords.all<- matrix(runif((n+n.pred)*2),ncol=2,nrow=n+n.pred) # simulate data locations
  X.all <- matrix(runif((n+n.pred)*2),ncol=2,nrow=(n+n.pred))
  suppressMessages(require(fields))
  dist.all <- fields::rdist(coords.all,coords.all) # compute distance matrix
  matern <- function(phi,mat.dist){
    K = (1+sqrt(5)/phi*mat.dist+ 5/(3*phi^2)*mat.dist^2)*exp(-sqrt(5)/phi*mat.dist)
    return(K)
  } # matern 2.5
  V.all <- sigma2*matern(phi,dist.all) # compute covariance matrix
  set.seed(1)
  r.e.all <- mgcv::rmvn(1,rep(0,nrow(coords.all)),V.all) # simulate random effects
  mu.all <- exp(X.all%*%beta.true+r.e.all) 
  Y.all <- rnbinom(length(mu.all), mu=mu.all,size=prec) 
} else {
  stop("Package \"mgcv\" needed to generate a simulated data set")
}


## -----------------------------------------------------------------------------
if(requireNamespace("MASS")){
  Y <- as.matrix(Y.all[1:n],nrow = n)
  X <- X.all[1:n,]
  coords <- coords.all[1:n,]
  data <- data.frame(cbind(Y,X))
  colnames(data) = c("Y","X1","X2")
  mod.glm <- MASS::glm.nb(Y~-1+X1+X2,data=data)
  mod.glm.esp <- predict(mod.glm, data)
  mod.glm.s2 <- var( log(Y+1) - mod.glm.esp)
  mod.glm.phi <- 0.1*max(dist(coords))
  mod.glm.prec <- mod.glm$theta
  startinit <- c(mod.glm$coef,log(mod.glm.s2),log(mod.glm.phi),log(mod.glm.prec))
  names(startinit) <- c("X1","X2","logsigma2","logphi","logprec")
} else {
  stop("Package \"MASS\" needed to set the initial parameters")
}


## -----------------------------------------------------------------------------
result.nb <- fsglmm(Y~-1+X1+X2, kappa=2.5, inits = startinit, data = data,coords = coords, family = "negative.binomial", offset = NA,method.optim = "CG", method.integrate = "NR", control = list(maxit=1000,ndeps=rep(1e-2,5),reltol=0.01),rank = 50)
result.nb$summary

## -----------------------------------------------------------------------------

X.pred <- X.all[(n+1):(n+n.pred),]
coords.pred <- coords.all[(n+1):(n+n.pred),]
pred.sglmm(result.nb,data=X.pred,coords=coords.pred)


## -----------------------------------------------------------------------------
if(requireNamespace("ngspatial")&
   requireNamespace("mgcv")){
  n = 30
  A = ngspatial::adjacency.matrix(n)
  Q = diag(rowSums(A),n^2) - A
  x = rep(0:(n - 1) / (n - 1), times = n) 
  y = rep(0:(n - 1) / (n - 1), each = n) 
  X = cbind(x, y)                                 # Use the vertex locations as spatial covariates.
  beta = c(1, 1)                                  # These are the regression coefficients.
  P.perp = diag(1,n^2) - X%*%solve(t(X)%*%X)%*%t(X)
  eig = eigen(P.perp %*% A %*% P.perp)
  eigenvalues = eig$values
  q = 400
  M = eig$vectors[,c(1:q)]
  Q.s = t(M) %*% Q %*% M
  tau = 6
  Sigma = solve(tau*Q.s)
  set.seed(1)
  delta.s = mgcv::rmvn(1, rep(0,q), Sigma)
  lambda = exp( X%*%beta + M%*%delta.s )
  Z = c()
  for(j in 1:n^2){Z[j] = rpois(1,lambda[j])}
  Y = as.matrix(Z,ncol=1)
  data = data.frame("Y"=Y,"X"=X)
  colnames(data) = c("Y","X1","X2")
} else {
  stop("Packages \"ngspatial\" and \"mgcv\" are needed to generate a simulated data set")
}

## -----------------------------------------------------------------------------
linmod <- glm(Y~-1+X1+X2,data=data,family="poisson") # Find starting values
linmod$coefficients
starting <- c(linmod$coefficients,"logtau"=log(1/var(linmod$residuals)) )
result.pois.disc <- fsglmm.discrete(Y~-1+X1+X2, inits = starting, data=data,family="poisson",ntrial=1, method.optim="BFGS", method.integrate="NR", rank=50, A=A)
result.pois.disc$summary

