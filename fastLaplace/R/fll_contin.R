##############################################################################################
inv.logit <- function(x) {
  ##############################################################################################
  res = 1/(1+exp(-x))
  return(res)
}

##############################################################################################
logit <- function(x) {
  ##############################################################################################
  res = log(x) - log(1-x)
  return(res)
}

##############################################################################################
matern.cov <- function(phi,kappa,mat.dist) {
  ##############################################################################################
  if(phi < 1e-8 ) phi = 1e-8
  if(kappa == 1/2) {
    K = exp(-1/phi*mat.dist)
  }
  if(kappa == 1.5) {
    K = (1+sqrt(3)/phi*mat.dist)*exp(-sqrt(3)/phi*mat.dist)
  }
  if(kappa == 2.5){
    K = (1+sqrt(5)/phi*mat.dist+ 5/(3*phi^2)*mat.dist^2)*exp(-sqrt(5)/phi*mat.dist)
  }
  if(kappa == 10){
    K = exp(-1/(2*phi^2)*mat.dist^2)
  }
  return(K)
}

##############################################################################################
rp_fl <- function(n=1000,k=100,C,alpha=1,m=50){
  ##############################################################################################
  PHI = C^alpha %*% matrix(stats::rnorm(n*k,0,sd=k^-0.25),ncol=k)
  K11 = t(PHI)%*%C%*%PHI
  SVDK11 = svd(K11)
  V11 = SVDK11$u
  L11 = SVDK11$d
  C = C%*%PHI%*%V11%*%diag(L11^-0.5)
  C.svd = svd(C)
  u_m = C.svd$u[,1:m]
  d_m = C.svd$d[1:m]
  return(list("u" = u_m, "d" = as.matrix(d_m,ncol=1)))
}

##############################################################################################
sigma.rp <- function(phi,kappa,mat.dist,U1,rank){
  ##############################################################################################
  covmat_original <- matern.cov(phi=phi,kappa=kappa,mat.dist=mat.dist)
  covmat_rp <- rp_fl(n=dim(mat.dist)[1],k=2*rank,alpha=1,C=covmat_original,m=rank)
  m <- rank
  um <- covmat_rp$u[,1:m]
  signdiag = sign(diag(t(um)%*%U1))
  signdiag = as.logical(1-signdiag)
  um[,signdiag] = -um[,signdiag]
  dm <- diag(covmat_rp$d[1:m,]^2)
  return(list(um=um ,dm = dm))
}
##############################################################################################
sigma.spectra <- function(phi,kappa,mat.dist,U1,rank){
  ##############################################################################################
  covmat_original <- matern.cov(phi=phi,kappa=kappa,mat.dist=mat.dist)
  covmat_svd <- RSpectra::eigs_sym(covmat_original,k=rank,which="LA")
  m <- rank
  um <- covmat_svd$vectors[,1:m]
  signdiag = sign(diag(t(um)%*%U1))
  signdiag = as.logical(1-signdiag)
  um[,signdiag] = -um[,signdiag]
  dm <- diag(covmat_svd$values[1:m])
  return(list(um=um ,dm = dm))
}

##############################################################################################
Q.b.poisson <- function(b,Xbeta,Y, det.Sigma, inv.Sigma,um,dm){
  ##############################################################################################
  eta = Xbeta + um%*%dm^0.5%*%b
  dens = sum(stats::dpois(Y,lambda= exp(eta), log=TRUE)) +
    my.gauss(b,det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)
  return(dens)
}

##############################################################################################
Q1.b.poisson <- function(b,Xbeta,Y,det.Sigma, inv.Sigma,um,dm){
  ##############################################################################################
  grad <- t(dm^0.5%*%t(um)%*%Y) - t(exp(Xbeta + um%*%dm^0.5%*%b))%*%um%*%dm^0.5 - t(b)*inv.Sigma
  return(grad)
}

##############################################################################################
Q2.b.poisson <- function(b, Xbeta,Y,det.Sigma,inv.Sigma,um,dm){
  ##############################################################################################
  m = dim(dm)[1]
  eta = exp(Xbeta + um%*%dm^0.5%*%b)
  nHess = dm^0.5%*%t(um)%*%diag(as.numeric(eta))%*%um%*%dm^0.5 + diag(inv.Sigma,m)
  Hess <- -nHess
  return(Hess)
}

##############################################################################################
Q.b.binomial <- function(b,Xbeta,Y,det.Sigma,inv.Sigma,ntrial=1,um,dm){
  ##############################################################################################
  eta = Xbeta + um%*%dm^0.5%*%b
  p <- inv.logit(eta)
  dens = sum(stats::dbinom(Y,size = ntrial, prob = p,log=TRUE)) + my.gauss(b,det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)
  return(dens)
}

##############################################################################################
Q1.b.binomial <- function(b,Xbeta,Y,det.Sigma, inv.Sigma,ntrial=1,um,dm){
  ##############################################################################################
  eta = Xbeta + um%*%dm^0.5%*%b
  b1 <- ntrial*inv.logit(eta)
  grad <- t(dm^0.5%*%t(um)%*%Y) - t(b1)%*%um%*%dm^0.5 - t(b)*inv.Sigma
  return(grad)
}

##############################################################################################
Q2.b.binomial <- function(b, Xbeta,Y,det.Sigma,inv.Sigma,ntrial=1,um,dm){
  ##############################################################################################
  m = dim(dm)[1]
  eta = as.numeric(Xbeta + um%*%dm^0.5%*%b)
  nHess = ntrial*dm^0.5%*%t(um)%*%diag( exp(eta)/(1+exp(eta))^2 )%*%um%*%dm^0.5 + diag(inv.Sigma,m)
  Hess <- -nHess
  return(Hess)
}

##############################################################################################
Q.b.negbin <- function(b,Xbeta,Y,prec,det.Sigma,inv.Sigma,um,dm){
  ##############################################################################################
  eta = Xbeta + um%*%dm^0.5%*%b
  dens = sum(stats::dnbinom(Y,size = prec, mu = exp(eta),log=TRUE)) + my.gauss(b,det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)
  return(dens)
}

##############################################################################################
Q1.b.negbin <- function(b,Xbeta,Y,prec,det.Sigma,inv.Sigma,um,dm){
  ##############################################################################################
  et1 = exp(Xbeta)
  et2 = exp(um%*%dm^0.5%*%b)
  et12 = et1*et2
  grad <- t(dm^0.5%*%t(um)%*%Y) - t((prec+Y)*(et12*(et12+prec)^-1))%*%um%*%dm^0.5 - t(b)*inv.Sigma
  return(grad)
}

##############################################################################################
Q2.b.negbin <- function(b, Xbeta,Y,prec,det.Sigma,inv.Sigma,um,dm){
  ##############################################################################################
  m = dim(dm)[1]
  et1 = exp(Xbeta)
  et2 = exp(um%*%dm^0.5%*%b)
  et12 = et1*et2
  p1 <- et12*((et12+prec)^-1)
  p2 <- p1^2
  D <- p1-p2
  nHess = dm^0.5%*%t(um)%*%diag(as.numeric((prec+Y)*D))%*%um%*%dm^0.5 + diag(inv.Sigma,m)
  Hess <- -nHess
  return(Hess)
}

##############################################################################################
my.gauss <- function(b, det.Sigma, inv.Sigma){
  ##############################################################################################
  n <- length(b)
  dens <- (-n/2) * log(2*pi) -(1/2)*det.Sigma - (1/2)*inv.Sigma * t(b)  %*% b
  return(dens)
}

##############################################################################################
newton_raphson <- function(initial, score, hessian, tolerance = 0.001, max.iter, m.dim,...){
  ##############################################################################################
  solution <- matrix(NA, max.iter, m.dim)
  solution[1,] <- initial
  for(i in 2:max.iter){
    HSS <- hessian(initial, ...)
    SCR <- t(score(initial, ...))
    solution[i,] <- initial - solve(HSS, SCR)
    initial <- solution[i,]
    convergence <- abs(solution[i,] - solution[i-1,])
    if(all(convergence < tolerance)==TRUE) break
  }
  output <-  list()
  output[1][[1]] <- HSS
  output[2][[1]] <- initial
  return(output)
}



##############################################################################################
fLaplace <- function(Q.b, gr, hess, method,m.dim=50, ...){
  ##############################################################################################
  log.integral <- -sqrt(.Machine$double.xmax)
  Init <- rep(0,m.dim)
  temp <- try(newton_raphson(initial = Init, score=gr, hessian = hess,m.dim = m.dim, max.iter = 100,...), silent = TRUE)
  if(class(temp)!='try-error'){
    value <- Q.b(b = temp[2][[1]] , ...)
    log.integral <- value + ((m.dim/2)*log(2*pi) - 0.5*determinant(-temp[1][[1]])$modulus)}
  return(log.integral)
}

##############################################################################################
nlikSGLMM <- function(par, Y, X, kappa, mat.dist, family, method,
                     ntrial=1, offset=NA,U1,rank){
  ##############################################################################################
  I = -sqrt(.Machine$double.xmax)
  n <- length(Y)
  n.beta <- dim(X)[2]
  beta <- as.numeric(par[1:n.beta])
  Xbeta <- X%*%beta
  if(is.na(offset)[1] != TRUE) { Xbeta <- cbind(X,log(offset))%*%c(beta,1)}
  sigma <- exp(as.numeric(par[c(n.beta+1)]))
  if(sigma < 1e-8) sigma <- 1e-8
  phi <- exp(as.numeric(par[c(n.beta+2)]))
  if(family == "negative.binomial") { prec <- exp(as.numeric(par[c(n.beta+3)]))}
  Sigma <- sigma.rp(phi=phi,kappa=kappa,mat.dist=mat.dist,U1=U1,rank=rank)

  um <- Sigma$um
  dm <- Sigma$dm
  m <- rank
  det.Sigma <- m * log(sigma)
  inv.Sigma <- 1 / sigma

  if(family == "poisson"){
    I <- fLaplace(Q.b.poisson, gr = Q1.b.poisson, hess = Q2.b.poisson, method = "NR",
                 m.dim = m,um = um,dm = dm,
                 Xbeta = Xbeta, Y = Y, det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)}
  if(family == "binomial"){
    I <- fLaplace(Q.b.binomial, gr = Q1.b.binomial, hess = Q2.b.binomial,method =  "NR",
                 m.dim = m, um = um, dm = dm,
                 Xbeta = Xbeta, Y = Y, det.Sigma = det.Sigma, inv.Sigma = inv.Sigma,
                 ntrial = ntrial)}
  if(family == "negative.binomial"){
    I <- fLaplace(Q.b.negbin, gr = Q1.b.negbin, hess = Q2.b.negbin, method =  "NR",
                 m.dim = m, um = um, dm = dm,
                 Xbeta = Xbeta, prec = prec,Y = Y, det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)}
  return(-I)
}


##############################################################################################
nlikSGLMM_pca <- function(par,rank,family,method,kappa,X,Y,mat.dist,ntrial,U1,offset){
  ##############################################################################################
  I = -sqrt(.Machine$double.xmax)
  n <- length(Y)
  n.beta <- dim(X)[2]
  beta <- as.numeric(par[1:n.beta])
  Xbeta <- X%*%beta
  if(is.na(offset)[1] != TRUE) { Xbeta <- cbind(X,log(offset))%*%c(beta,1)}
  sigma <- as.numeric(exp(par[c(n.beta+1)]))
  phi <- as.numeric(exp(par[c(n.beta+2)]))
  if(family == "negative.binomial") { prec <- exp(as.numeric(par[c(n.beta+3)]))}
  Sigma <- sigma.spectra(phi = phi,kappa = kappa,mat.dist = mat.dist,U1=U1,rank=rank)
  um <- Sigma$um
  dm <- Sigma$dm
  m <- rank
  det.Sigma <- m * log(sigma)
  inv.Sigma <- 1 / sigma
  if(family == "poisson"){
    I <- fLaplace(Q.b.poisson, gr = Q1.b.poisson, hess = Q2.b.poisson, method = method,
                 m.dim = m,um = um,dm = dm,
                 Xbeta = Xbeta, Y = Y, det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)}
  if(family == "binomial"){
    I <- fLaplace(Q.b.binomial, gr = Q1.b.binomial, hess = Q2.b.binomial,method = method,
                 m.dim = m, um = um, dm = dm,
                 Xbeta = Xbeta, Y = Y, det.Sigma = det.Sigma, inv.Sigma = inv.Sigma,
                 ntrial= ntrial)}
  if(family == "negative.binomial"){
    I <- fLaplace(Q.b.negbin, gr = Q1.b.negbin, hess = Q2.b.negbin, method = method,
                 m.dim = m, um = um, dm = dm,
                 Xbeta = Xbeta, prec = prec,Y = Y, det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)}
  return(-I)
}




#' @title Fitting Projection Based Laplace Approximation for Spatial Generalized Linear Mixed Model
#'
#' @description \code{fsglmm} is used to fit reduced-dimensional spatial generalized linear mixed models for continuous spatial domain.
#'
#' @param formula an object of class "formula."
#' @param kappa the smoothness parameter for the matern process (either 0.5, 1.5, 2.5 or 10).
#' @param inits starting values for the parameters.
#' @param data a data frame containing variables in the model.
#' @param coords a matrix of dimension \eqn{N} x 2 representing the longitude and latitude of each observation.
#' @param family a character string of the error distribution and link function to be used in the model.
#' @param ntrial a numeric vector for a binomial model.
#' @param offset this is used to specify an a priori a known component to be included in the linear predictor during fitting.
#' @param method.optim the method to be used for outer optimization. "CG" for Conjugate Gradient Method.
#' @param method.integrate the method to be used for inner optimization. "NR" for Newton Raphson Method.
#' @param control a list of control parameters.
#' @param rank an integer of 'rank' to be used for projections. Default is 5 percent of observations.
#'
#'
#' @return a list containing the following components:
#' @return \code{summary} a summary of the fitted model
#' @return \code{vcov} a symmetric matrix giving an estimate of the Hessian at the solution found.
#' @return \code{mle2} an object of class "mle2"
#' @return \code{family} the family used.
#' @return \code{kappa} the matern smoothness parameter used.
#' @return \code{Delta} a matrix containing the estimated random effects of the reduced dimensional model.
#' @return \code{U} a matrix whose columns contain the estimated eigenvectors of the reduced dimensional model.
#' @return \code{D} a matrix whose diagonal components contain the estimated eigenvalues of the reduced dimensional model.
#' @return \code{coords} the matrix of coordinates used.
#'
#' @import bbmle
#' @importFrom fields rdist
#' @export
#'
#' @references Jaewoo Park and Sangwan Lee - "A Projection-based Laplace Approximation for Spatial Latent Variable Models"
#'
#' @examples
#' \donttest{
#' if(requireNamespace("mgcv")){
#' sigma2 = 1
#' phi = 0.2
#' beta.true = c(1,1)
#' n = 400
#' n.pred = 100
#' coords.all <- matrix(runif((n+n.pred)*2),ncol=2,nrow=n+n.pred)
#' X.all <- matrix(runif((n+n.pred)*2),ncol=2,nrow=(n+n.pred))
#' dist.all <- fields::rdist(coords.all,coords.all)
#' V.all <- sigma2*(1+sqrt(5)/phi*dist.all+5/(3*phi^2)*dist.all^2)*exp(-sqrt(5)/phi*dist.all)
#' set.seed(1)
#' r.e.all <- mgcv::rmvn(1,rep(0,nrow(coords.all)),V.all)
#' pi.all <- X.all%*%beta.true + r.e.all
#' p.all <- exp(pi.all)/(1+exp(pi.all))
#' Y.all <- sapply(p.all, function(x) sample(0:1, 1, prob = c(1-x, x)))
#' Y <- as.matrix(Y.all[1:n],nrow = n)
#' X <- X.all[1:n,]
#' coords <- coords.all[1:n,]
#' data <- data.frame(cbind(Y,X))
#' colnames(data) = c("Y","X1","X2")
#' mod.glm <- glm(Y~-1+X1+X2,family="binomial",data=data)
#' mod.glm.esp <- predict(mod.glm,data, type="response")
#' mod.glm.s2 <- var(Y - mod.glm.esp)
#' mod.glm.phi <- 0.1*max(dist(coords))
#' startinit <- c(mod.glm$coef,log(mod.glm.s2),log(mod.glm.phi))
#' names(startinit) <- c("X1","X2","logsigma2","logphi")
#' result.bin <- fsglmm(Y~-1+X1+X2, kappa=2.5, inits = startinit,
#'  data = data,coords = coords, family = "binomial", ntrial = 1,
#'  offset = NA,method.optim = "CG", method.integrate = "NR",rank = 50)
#' }
#' }
fsglmm <- function(formula, kappa ,inits, data ,coords, family, ntrial=1,offset=NA,
                   method.optim, method.integrate,rank=NULL, control = list()){
  formula <- stats::as.formula(formula)
  model.formula <- stats::model.frame(formula, data=data)
  Y <- stats::model.response(model.formula)
  X <- stats::model.matrix(formula, data=data)
  mat.dist <- as.matrix(fields::rdist(coords,coords))
  names.par <- c(colnames(X), "logsigma2", "logphi")
  n.beta <- dim(X)[2]
  n <- dim(X)[1]

  if(is.null(rank)) rank <- floor(n*0.05)

  sign.cov <- as.matrix(matern.cov(phi=exp(inits['logphi']),kappa=kappa,mat.dist=mat.dist))
  sign.temp <- rp_fl(n = n,k=2*rank,alpha=1,C = sign.cov,m=rank)
  U1 <- sign.temp$u[,1:rank]

  if(family == "negative.binomial"){names.par <- c(names.par,"logprec")}
  if(length(control)==0) control = list(maxit=1000,ndeps=rep(1e-3,length(names.par)),reltol=1e-3)

  names(inits) <- parnames(nlikSGLMM) <- names.par

  estimate <- bbmle::mle2(nlikSGLMM, start=inits,
                   vecpar=TRUE,
                   method=method.optim,
                   control = control,
                   skip.hessian = TRUE,
                   data = list(Y=Y, X=X, mat.dist=mat.dist, ntrial=ntrial,
                               family= family,method = method.integrate,kappa = kappa, offset = offset,U1 = U1,
                               rank = rank))
  coef_final <- estimate@coef
  output.hessian.pca <- stats::optimHess(coef_final,fn=nlikSGLMM_pca,rank=rank,kappa=kappa,family=family,
                                  method=method.integrate,X=X,Y=Y,mat.dist=mat.dist,ntrial=ntrial,U1=U1,
                                  offset = offset)
  vcov.mat <- try(solve(output.hessian.pca),silent = T)
  if(class(vcov.mat)[1] != "try-error"){
    output.se <- suppressWarnings(sqrt(diag(solve(output.hessian.pca))))
    if(sum(is.na(output.se)) != 0 ) {
      output.se <- NA
    }
  } else{
    vcov.mat <- matrix(NA,ncol=length(names.par),nrow=length(names.par))
  }
  n.pars <- length(coef(estimate))
  summary.estimate <- summary(estimate)
  summary.estimate@coef[,1] <- summary.estimate@coef[,1]
  summary.estimate@coef[,2] <- output.se
  summary.estimate@coef[,3] <- NA
  summary.estimate@coef[,4] <- NA
  output <- list()
  output[1][[1]] <- summary.estimate
  output[2][[1]] <- vcov.mat
  output[3][[1]] <- estimate

  sigma <- as.numeric(exp(coef_final["logsigma2"]))
  phi <- as.numeric(exp(coef_final["logphi"]))
  Sigma <- sigma.rp(phi=phi,kappa=kappa,mat.dist=mat.dist,U1=U1,rank=rank)
  UM <- Sigma$um
  DM <- Sigma$dm
  m <- rank
  det.Sigma <- m * log(sigma)
  inv.Sigma <- 1 / sigma
  if(family == "poisson"){
    Xbeta <- X%*%coef_final[1:n.beta]
    gr <- Q1.b.poisson
    hess <- Q2.b.poisson
    DELTA.HAT <- newton_raphson(rep(0,m),score = gr,hessian = hess,m.dim=m,max.iter = 100,
                               Xbeta=Xbeta, Y=Y, det.Sigma=det.Sigma, inv.Sigma=inv.Sigma,
                               um=UM, dm=DM)[2][[1]]}
  if(family == "binomial"){
    Xbeta = X%*%coef_final[1:n.beta]
    gr <- Q1.b.binomial
    hess <- Q2.b.binomial
    DELTA.HAT <- newton_raphson(rep(0,m),score = gr,hessian = hess,m.dim=m,max.iter = 100,
                               Xbeta=Xbeta, Y=Y, det.Sigma=det.Sigma, inv.Sigma=inv.Sigma,
                               ntrial=ntrial,um=UM, dm=DM)[2][[1]]}
  if(family == "negative.binomial"){
    prec <- exp(coef_final["logprec"])
    Xbeta <- X%*%coef_final[1:n.beta]
    gr <- Q1.b.negbin
    hess <- Q2.b.negbin
    DELTA.HAT <- newton_raphson(rep(0,m),score = gr,hessian = hess,m.dim=m,max.iter = 100,
                               Xbeta=Xbeta, Y=Y, prec = prec,det.Sigma=det.Sigma, inv.Sigma=inv.Sigma,
                               um=UM, dm=DM)[2][[1]]}
  output[4][[1]] <- family
  output[5][[1]] <- kappa
  output[6][[1]] <- as.matrix(DELTA.HAT)
  output[7][[1]] <- UM
  output[8][[1]] <- DM
  output[9][[1]] <- coords
  names(output) <- c("summary","vcov","mle2","family","kappa","Delta","U","D","coords")
  return(output)
}

