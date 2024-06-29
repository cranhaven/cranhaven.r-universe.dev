
##############################################################################################
Q.d.poisson <- function(d,Xbeta,Y,det.Sigma,inv.Sigma,M){
  ##############################################################################################
  eta = as.numeric(Xbeta + M%*%d)
  dens = sum(stats::dpois(Y,exp(eta),log=TRUE)) +
    my.gauss.d(d,det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)
  return(dens)
}

##############################################################################################
Q1.d.poisson <- function(d,Xbeta,Y,det.Sigma, inv.Sigma,M){
  ##############################################################################################
  eta = as.numeric(Xbeta + M%*%d)
  grad <- t(t(M)%*%Y) - t(exp(eta))%*%M - t(d)%*%inv.Sigma
  return(grad)
}

##############################################################################################
Q2.d.poisson <- function(d, Xbeta,Y,det.Sigma,inv.Sigma,M){
  ##############################################################################################
  eta = as.numeric(Xbeta + M%*%d)
  nHess = t(M)%*%diag(exp(eta))%*%M + inv.Sigma
  Hess <- -nHess
  return(Hess)
}

##############################################################################################
my.gauss.d <- function(d, det.Sigma, inv.Sigma){
  ##############################################################################################
  n <- length(d)
  dens <- (-n/2) * log(2*pi) + (1/2)*det.Sigma - (1/2) * t(d) %*% inv.Sigma %*% d
  return(dens)
}

##############################################################################################
newton_raphson.discrete <- function(initial, score, hessian, tolerance = 0.001, max.iter=100, m.dim,...){
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
fLaplace.discrete <- function(Q.b, gr, hess, method, optimizer, m.dim, ...){
  ##############################################################################################
  log.integral <- -sqrt(.Machine$double.xmax)
  Init <- rep(0,m.dim)
  if (method=="NR"){
    temp <- try(newton_raphson.discrete(initial = Init, score=gr, hessian = hess, m.dim = m.dim, max.iter = 100,...), silent = TRUE)}
  if(class(temp)!='try-error' & method=="NR"){
    value <- Q.b(d = temp[2][[1]] , ...)
    log.integral <- value + ((m.dim/2)*log(2*pi) - 0.5*determinant(-temp[1][[1]])$modulus)}
  return(log.integral)
}

##############################################################################################
nlikSGLMM.discrete <- function(par, Y, X, nugget, family, method, ntrial=1, offset=NA,
                              M,Q,MQM,rank){
  ##############################################################################################
  I = -sqrt(.Machine$double.xmax)
  n <- length(Y)
  n.beta <- dim(X)[2]
  beta <- as.numeric(par[1:n.beta])
  Xbeta <- X%*%beta
  if(is.na(offset)[1] != TRUE){Xbeta <- cbind(X,log(offset))%*%c(beta,1)}
  tau <- as.numeric(exp(par[(n.beta+1)]))

  det.Sigma <- rank*log(tau)
  inv.Sigma <- tau * MQM
  if(family == "poisson"){
    I <- fLaplace.discrete(Q.d.poisson, gr = Q1.d.poisson, hess = Q2.d.poisson, method= method, optimizer="BFGS",
                          m.dim = rank, M = M,
                          Xbeta = Xbeta, Y = Y, det.Sigma = det.Sigma, inv.Sigma = inv.Sigma)}
  return(-I)
}


#' Fitting Projection Based Laplace Approximation for Spatial Generalized Linear Mixed Model
#' @description \code{fsglmm.discrete}
#' @param formula an object of class "formula."
#' @param inits starting values for the parameters.
#' @param data a data frame containing variables in the model.
#' @param family a character string of the error distribution and link function to be used in the model.
#' @param ntrial a numeric vector for binomial model.
#' @param offset this is used to specify an a priori a known component to be included in the linear predictor during fitting.
#' @param method.optim the method to be used for outer optimization. "CG" for Conjugate Gradient Method.
#' @param method.integrate the method to be used for inner optimization. "NR" for Newton Raphson Method.
#' @param rank an integer of 'rank' to be used for projections. Default is 5 percent of observations.
#' @param A an adjacency matrix
#'
#'
#'
#' @return a list containing the following components:
#' @return \code{summary} a summary of the fitted model
#' @return \code{mle2} an object of class "mle2"
#' @return \code{Delta} a matrix containing the estimated random effects of the reduced dimensional model.
#' @return \code{M} the projection matrix used.
#'
#'
#' @import bbmle
#' @export
#'
#' @examples
#' \donttest{
#' if(requireNamespace("ngspatial")&
#' requireNamespace("mgcv")){
#' n = 30
#' A = ngspatial::adjacency.matrix(n)
#' Q = diag(rowSums(A),n^2) - A
#' x = rep(0:(n - 1) / (n - 1), times = n)
#' y = rep(0:(n - 1) / (n - 1), each = n)
#' X = cbind(x, y)
#' beta = c(1, 1)
#' P.perp = diag(1,n^2) - X%*%solve(t(X)%*%X)%*%t(X)
#' eig = eigen(P.perp %*% A %*% P.perp)
#' eigenvalues = eig$values
#' q = 400
#' M = eig$vectors[,c(1:q)]
#' Q.s = t(M) %*% Q %*% M
#' tau = 6
#' Sigma = solve(tau*Q.s)
#' set.seed(1)
#' delta.s = mgcv::rmvn(1, rep(0,q), Sigma)
#' lambda = exp( X%*%beta + M%*%delta.s )
#' Z = c()
#' for(j in 1:n^2){Z[j] = rpois(1,lambda[j])}
#' Y = as.matrix(Z,ncol=1)
#' data = data.frame("Y"=Y,"X"=X)
#' colnames(data) = c("Y","X1","X2")
#' linmod <- glm(Y~-1+X1+X2,data=data,family="poisson") # Find starting values
#' linmod$coefficients
#' starting <- c(linmod$coefficients,"logtau"=log(1/var(linmod$residuals)) )
#'
#' result.pois.disc <- fsglmm.discrete(Y~-1+X1+X2, inits = starting, data=data,
#' family="poisson",ntrial=1, method.optim="BFGS", method.integrate="NR",rank=50, A=A)
#' }
#'
#' }
#'
fsglmm.discrete <- function(formula, inits, data, family, ntrial=1,
                          method.optim, method.integrate, rank=NULL, A, offset = NA){
  formula <- stats::as.formula(formula)
  mf <- stats::model.frame(formula, data=data)
  Y <- stats::model.response(mf)
  X <- stats::model.matrix(formula, data=data)
  names <- c(colnames(X), "logtau")
  n.beta <- dim(X)[2]
  n.obs <- dim(X)[1]
  if(is.null(rank)) rank <- floor(n.obs*0.05)
  P.perp = diag(1,n.obs) - X%*%solve(t(X)%*%X)%*%t(X)
  eig = RSpectra::eigs_sym(P.perp %*% A %*% P.perp,k=rank,which="LA")
  eigenvalues = eig$values
  M = eig$vectors[,c(1:rank)]
  Q = diag(rowSums(A),n.obs) - A
  MQM = t(M) %*% Q %*% M

  names(inits) <- parnames(nlikSGLMM.discrete) <- names
  estimate <- bbmle::mle2(nlikSGLMM.discrete, start=inits,
                   vecpar=TRUE,
                   method=method.optim,
                   control = list(maxit=1000),
                   skip.hessian = FALSE,
                   data = list(Y=Y, X=X, family= family, method = method.integrate, ntrial,
                               offset = offset, M = M, MQM = MQM, rank = rank))

  n.pars <- length(coef(estimate))
  summary.estimate <- summary(estimate)
  summary.estimate@coef[,1] <- summary.estimate@coef[,1]
  output.se <- suppressWarnings(sqrt(diag(estimate@vcov)))
  if(sum(is.na(output.se)) != 0 ) {
    summary.estimate@coef[,2] <- NA
  }
  summary.estimate@coef[,3] <- NA
  summary.estimate@coef[,4] <- NA
  output <- list()
  output[1][[1]] <- summary.estimate
  output[2][[1]] <- estimate
  coef_final <- summary.estimate@coef[,1]
  beta <- as.numeric(coef_final[1:n.beta])
  tau <- as.numeric(exp(coef_final[n.beta+1]))
  det.Sigma <- rank*log(tau)
  inv.Sigma <- tau * MQM
  if(family == "poisson"){
    Xbeta = X%*%beta[1:n.beta]
    if(is.na(offset)[1] != TRUE) { Xbeta <- cbind(X,log(offset))%*%c(beta,1)}
    Init <- rep(0,rank)
    gr = Q1.d.poisson
    hess = Q2.d.poisson

    DELTA.HAT <- newton_raphson.discrete(initial = Init, score = gr, hessian = hess,
                                         tolerance = 0.001, max.iter = 100,
                                         m.dim=rank,
                                         Y=Y, Xbeta = Xbeta, det.Sigma=det.Sigma,inv.Sigma=inv.Sigma,M=M)[2][[1]]
  }
  output[3][[1]] <- as.matrix(DELTA.HAT)
  output[4][[1]] <- M
  names(output) <- c("summary","mle2","Delta","M")
  return(output)
}
