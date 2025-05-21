#' Nonstationary ebp for proportion under generalized linear mixed model
#'
#'@description This function gives the nonstationary ebp and the estimate of mean squared error (mse) for proportion
#'    based on a generalized linear mixed model.
#'
#' @param formula an object of class list of formula, describe the model to be fitted
#' @param vardir a vector of sampling variances of direct estimators for each small area
#' @param Ni a vector of population size for each small area
#' @param ni a vector of sample size for each small area
#' @param lat a vector of latitude for each small area
#' @param lon a vector of longitude for each small area
#' @param method type of fitting method, default is "REML" method
#' @param maxit number of iterations allowed in the algorithm. Default is 100 iterations
#' @param precision convergence tolerance limit for the Fisher-scoring algorithm. Default value is 1e-04
#' @param data a data frame comprising the variables named in formula and vardir
#'
#' @return The function returns a list with the following objects:
#' \describe{
#'   \item{ebp}{a vector with the values of the estimators for each small area}
#'   \item{mse}{a vector of the mean squared error estimates for each small area}
#'   \item{sample}{a matrix consist of area code, ebp, mse, standard error (SE) and coefficient of variation (CV)}
#'   \item{fit}{a list containing the following objects:}
#'   \itemize{
#'     \item estcoef : a data frame with the estimated model coefficients in the first column (beta),
#'         their asymptotic standard errors in the second column (std.error), the t statistics in
#'         the third column (tvalue) and the p-values of the significance of each coefficient in
#'         last column (pvalue)
#'     \item refvar : estimated random effects variance
#'     \item lambda : estimated spatial intensity parameter
#'     \item randomeffect : a data frame with the values of the area specific random effect
#'     \item gamma : a data frame with the values of the spatially correlated random effect
#'     \item variance : a covariance matrix of estimated variance components
#'     \item loglike : value of the loglikelihood
#'     \item deviance : value of the deviance
#'     \item loglike1 : value of the restricted loglikelihood
#'   }
#'  }
#' @export ebpNS
#'
#' @examples
#' # Load data set
#' data(headcount)
#' # Fit a nonstationary generalized linear mixed model using headcount data
#' result <- ebpNS(y~x1, var, N, n, lat, long, "REML", 100, 1e-04, headcount)

#' result
ebpNS <- function (formula, vardir, Ni, ni, lat, lon, method = "REML", maxit = 100, precision = 1e-04, data) {
  nameNi <- deparse(substitute(Ni))
  nameni <- deparse(substitute(ni))
  namevar <- deparse(substitute(vardir))
  namelat <- deparse(substitute(lat))
  namelon <- deparse(substitute(lon))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit,data)
    X <- model.matrix(formula, data)
    Ni <- data[, nameNi]
    ni <- data[, nameni]
    vardir <- data[, namevar]
    lat <- data[, namelat]
    lon <- data[, namelon]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  if (attr(attributes(formuladata)$terms, "response") == 1)
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0)
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir)))
    stop("Argument vardir=", namevar, " contains NA values.")
  y <- formuladata[, 1]
  m <- length(y)
  p <- dim(X)[2]
  dir <- y
  I<-D<-diag(1,m)
  distance<-matrix(0,m,m)
  distance<-as.matrix(dist(cbind(as.vector(lon),as.vector(lat))))
  W<-1/(1+distance)
  Z<-matrix(0,m,1)
  for (i in 1:p)
  {Z<-cbind(Z,diag(X[,i]))}
  Z<-Z[,-1]
  lambda<-NULL
  lambda[1]<-0.05
  sigma2u<-NULL
  sigma2u[1]<-median(vardir)
  beta=matrix(0,p,maxit)
  beta[,1]=0
  u=matrix(0,m,maxit)
  u[,1]=0
  gamma=matrix(0,(m*p),maxit)
  gamma[,1]=0
  betaug=rbind(beta,u,gamma)
  xm<-X
  nd=ni
  xmdz=rbind(t(X),t(D),t(Z))
  xmdzt=t(xmdz)
  for (iiter in 1:maxit)
  {
    C<-lambda[iiter]*diag(1,p)
    Sigma.l<-kronecker(C,W)
    Sigma.u<-sigma2u[iiter]*D
      for (iiiter in 1:maxit){
        eta=X%*%betaug[1:p,(iiiter)]+D%*%betaug[(p+1):(p+m),(iiiter)]+Z%*%betaug[(p+m+1):(p+m+(m*p )),(iiiter)]
        pd=exp(eta)/(1+exp(eta))
        Sigma.e=matrix(0,m,m)
        for (i in 1:m) {Sigma.e[i,i]=ni[i]*pd[i]*(1-pd[i])}
        Omega=bdiag(list(sigma2u[iiter]*diag(1,m),lambda[iiter]*kronecker(diag(1,p),W)))
        Omega1=bdiag(list(diag(0,p),solve(Omega)))
        Vs=xmdz%*%Sigma.e%*%xmdzt+Omega1
        betaug[,(iiiter+1)]<- betaug[,(iiiter)]+as.numeric(solve(Vs)%*%xmdz%*%(y-ni*pd)-solve(Vs)%*%c(rep(0,p),as.numeric(solve(Omega)%*%betaug[(p+1):(p+m+(p*m)),(iiiter)])))
        diff<-sum((betaug[,(iiiter+1)]-betaug[,(iiiter)])^2)
        done <- (diff<precision)
        if (done)
        break
      }
    if (!done)
      warning(paste("PQL failed to converge in", maxit, "steps"))
    betaug[,1]<-betaug[,(iiiter+1)]
    etastim=X%*%betaug[1:p,1]+D%*%betaug[(p+1):(p+m),1]+Z%*%betaug[(p+m+1):(p+m+(m*p)),1]
    pd=exp(etastim)/(1+exp(etastim))
    Sigma.e=matrix(0,m,m)
    for (i in 1:m) {
      Sigma.e[i,i]=ni[i]*pd[i]*(1-pd[i])
    }
    Ts=solve(solve(Sigma.u)+t(D)%*%Sigma.e%*%D)
    T11=solve(t(X)%*%Sigma.e%*%X-t(X)%*%Sigma.e%*%D%*%Ts%*%D%*%Sigma.e%*%X)
    T22=Ts+Ts%*%D%*%Sigma.e%*%X%*%T11%*%t(X)%*%Sigma.e%*%D%*%Ts
    sigma2u[iiter+1]=(1/m)*(sum(diag(T22%*%solve(D)))+t(betaug[(p+1):(p+m),1])%*%solve(D)%*%betaug[(p+1):(p+m),1])
    logl=function(delta) {
      area=m
      psi=solve(Sigma.e)
      Y=matrix(pseudoy,area,1)
      Z.area=diag(1,area)
      lambda<-delta[1]
      C<-lambda*diag(1,p)
      Sigma.l<-kronecker(C,W)
      Cov<-(Z%*%Sigma.l%*%t(Z))+sigma2u[iiter]*Z.area%*%t(Z.area)
      V<-Cov+psi
      Vi<-solve(V)
      Xt=t(X)
      XVi<-Xt%*%Vi
      Q<-solve(XVi%*%X)
      P<-Vi-(Vi%*%X%*%Q%*%XVi)
      b.s<-betaug[1:p,1]
      ee=eigen(V)
      -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
    }
    grr=function(delta){
      area=m
      psi=solve(Sigma.e)
      Y=matrix(pseudoy,area,1)
      Z.area=diag(1,area)
      lambda<-delta[2]
      sigma.u<-delta[1]
      C<-lambda*diag(1,p)
      Sigma.l<-kronecker(C,W)
      Cov<-(Z%*%Sigma.l%*%t(Z))+sigma.u*Z.area%*%t(Z.area)
      V<-Cov+psi
      Vi<-solve(V)
      Xt=t(X)
      XVi<-Xt%*%Vi
      Q<-solve(XVi%*%X)
      P<-Vi-(Vi%*%X%*%Q%*%XVi)
      b.s<-betaug[1:p,1]
      ee=eigen(V)
      -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
    }
    pseudoy<-X%*%betaug[1:p,1]+D%*%betaug[(p+1):(p+m),1]+Z%*%betaug[(p+m+1):(p+m+(m*p)),1]+solve(Sigma.e)%*%(y-ni*pd)
    ottimo=optimize(logl,interval=c(0.01,100),maximum=TRUE)
    lambda[iiter+1]=ottimo$maximum
    variance<--solve(hessian(grr,c(sigma2u[iiter+1],lambda[iiter+1])))
    diff1<-(sigma2u[iiter+1]-sigma2u[iiter])^2+(lambda[iiter+1]-lambda[iiter])^2
    done1 <- (diff1<precision)
    if (done1)
      break
  }
  if (!done)
    warning(paste("REML failed to converge in", maxit, "steps"))
  dim.ess<-m+(p*m)
  ess<-matrix(0,dim.ess,dim.ess)
  ess11<-Sigma.u
  ess22<-Sigma.l
  ess12<-matrix(0,m,p*m)
  ess21<-matrix(0,p*m,m)
  row1<-cbind(ess11,ess12)
  row2<-cbind(ess21,ess22)
  ess<-rbind(row1,row2)
  ee<-eigen(ess)
  loglike<- sum(c(dir*etastim-ni*log(1+exp(etastim))))-0.5*log(m+p*m)-0.5*(sum(log(ee$value))+t(betaug[(p+1):(p+m+(m*p)),1])%*%solve(ess)%*%betaug[(p+1):(p+m+(m*p)),1])
  deviance<-2*sum(dir*log(dir/(ni*pd))+(ni-dir)*log((ni-dir)/(ni-ni*pd)),na.rm=TRUE)
  loglike1<-logl(lambda[iiter+1])
  beta.gwr.est<-betaug[1:p,1]
  lambda.gwr.est<-lambda[iiter+1]
  sigma2u.gwr.est<-sigma2u[iiter+1]
  C<-lambda.gwr.est*diag(1,p)
  Sigma.l<-kronecker(C,W)
  Cov<-(Z%*%Sigma.l%*%t(Z))+sigma2u.gwr.est*I%*%t(I)
  V<-Cov+solve(Sigma.e)
  Vi<-solve(V)
  Xt=t(X)
  XVi<-Xt%*%Vi
  Q<-solve(XVi%*%X)
  eta.gwr<-X%*%betaug[1:p,1]+betaug[(p+1):(p+m),1]+Z%*%betaug[(p+m+1):(p+m+(m*p)),1]
  theta.est.gwr<-exp(eta.gwr)/(1+exp(eta.gwr))
  GWREBP.Mean<-((1/Ni[1:m])*dir) +  (((Ni[1:m]-ni[1:m])/Ni[1:m])*theta.est.gwr)
  zvalue <- beta.gwr.est/sqrt(diag(Q))
  pvalue <- 2 * pnorm(abs(zvalue), lower.tail = FALSE)
  coef <- data.frame(beta = beta.gwr.est, std.error = sqrt(diag(Q)),tvalue = zvalue, pvalue)
  Hr.gwr=matrix(0,m,m)
  for (i in 1:m) {
    Hr.gwr[i,i]=(Ni[i]-ni[i])*theta.est.gwr[i]*(1-theta.est.gwr[i])
  }
  ar<-diag(1/Ni[1:m],m)
  Ar.gwr=ar%*%Hr.gwr
  B.gwr=matrix(0,m,m)
  for (i in 1:m) {
    B.gwr[i,i]=ni[i]*theta.est.gwr[i]*(1-theta.est.gwr[i])
  }
  Z.gwr=Z
  D.gwr=diag(1,m)
  W.gwr=cbind(D.gwr,Z.gwr)
  Sigma2.v=bdiag(list((sigma2u.gwr.est)*diag(1,m),kronecker(lambda.gwr.est*diag(1,p),W)))
  Ts.gwr=solve((t(W.gwr)%*%B.gwr%*%W.gwr)+solve(Sigma2.v))
  T11.gwr=solve(t(X)%*%B.gwr%*%X -  t(X)%*%B.gwr%*%(W.gwr%*%Ts.gwr%*%t(W.gwr))%*%B.gwr%*%X)
  T22.gwr=Ts.gwr+Ts.gwr%*%t(W.gwr)%*%B.gwr%*%X%*%T11.gwr%*%t(X)%*%t(B.gwr)%*%W.gwr%*%Ts.gwr
  G1.gwr=diag(Ar.gwr%*%W.gwr%*%Ts.gwr%*%t(W.gwr)%*%t(Ar.gwr))
  G2.gwr=diag(Ar.gwr%*%((X-W.gwr%*%Ts.gwr%*%t(W.gwr)%*%B.gwr%*%X)%*%T11.gwr%*%t(X-W.gwr%*%Ts.gwr%*%t(W.gwr)%*%B.gwr%*%X))%*%t(Ar.gwr))
  Vs.gwr=solve(B.gwr)+W.gwr%*%as.matrix(Sigma2.v)%*%t(W.gwr)
  sigmaPlus.Hat.gwr=t(W.gwr)%*%B.gwr%*%Vs.gwr%*%B.gwr%*%W.gwr
  Delta.Sigma2.v1=bdiag(list(diag(1,m),diag(0,m*p)))
  Delta.Sigma2.v2=bdiag(list(diag(0,m),kronecker(diag(1,p),W)))
  g3.der1=-(Ar.gwr%*%W.gwr%*%Ts.gwr)%*%(-solve(Sigma2.v)%*%Delta.Sigma2.v1%*%solve(Sigma2.v))%*%Ts.gwr
  g3.der2=-(Ar.gwr%*%W.gwr%*%Ts.gwr)%*%(-solve(Sigma2.v)%*%Delta.Sigma2.v2%*%solve(Sigma2.v))%*%Ts.gwr
  G3.gwr<-0
  g3spgrad.gwr<-matrix(0,2,(m+m*p))
  var.gwr<-variance
  for (i in 1:m) {
    g3spgrad.gwr[1,]<-g3.der1[i,]
    g3spgrad.gwr[2,]<-g3.der2[i,]
    tmp<-g3spgrad.gwr%*%sigmaPlus.Hat.gwr%*%t(g3spgrad.gwr)
    G3.gwr[i]<-sum(diag(tmp%*%var.gwr))
  }
  GWREBP.MSE=round(G1.gwr+G2.gwr+2*G3.gwr,5)
  areacode=1:m
  EBP.SE=round(sqrt(GWREBP.MSE),5)
  EBP.CV=round(100*(sqrt(GWREBP.MSE)/GWREBP.Mean),5)
  result1= cbind(areacode,GWREBP.Mean, GWREBP.MSE,EBP.SE,EBP.CV)
  colnames(result1)=c("area","EBP","MSE","SE","CV")
  result <- list(EBP = NA, mse = NA, sample = NA,
                 fit = list(estcoef = NA, refvar = NA, lambda = NA,randomeffect = NA,
                            gamma = NA, variance = NA, loglike = NA ,deviance = NA, loglike1 = NA))
  result=list()
  result$EBP=c(GWREBP.Mean)
  result$mse=GWREBP.MSE
  result$fit$estcoef <- coef
  result$fit$refvar=sigma2u.gwr.est
  result$fit$lambda=lambda.gwr.est
  result$fit$randomeffect=betaug[(p+1):(p+m),1]
  result$fit$gamma=betaug[(p+m+1):(p+m+(m*p)),1]
  result$fit$variance=variance
  result$fit$loglike=loglike
  result$fit$deviance=deviance
  result$fit$loglike1=loglike1
  result$sample=result1
  return(result)
}

