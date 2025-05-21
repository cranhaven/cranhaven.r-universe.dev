#' Spatial ebp for proportion under generalized linear mixed model
#'
#'@description This function gives the spatial ebp and the estimate of mean squared error (mse) for proportion
#'    based on a generalized linear mixed model.
#'
#' @param formula an object of class list of formula, describe the model to be fitted
#' @param vardir a vector of sampling variances of direct estimators for each small area
#' @param Ni a vector of population size for each small area
#' @param ni a vector of sample size for each small area
#' @param proxmat a D*D proximity matrix of D small areas. The matrix must be row-standardized.
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
#'     \item rho : estimated spatial correlation
#'     \item randomeffect : a data frame with the values of the area specific random effect
#'     \item variance : a covariance matrix of estimated variance components
#'     \item loglike : value of the loglikelihood
#'     \item deviance : value of the deviance
#'   }
#'  }
#' @export ebpSP
#'
#' @examples
#' # Load data set
#' data(headcount)
#' # Fit a generalized linear mixed model with SAR spcification using headcount data
#' result <- ebpSP(ps~x1, var, N, n, Wmatrix, "REML", 100, 1e-04, headcount)

#' result
ebpSP <- function (formula, vardir,Ni, ni, proxmat, method = "REML", maxit = 100, precision = 1e-04, data) {
  nameNi <- deparse(substitute(Ni))
  nameni <- deparse(substitute(ni))
  namevar <- deparse(substitute(vardir))
  nameproxmat <- deparse(substitute(proxmat))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit,data)
    X <- model.matrix(formula, data)
    Ni <- data[, nameNi]
    ni <- data[, nameni]
    vardir <- data[, namevar]
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
  if (any(is.na(proxmat)))
    stop("Argument proxmat=", nameproxmat, " contains NA values.")
  if (any(is.na(vardir)))
    stop("Argument vardir=", namevar, " contains NA values.")
  if (!is.matrix(proxmat))
  W <- W.matrix <- as.matrix(proxmat)
  y <- formuladata[, 1]
  m <- length(y)
  p <- dim(X)[2]
  dir <- y
  I<-D<-D.sp<-diag(1,m)
  sigma2u<-NULL
  sigma2u[1]<-median(vardir)
  rho<-NULL
  rho[1]<-0.5
  beta=matrix(0,p,maxit)
  beta[,1]=0
  u=matrix(0,m,maxit)
  u[,1]=0
  betaug=rbind(beta,u)
  xm<-X
  nd=ni
  xmd=rbind(t(xm),t(D))
  xmdzt=t(xmd)

  for (iiter in 1:maxit){
   Sigma.u<-sigma2u[iiter]*solve((diag(1,m)-rho[iiter]*W)%*%(diag(1,m)-rho[iiter]*t(W)))
    for (iiiter in 1:maxit){
      eta=xm%*%betaug[1:p,(iiiter)]+D%*%betaug[(p+1):(p+m),(iiiter)]
      pd=exp(eta)/(1+exp(eta))
      Sigma.e=matrix(0,m,m)
      for (i in 1:m) {
        Sigma.e[i,i]=ni[i]*pd[i]*(1-pd[i])
        }
      dim.ess<-p+m
      ess<-matrix(0,dim.ess,dim.ess)
      ess11<-t(xm)%*%Sigma.e%*%xm
      ess12<-t(xm)%*%Sigma.e%*%D
      ess21<-t(D)%*%Sigma.e%*%xm
      ess22<-t(D)%*%Sigma.e%*%D+solve(Sigma.u)
      row1<-cbind(ess11,ess12)
      row2<-cbind(ess21,ess22)
      ess<-rbind(row1,row2)
      epsilon<-xm%*%betaug[1:p,(iiiter)]+D%*%betaug[(p+1):(p+m),(iiiter)]+solve(Sigma.e)%*%(y-ni*pd)
      betaug[,(iiiter+1)]<-solve(ess)%*%xmd%*%Sigma.e%*%epsilon
      diff<-sum((betaug[,(iiiter+1)]-betaug[,(iiiter)])^2)
      done <- (diff<precision)
      if (done)
        break
    }
    if (!done)
      warning(paste("PQL failed to converge in", maxit, "steps"))
    betaug[,1]<-betaug[,(iiiter+1)]
    etastim=xm%*%betaug[1:p,1]+D%*%betaug[(p+1):(p+m),1]
    pd=exp(etastim)/(1+exp(etastim))
    Sigma.e=matrix(0,m,m)
    for (i in 1:m) {
      Sigma.e[i,i]=ni[i]*pd[i]*(1-pd[i])
      }
    Ts=solve(solve(Sigma.u)+t(D)%*%Sigma.e%*%D)
    T11=solve(t(X)%*%Sigma.e%*%X-t(X)%*%Sigma.e%*%D%*%Ts%*%D%*%Sigma.e%*%X)
    T22=Ts+Ts%*%D%*%Sigma.e%*%X%*%T11%*%t(X)%*%Sigma.e%*%D%*%Ts
    sigma2u[iiter+1]=(1/m)*(sum(diag(T22%*%solve(D)))+t(betaug[(p+1):(p+m),1])%*%solve(D)%*%betaug[(p+1):(p+m),1])
    logl=function(delta){
      rhospat<-delta[1]
      sigma.v<-sigma2u[iiter+1]
      area=m
      Z.area=diag(1,area)
      psi=Sigma.e
      Y=matrix(c(pseudoy),area,1)
      I<-diag(1,area)
      V<-psi+sigma.v*Z.area%*%(solve((I-rhospat*W)%*%(I-rhospat*t(W))))%*%t(Z.area)
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
      I<-diag(1,m)
      Z.area=diag(1,area)
      rhospat<-delta[1]
      sigma.v<-delta[2]
      V<-psi+sigma.v*Z.area%*%(solve((I-rhospat*W)%*%(I-rhospat*t(W))))%*%t(Z.area)
      Vi<-solve(V)
      Xt=t(X)
      XVi<-Xt%*%Vi
      Q<-solve(XVi%*%X)
      P<-Vi-(Vi%*%X%*%Q%*%XVi)
      b.s<-betaug[1:p,1]
      ee=eigen(V)
      -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
    }
    pseudoy<-xm%*%betaug[1:p,1]+D%*%betaug[(p+1):(p+m),1]+solve(Sigma.e)%*%(y-ni*pd)
    ottimo=optimize(logl,interval=c(-0.99,0.99),maximum=TRUE)
    rho[iiter+1]=ottimo$maximum
    variance<--solve(hessian(grr,c(rho[iiter+1],sigma2u[iiter+1])))
    diff1<-(sigma2u[iiter+1]-sigma2u[iiter])^2+(rho[iiter+1]-rho[iiter])^2
    done1 <- (diff1<precision)
    if (done1)
      break
  }
  if (!done)
    warning(paste("REML failed to converge in", maxit, "steps"))

  ee<-eigen(Sigma.u)
  loglike<-sum(c(dir*etastim-ni*log(1+exp(etastim))))-0.5*log(m)-0.5*(sum(log(ee$value))+t(betaug[(p+1):(p+m),1])%*%solve(Sigma.u)%*%betaug[(p+1):(p+m),1])
  deviance<-2*sum(dir*log(dir/(ni*pd))+(ni-dir)*log((ni-dir)/(ni-ni*pd)),na.rm=TRUE)
  beta.sp.est<-betaug[1:p,1]
  rho.sp.est<- rho[iiter+1]
  sigma2u.sp.est<-sigma2u[iiter+1]
  V<-solve(Sigma.e)+sigma2u.sp.est*I%*%(solve((I-rho.sp.est*W)%*%(I-rho.sp.est*t(W))))%*%t(I)
  Vi<-solve(V)
  Xt=t(X)
  XVi<-Xt%*%Vi
  Q<-solve(XVi%*%X)
  eta.SAR<-X%*%betaug[1:p,1]+betaug[(p+1):(p+m),1]
  theta.est.SAR<-exp(eta.SAR)/(1+exp(eta.SAR))
  SPEBP.Mean<-((1/Ni)*dir) +  (((Ni-ni)/Ni)*theta.est.SAR)
  zvalue <- beta.sp.est/sqrt(diag(Q))
  pvalue <- 2 * pnorm(abs(zvalue), lower.tail = FALSE)
  coef <- data.frame(beta = beta.sp.est, std.error = sqrt(diag(Q)),tvalue = zvalue, pvalue)
  Hr.sp=matrix(0,m,m)
  for (i in 1:m) {
      Hr.sp[i,i]=(Ni[i]-ni[i])*theta.est.SAR[i]*(1-theta.est.SAR[i])
    }
  ar<-diag(1/Ni[1:m],m)
  Zr=diag(1,m)
  Zplus=ar%*%Hr.sp%*%Zr
  Ar.sp=ar%*%Hr.sp
  B.sp=matrix(0,m,m)
  for (i in 1:m) {
    B.sp[i,i]=ni[i]* theta.est.SAR[i]*(1-theta.est.SAR[i])
  }
  W.sp=D.sp
  Sigma2.v= sigma2u.sp.est*(solve((I-rho.sp.est*W.matrix)%*%(I-rho.sp.est*t(W.matrix))))
  Ts.sp=solve((t(W.sp)%*%B.sp%*%W.sp)+solve(Sigma2.v))
  T11.sp=solve(t(X)%*%B.sp%*%X -  t(X)%*%B.sp%*%(W.sp%*%Ts.sp%*%t(W.sp))%*%B.sp%*%X)
  T22.sp=Ts.sp+Ts.sp%*%t(W.sp)%*%B.sp%*%X%*%T11.sp%*%t(X)%*%t(B.sp)%*%W.sp%*%Ts.sp
  G1.sp=diag(Ar.sp%*%W.sp%*%Ts.sp%*%t(W.sp)%*%t(Ar.sp))
  G2.sp=diag(Ar.sp%*%((X-W.sp%*%Ts.sp%*%t(W.sp)%*%B.sp%*%X)%*%T11.sp%*%t(X-W.sp%*%Ts.sp%*%t(W.sp)%*%B.sp%*%X))%*%t(Ar.sp))
  Vs.sp=solve(B.sp)+W.sp%*%as.matrix(Sigma2.v)%*%t(W.sp)
  sigmaPlus.Hat.sp=t(W.sp)%*%B.sp%*%Vs.sp%*%B.sp%*%W.sp+t(W.sp)%*%B.sp%*%W.sp
  WW<-2*rho.sp.est*(W.matrix%*%t(W.matrix))-W.matrix-t(W.matrix)
  g3spgrad.sp<-matrix(0,2,m)
  C=Zplus
  E=(1/sigma2u.sp.est^2)*(diag(1,m)-rho.sp.est*W.matrix)%*%(diag(1,m)-rho.sp.est*t(W.matrix))
  g3spgrad1<-C%*%Ts.sp%*%E%*%Ts.sp
  g3spgrad2<--(1/sigma2u.sp.est)*C%*%Ts.sp%*%WW%*%Ts.sp
  G3.sp<-NULL
  var.sp<-variance
  for (i in 1:m) {
    g3spgrad.sp[1,]<-g3spgrad1[i,]
    g3spgrad.sp[2,]<-g3spgrad2[i,]
    tmp<-g3spgrad.sp%*%sigmaPlus.Hat.sp%*%t(g3spgrad.sp)
    G3.sp[i]<-sum(diag(tmp%*%var.sp))
  }
  SPEBP.MSE=round(G1.sp+G2.sp+2*G3.sp,5)
  areacode=1:m
  EBP.SE=round(sqrt(SPEBP.MSE),5)
  EBP.CV=round(100*(sqrt(SPEBP.MSE)/SPEBP.Mean),5)
  result1= cbind(areacode,SPEBP.Mean, SPEBP.MSE,EBP.SE,EBP.CV)
  colnames(result1)=c("area","EBP","MSE","SE","CV")
  result <- list(EBP = NA, mse = NA, sample = NA,
                 fit = list(estcoef = NA, refvar = NA, rho = NA,randomeffect = NA,
                            variance = NA, loglike = NA ,deviance= NA))
  result=list()
  result$EBP=c(SPEBP.Mean)
  result$mse=SPEBP.MSE
  result$fit$estcoef <- coef
  result$fit$refvar=sigma2u.sp.est
  result$fit$rho=rho.sp.est
  result$fit$randomeffect=betaug[(p+1):(p+m),1]
  result$fit$variance=variance
  result$fit$loglike=loglike
  result$fit$deviance=deviance
  result$sample=result1
  return(result)

}
