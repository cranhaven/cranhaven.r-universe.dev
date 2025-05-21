#' EBP for proportion under generalized linear mixed model
#'
#'@description This function gives the ebp and the estimate of mean squared error (mse) for proportion
#'    based on a generalized linear mixed model.
#'
#' @param formula an object of class list of formula, describe the model to be fitted
#' @param vardir a vector of sampling variances of direct estimators for each small area
#' @param Ni a vector of population size for each small area
#' @param ni a vector of sample size for each small area
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
#'     \item randomeffect : a data frame with the values of the random effect estimators
#'     \item loglike : value of the loglikelihood
#'     \item deviance : value of the deviance
#'     \item loglike1 : value of the restricted loglikelihood
#'   }
#'  }
#' @export ebp
#'
#' @examples
#' # Load data set
#' data(headcount)
#' # Fit generalized linear mixed model using HCR data
#' result <- ebp(y~x1, var, N, n,"REML",100,1e-04, headcount)

#' result
ebp <- function (formula, vardir, Ni, ni, method = "REML", maxit = 100, precision = 1e-04, data) {
  nameNi <- deparse(substitute(Ni))
  nameni <- deparse(substitute(ni))
  namevar <- deparse(substitute(vardir))
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
  if (any(is.na(vardir)))
    stop("Argument vardir=", namevar, " contains NA values.")
  y <- formuladata[, 1]
  m <- length(y)
  p <- dim(X)[2]
  dir <- y
  I<-D<-diag(1,m)
  sigma2u<-NULL
  sigma2u[1]<-median(vardir)
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
    Sigma.u<-sigma2u[iiter]*D
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
    if(method=="REML")sigma2u[iiter+1]=(1/m)*(sum(diag(T22%*%solve(D)))+t(betaug[(p+1):(p+m),1])%*%solve(D)%*%betaug[(p+1):(p+m),1])
    diff1<-(sigma2u[iiter+1]-sigma2u[iiter])^2
    done1 <- (diff1<precision)
    if (done1)
      break
  }
  if (!done)
    warning(paste("REML failed to converge in", maxit, "steps"))
  logl=function(delta){
    area=m
    psi=solve(Sigma.e)
    Y=matrix(epsilon,area,1)
    Z.area=diag(1,area)
    sigma2u<-delta[1]
    Cov<-sigma2u*Z.area%*%t(Z.area)
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
  ee<-eigen(Sigma.u)
  loglike<-sum(c(dir*etastim-ni*log(1+exp(etastim))))-0.5*log(m)-0.5*(sum(log(ee$value))+t(betaug[(p+1):(p+m),1])%*%solve(Sigma.u)%*%betaug[(p+1):(p+m),1])
  deviance<-2*sum(dir*log(dir/(ni*pd))+(ni-dir)*log((ni-dir)/(ni-ni*pd)),na.rm=TRUE)
  loglike1<-logl(sigma2u[iiter+1])
  beta.est<-try(betaug[1:p,1],silent = TRUE)
  sigma2u.est<-try(sigma2u[iiter+1],silent = TRUE)
  Cov<-sigma2u.est*I%*%t(I)
  V<-Cov+solve(Sigma.e)
  Vi<-solve(V)
  Xt=t(X)
  XVi<-Xt%*%Vi
  Q<-solve(XVi%*%X)
  eta.est<-try(X%*%betaug[1:p,1]+betaug[(p+1):(p+m),1],silent = TRUE)
  theta.est<-try(exp(eta.est)/(1+exp(eta.est)),silent = TRUE)
  EBP.Mean<-((1/Ni)*dir )  +  (((Ni-ni)/Ni)*theta.est)
  zvalue <- beta.est/sqrt(diag(Q))
  pvalue <- 2 * pnorm(abs(zvalue), lower.tail = FALSE)
  coef <- data.frame(beta = beta.est, std.error = sqrt(diag(Q)),tvalue = zvalue, pvalue)
  ar<-diag(1/Ni[1:m],m)
  Hr=matrix(0,m,m)
  for (i in 1:m) {
    Hr[i,i]=(Ni[i]-ni[i])*theta.est[i]*(1-theta.est[i])
    }
  z=diag(1,m)
  Xs=Xr=X
  Zr=diag(1,m)
  Zplus=ar%*%Hr%*%Zr
  Xplus=ar%*%Hr%*%Xr
  B=matrix(0,m,m)
  for (i in 1:m) {
    B[i,i]=ni[i]*theta.est[i]*(1-theta.est[i])
  }
  Ts=solve(solve( sigma2u.est*diag(1,m))+t(z)%*%B%*%z)
  T11=solve(t(Xs)%*%B%*%Xs-t(Xs)%*%B%*%z%*%Ts%*%t(z)%*%B%*%Xs)
  T22=Ts+Ts%*%t(z)%*%B%*%Xs%*%T11%*%t(Xs)%*%t(B)%*%z%*%Ts
  G1=diag(Zplus%*%Ts%*%t(Zplus))
  G2=diag((Xplus-Zplus%*%Ts%*%t(z)%*%B%*%Xs)%*%T11%*%t(Xplus-Zplus%*%Ts%*%t(z)%*%B%*%Xs))
  G3<-0
  g3spgrad<-matrix(0,1,m)
  sigmaPlus.Hat=t(z)%*%B%*%z+ sigma2u.est*t(z)%*%B%*%z%*%t(z)%*%B%*%z
  g3spgrad1<-(1/ sigma2u.est^2)*Zplus%*%Ts%*%Ts
  r1=( sigma2u.est^(-1))*sum(diag(solve(z)%*%Ts))
  r11=sum(diag(Ts%*%solve(z)%*%Ts%*%solve(z)))
  varpsi=2*(( sigma2u.est^(-2))*(m-2*r1)+( sigma2u.est^(-4))*r11)^(-1)
  for (i in 1:m) {
    g3spgrad[1,]<-g3spgrad1[i,]
    G3[i]<-sum(diag(g3spgrad%*%sigmaPlus.Hat%*%t(g3spgrad)*varpsi))
  }
  EBP.MSE=round(G1+G2+2*G3,5) 	# Estimate of mse for the proprtions
  areacode=1:m
  EBP.SE=round(sqrt(EBP.MSE),5)
  EBP.CV=round(100*(sqrt(EBP.MSE)/EBP.Mean),5)
  result1= cbind(areacode,EBP.Mean, EBP.MSE,EBP.SE,EBP.CV)
  colnames(result1)=c("area","EBP","MSE","SE","CV")
  result <- list(ebp = NA, mse = NA, sample = NA,
                 fit = list(estcoef = NA, refvar = NA, randomeffect = NA,
                            loglike = NA ,deviance= NA, loglike1 = NA))
  result=list()
  result$ebp=EBP.Mean
  result$mse=EBP.MSE
  result$fit$estcoef <- coef
  result$fit$refvar=sigma2u.est
  result$fit$randomeffect=betaug[(p+1):(p+m),1]
  result$fit$loglike=loglike
  result$fit$deviance=deviance
  result$fit$loglike1=loglike1
  result$sample=result1
  return(result)

}
