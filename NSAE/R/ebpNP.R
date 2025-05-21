#' Nonparametric ebp using spatial spline for proportion under generalized linear mixed model
#'
#'@description This function gives the nonparametric ebp and the estimate of mean squared error (mse) for proportion
#'    based on a nonstationary generalized linear mixed model.
#'
#' @param formula an object of class list of formula, describe the model to be fitted
#' @param vardir a vector of sampling variances of direct estimators for each small area
#' @param n.knot number of knot in spatial splines. Default is 25 knot
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
#'     \item lambda : estimated spatial intensity paprameter
#'     \item randomeffect : a data frame with the values of the area specific random effect
#'     \item gamma : a data frame with the values of the spatially correlated random effect
#'     \item variance : a covariance matrix of estimated variance components
#'   }
#'  }
#' @export ebpNP
#'
#' @examples
#' # Load data set
#' data(headcount)
#' # Fit a nonparametric generalized linear mixed model using headcount data
#' result <- ebpNP(y~x1, var,25, N, n,  lat, long, "REML", 100, 1e-04,headcount)

#' result
ebpNP <- function (formula, vardir, n.knot, Ni, ni, lat, lon, method = "REML", maxit = 100, precision = 1e-04, data) {
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
  default.knots.2D<-function (x1, x2, num.knots)
  {
    if (missing(num.knots))
    num.knots <- max(10, min(50, round(length(x1)/4)))
    X <- cbind(x1, x2)
    dup.inds <- (1:nrow(X))[dup.matrix(X) == T]
    if (length(dup.inds) > 0)
      X <- X[-dup.inds, ]
    knots <- clara(X, num.knots)$medoids
    return(knots)
  }
  Z.matrix<-function(lon,lat,knots){
    K<-nrow(knots)
    dist.knot<-matrix(0,K,K)
    dist.knot[lower.tri(dist.knot)]<-dist(knots)
    dist.knot<-dist.knot+t(dist.knot)
    Omega<-tps.cov(dist.knot)
    dist.lon<-outer(lon,knots[,1],"-")
    dist.lat<-outer(lat,knots[,2],"-")
    dist.x<-sqrt(dist.lon^2+dist.lat^2)
    svd.Omega<-svd(Omega)
    sqrt.Omega<-t(svd.Omega$v %*% (t(svd.Omega$u) * sqrt(svd.Omega$d)))
    Z<- t(solve(sqrt.Omega,t(tps.cov(dist.x))))
    return(Z)
  }
  knots<-default.knots.2D(lon,lat,n.knot)
  Z<-Z.matrix(lon,lat,knots)
  K<-ncol(Z)
  sigma2g<-NULL
  sigma2g[1]<-0.5
  sigma2u<-NULL
  sigma2u[1]<- median(vardir)
  beta=matrix(0,p,maxit)
  beta[,1]=0
  u=matrix(0,m,maxit)
  u[,1]=0
  gamma=matrix(0,K,maxit)
  gamma[,1]=0
  betaug=rbind(beta,u,gamma)
  xm<-X
  nd=ni
  xmdz=rbind(t(xm),t(D),t(Z))
  xmdzt=t(xmdz)
  Z.spline=Z
  for (iiter in 1:maxit){
    Sigma.g<-sigma2g[iiter]*diag(1,K)
    Sigma.u<-sigma2u[iiter]*D
    for (iiiter in 1:maxit){
      eta=xm%*%betaug[1:p,(iiiter)]+D%*%betaug[(p+1):(p+m),(iiiter)]+Z%*%betaug[(p+m+1):(p+m+K),(iiiter)]
      pd=exp(eta)/(1+exp(eta))
      Sigma.e=matrix(0,m,m)
      for (i in 1:m) {
        Sigma.e[i,i]=ni[i]*pd[i]*(1-pd[i])
        }
      dim.ess<-p+m+K
      ess<-matrix(0,dim.ess,dim.ess)
      ess11<-t(xm)%*%Sigma.e%*%xm
      ess12<-t(xm)%*%Sigma.e%*%D
      ess13<-t(xm)%*%Sigma.e%*%Z
      ess21<-t(D)%*%Sigma.e%*%xm
      ess22<-t(D)%*%Sigma.e%*%D+solve(Sigma.u)
      ess23<-t(D)%*%Sigma.e%*%Z
      ess31<-t(Z)%*%Sigma.e%*%xm
      ess32<-t(Z)%*%Sigma.e%*%D
      ess33<-t(Z)%*%Sigma.e%*%Z+solve(Sigma.g)
      row1<-cbind(ess11,ess12,ess13)
      row2<-cbind(ess21,ess22,ess23)
      row3<-cbind(ess31,ess32,ess33)
      ess<-rbind(row1,row2,row3)
      epsilon<-xm%*%betaug[1:p,(iiiter)]+D%*%betaug[(p+1):(p+m),(iiiter)]+Z%*%betaug[(p+m+1):(p+m+K),(iiiter)]+solve(Sigma.e)%*%(y-ni*pd)
      betaug[,(iiiter+1)]<-solve(ess)%*%xmdz%*%Sigma.e%*%epsilon
      diff<-sum((betaug[,(iiiter+1)]-betaug[,(iiiter)])^2)
      done <- (diff<precision)
      if (done)
        break
    }
    if (!done)
      warning(paste("PQL failed to converge in", maxit, "steps"))
    betaug[,1]<-betaug[,(iiiter+1)]
    etastim=xm%*%betaug[1:p,1]+D%*%betaug[(p+1):(p+m),1]+Z%*%betaug[(p+m+1):(p+m+K),1]
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
      area=m
      psi=Sigma.e
      Y=matrix(pseudoy,area,1)
      Z.area=diag(1,area)
      sigma.g<-delta[1]
      sigma.u<-sigma2u[iiter+1]
      V<-sigma.g*Z.spline%*%t(Z.spline)+sigma.u*Z.area%*%t(Z.area)+psi
      Vi<-solve(V)
      Xt=t(X)
      XVi<-Xt%*%Vi
      Q <-solve(XVi%*%X)
      P<-Vi-(Vi%*%X%*%Q%*%XVi)
      b.s<-betaug[1:p,1]
      ee=eigen(V)
      -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
    }
    pseudoy<-xm%*%betaug[1:p,1]+D%*%betaug[(p+1):(p+m),1]+Z%*%betaug[(p+m+1):(p+m+K),1]+solve(Sigma.e)%*%(y-ni*pd)
    ottimo=optimize(logl,interval=c(0.01,100),maximum=TRUE)
    sigma2g[iiter+1]=ottimo$maximum
    diff1<-(sigma2u[iiter+1]-sigma2u[iiter])^2+(sigma2g[iiter+1]-sigma2g[iiter])^2
    done1 <- (diff1<precision)
    if (done1)
      break
  }
  if (!done)
    warning(paste("REML failed to converge in", maxit, "steps"))
  grr=function(delta){
    area=m
    psi=solve(Sigma.e)
    Y=matrix(pseudoy,area,1)
    Z.area=diag(1,area)
    sigma.g<-delta[2]
    sigma.u<-delta[1]
    V<-sigma.g*Z.spline%*%t(Z.spline)+sigma.u*Z.area%*%t(Z.area)+psi
    Vi<-solve(V)
    Xt=t(X)
    XVi<-Xt%*%Vi
    Q <-solve(XVi%*%X)
    P<-Vi-(Vi%*%X%*%Q%*%XVi)
    b.s<-betaug[1:p,1]
    ee=eigen(V)
    -(area/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
  }
  variance<--solve(hessian(grr,c(sigma2u[iiter+1], sigma2g[iiter+1])))
  beta.est<-betaug[1:p,1]
  g.est<-sigma2g[iiter+1]
  sigma2u.est<-sigma2u[iiter+1]
  psi=solve(Sigma.e)
  Z.area=diag(1,m)
  V<-g.est*Z.spline%*%t(Z.spline)+sigma2u.est*Z.area%*%t(Z.area)+psi
  Vi<-solve(V)
  Xt=t(X)
  XVi<-Xt%*%Vi
  Q <-solve(XVi%*%X)
  eta.NP<-try(X%*%betaug[1:p,1]+betaug[(p+1):(p+m),1]+Z.spline%*%betaug[(p+m+1):(p+m+K),1],silent = TRUE)
  theta.est.NP<-try(exp(eta.NP)/(1+exp(eta.NP)),silent = TRUE)
  NPEBP.Mean<-try(((1/Ni)*dir) +  (((Ni-ni)/Ni)*theta.est.NP),silent = TRUE)
  zvalue <- beta.est/sqrt(diag(Q))
  pvalue <- 2 * pnorm(abs(zvalue), lower.tail = FALSE)
  coef <- data.frame(beta = beta.est, std.error = sqrt(diag(Q)),tvalue = zvalue, pvalue)
  Hr.np=matrix(0,m,m)
  for (i in 1:m) {
     Hr.np[i,i]=(Ni[i]-ni[i])*theta.est.NP[i]*(1-theta.est.NP[i])
  }
  ar<-diag(1/Ni[1:m],m)
  Ar.np=ar%*%Hr.np
  B.np=matrix(0,m,m)
  for (i in 1:m) {
    B.np[i,i]=ni[i]* theta.est.NP[i]*(1-theta.est.NP[i])
  }
  D.np=diag(1,m)
  W.np=cbind(D.np,Z.spline)
  Sigma2.v=bdiag(list((sigma2u[iiter+1])*diag(1,m),(sigma2g[iiter+1])*diag(1,n.knot)))
  Ts.np=solve((t(W.np)%*%B.np%*%W.np)+solve(Sigma2.v))
  T11.np=solve(t(X)%*%B.np%*%X -  t(X)%*%B.np%*%(W.np%*%Ts.np%*%t(W.np))%*%B.np%*%X)
  T22.np=Ts.np+Ts.np%*%t(W.np)%*%B.np%*%X%*%T11.np%*%t(X)%*%t(B.np)%*%W.np%*%Ts.np
  G1.np=diag(Ar.np%*%W.np%*%Ts.np%*%t(W.np)%*%t(Ar.np))
  G2.np=diag(Ar.np%*%((X-W.np%*%Ts.np%*%t(W.np)%*%B.np%*%X)%*%T11.np%*%t(X-W.np%*%Ts.np%*%t(W.np)%*%B.np%*%X))%*%t(Ar.np))
  Vs.np=solve(B.np)+W.np%*%as.matrix(Sigma2.v)%*%t(W.np)
  sigmaPlus.Hat.np=t(W.np)%*%B.np%*%Vs.np%*%B.np%*%W.np
  Delta.Sigma2.v1=bdiag(list(diag(1,m),diag(0,n.knot)))
  Delta.Sigma2.v2=bdiag(list(diag(0,m),diag(1,n.knot)))
  g3.der1=-(Ar.np%*%W.np%*%Ts.np)%*%(-solve(Sigma2.v)%*%Delta.Sigma2.v1%*%solve(Sigma2.v))%*%Ts.np
  g3.der2=-(Ar.np%*%W.np%*%Ts.np)%*%(-solve(Sigma2.v)%*%Delta.Sigma2.v2%*%solve(Sigma2.v))%*%Ts.np
  G3.np<-0
  g3spgrad.np<-matrix(0,2,(m+n.knot))
  var.np<-variance
  for (i in 1:m) {
    g3spgrad.np[1,]<-g3.der1[i,]
    g3spgrad.np[2,]<-g3.der2[i,]
    tmp<-g3spgrad.np%*%sigmaPlus.Hat.np%*%t(g3spgrad.np)
    G3.np[i]<-sum(diag(tmp%*%var.np))
  }
  NPEBP.MSE=round(G1.np+G2.np+2*G3.np,5)
  areacode=1:m
  EBP.SE=round(sqrt(NPEBP.MSE),5)
  EBP.CV=round(100*(sqrt(NPEBP.MSE)/NPEBP.Mean),5)
  result1= cbind(areacode,NPEBP.Mean, NPEBP.MSE,EBP.SE,EBP.CV)
  colnames(result1)=c("area","EBP","MSE","SE","CV")
  result <- list(EBP = NA, mse = NA, sample = NA,
                 fit = list(estcoef = NA, refvar = NA, lambda = NA,randomeffect = NA,
                            gamma = NA, variance = NA))
  result=list()
  result$EBP=NPEBP.Mean
  result$mse=NPEBP.MSE
  result$fit$estcoef <- coef
  result$refvar=sigma2u.est
  result$lambda=g.est
  result$randomeffect=betaug[(p+1):(p+m),1]
  result$gamma=betaug[(p+m+1):(p+m+K),1]
  result$variance=variance
  result$sample=result1
  return(result)
}
