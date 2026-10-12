# library(copula)
# library(msm)
# library(copBasic)
# Method

#0.
#' @export
print.CopC <-function(x,...)
{
  cat(paste(x$cop," is used.\n",sep = ""))
  cat("CopC:  ")
  cat(x$para)
  cat("\n")
  # cat("\nRunning time:\n")
  # print(x$time)
}
#' @export
print.CS_Comp <-function(x,...)
{
  cat("Selection Completed:\n")
  print(x$Selected)
  for(i in 1:length(x$paras)){
    cat(paste("\n",class(x$estCop[[i]])[1],"(",x$paras[i],"). L2:",x$L2distance[i],sep = ""))
  }
}
#' @export
print.CS_Fail <-function(x,...)
{
  cat("Selection Failed due to lack of non-censored data:")
  for(i in 1:length(x$paras)){
    cat(paste("\n",class(x$estCop[[i]])[1],"(",x$paras[i],"). Likelihood:",x$likelihoods[i],sep = ""))
  }
}
CopCens <- function(delta,jumps)
{
  # delta = (Y<=d), different from wrapper
  output <- NULL
  nseg <- sum(jumps)
  jumpPoints <- c(which(jumps),length(jumps)+1)
  for (i in 1:nseg) {
    delta.seg <- delta[jumpPoints[i]:(jumpPoints[i+1]-1)]
    info <- NULL

    tt = rle(as.vector(delta.seg))
    tt = cbind(tt$lengths, tt$values)
    idx = c(1, cumsum(tt[,1])+1)
    start = idx[-length(idx)]
    length = tt[,1]
    status = tt[,2]
    end = start + length-1

    start <- start + jumpPoints[i]-1
    end <- end + jumpPoints[i]-1
    if(1==status[length(status)]) status[length(status)]<-2

    info=cbind(start, end, length, status)

    output <- rbind(output,info)
  }
  return(output)
}
CopEmp <- function(Yc,delta){
  N <- length(Yc)
  uu <- ecdf(Yc)(Yc)*N/(N+1)
  lag0 <- uu[-N]
  lag1 <- uu[-1]
  UV=as.data.frame(cbind(lag0,lag1))
  lag0d <- delta[-N]
  lag1d <- delta[-1]
  # idx <- !(lag0d | lag1d)
  idx = lag0d & lag1d
  if(0==sum(idx)) return(list(u=NULL,v=NULL,UV=UV,nEmp=0))
  u = lag0[idx]
  v = lag1[idx]
  ret <- list(u=u,v=v,UV=UV,nEmp = sum(idx))
  return(ret)
}



#1.
# mypCopula<- function(U,theta,coptype){
#   UseMethod("mypCopula",coptype)
# }
mypCopula.normalCopula =function(U,theta,...)
{
  pCopula(as.matrix(U),copula = normalCopula(theta))
}
mypCopula.claytonCopula =function(U,theta,...)
{
  U = as.matrix(U)
  (pmax(rowSums(U^(-theta))-1,0))^(-1/theta)
}
mypCopula.gumbelCopula = function(U,a,...)
{
  U <- as.matrix(U)
  U = pmax(U,0.000001)
  U = pmin(U,0.999999)
  exp(-(rowSums((-log(U))^a))^(1/a))
}
mypCopula.frankCopula = function(U,theta,...)
{
  U <- as.matrix(U)
  a = theta
  U <- as.matrix(U)
  U = pmax(U,0.000001)
  U = pmin(U,0.999999)
  -1/a*log((1-exp(-a)-(1-exp(-a*U[,1]))*(1-exp(-a*U[,2])))/(1-exp(-a)))
}
mypCopula.joeCopula = function(U,a,...)
{
  U <- as.matrix(U)
  U = pmax(U,0.000001)
  U = pmin(U,0.999999)
  A=(1-U[,1])^a
  B=(1-U[,2])^a
  1-(A+B-A*B)^(1/a)
}
#1.
# mydCopula <- function(u,v,theta){
#   UseMethod("mydCopula",theta)
# }

mydCopula.claytonCopula = function(u,v,theta)
{
  (1+theta)*(u*v)^(-1-theta)*(pmax(0.000001,u^(-theta) + v^(-theta)-1))^(-1/theta-2)
}
mydCopula.gumbelCopula = function(u,v,a){
  U <- as.matrix(cbind(u,v))
  U = pmax(U,0.000001)
  U = pmin(U,0.999999)
  # A = mypCopula.gumbelCopula(U,a) * (U[,1]*U[,2])^(-1) * ( rowSums((-log(U))^a) )^(-2+2/a) * (log(U[,1])*log(U[,2]))^(a-1)
  # B = 1+(a-1)*(rowSums((-log(U))^a))^(-1/a)
  # return(A*B)
  dC = dCopula(U,gumbelCopula(a))
  dC[which(is.na(dC))] <- 1e-6
  return(dC)
}
mydCopula.frankCopula = function(u,v,theta){
  a = theta
  U <- as.matrix(cbind(u,v))
  U = pmax(U,0.000001)
  U = pmin(U,0.999999)
  A = a*(1-exp(-a))*(exp(-a*(rowSums(U))))
  # B = (1-exp(-a)-(1-exp(-a*U[,1]))*(1-exp(-a*U[,2])))^2
  B = (1-exp(-a)-(1-pmax(1e-6,exp(-a*U[,1])))*(1-pmax(1e-6,exp(-a*U[,2]))))^2
  return(A/B)
}
mydCopula.joeCopula = function(u,v,a){
  U <- as.matrix(cbind(u,v))
  U = pmax(U,0.000001)
  U = pmin(U,0.999999)
  u = U[,1]
  v = U[,2]
  dC <- ((1-u)^a+(1-v)^a-(1-u)^a*(1-v)^a)^(1/a-2)
  dC <- dC * (1-u)^(a-1) * (1-v)^(a-1)
  dC <- dC * (a - 1 + (1-u)^a+(1-v)^a-(1-u)^a*(1-v)^a)
  # dC = dCopula(U,joeCopula(a))
  # dC[which(is.na(dC))] <- 1e-100
  return(dC)
}





mycCopula.claytonCopula =function(v, u, theta)
{
  u^(-theta-1)*(pmax(u^(-theta) + v^(-theta)-1,0.000001))^(-1-1/theta)
}
mycCopula.gumbelCopula =function(v,u, a)
{
  U = cbind(v,u)
  U <- as.matrix(U)
  U[,2] = pmax(U[,2],0.000001)
  U[,2] = pmin(U[,2],0.999999)

  A = -mypCopula.gumbelCopula(U,a)
  B = 1/a * (rowSums((-log(U))^a))^(1/a-1)
  C = a * (-log(U[,2]))^(a-1)
  pc = A*B*C*(-1/U[,2])
  if(any(is.na(pc))) pc[which(is.na(pc))]=ifelse(v>=u,1,0)
  pc = pmin(1-1e-6,pmax(1e-6,pc))
  return(pc)
}
mycCopula.frankCopula =function(v,u, theta)
{
  a = theta
  if(a==0) return(v)
  # pc = exp(-a*u)*((1-exp(-a))/(1-exp(-a*v))-(1-exp(-a*u)))^(-1)
  pc = exp(-a*u)*(1-exp(-a*v))/(exp(-a*u)+exp(-a*v)-exp(-a)-exp(-a*(u+v)))
  if(any(is.na(pc))) pc[which(is.na(pc))]=ifelse(v>=u,1,0)
  pc = pmin(1-1e-6,pmax(1e-6,pc))
  return(pc)
}
mycCopula.joeCopula =function(v, u, a)
{
  u = pmax(u,0.000001)
  v = pmax(v,0.000001)
  u = pmin(u,0.999999)
  v = pmin(v,0.999999)
  A = (1-u)^(a-1)
  B = 1-(1-v)^a
  C = ((1-u)^a+(1-v)^a-(1-u)^a*(1-v)^a)^(1/a-1)
  ret <- pmin(A*B*C,0.999999)
  ret <- pmax(0.000001,ret)
  return(ret)
}

#2. myrCopula
myrCopula.claytonCopula = function(t,u,theta)
{
  v = ((t^(-theta/(theta+1))-1)*u^(-theta)+1)^(-1/theta)
  return(v)
}
myrCopula.gumbelCopula = function(t,u,a)
{
  len = length(u)
  v = u
  eps = rep(1,len)
  nloop = 0
  while(sum(eps>0.000001) & (sum(eps>0.000001)>len/100 | max(eps) > 0.001) & nloop<10){
    f = mycCopula.gumbelCopula(v,u,a)-t
    f1 = mydCopula.gumbelCopula(u,v,a)
    eps = abs(f)
    v = v - f/f1
    v=pmax(0,v) # to solve rcopula.gum.new(0.000001,0.000001,1.5) = -2.35329e-05
    v=pmin(1,v) # to solve rcopula.gum.new(1-0.000001,1-0.000001,1.5) = 1.00002
    nloop <- nloop+1
  }
  return(v)
}
myrCopula.frankCopula = function(t,u,theta)
{
  a = theta
  A = (1-exp(-a))/((1/t-1)*exp(-a*u)+1)
  A = pmin(1-1e-6,A)
  v = -1/a*log(1-A)
  return(v)
}
myrCopula.joeCopula = function(t,u,a){
  len = length(u)
  v = u
  eps = rep(1,len)
  nloop = 0
  while(sum(eps>0.000001) & (sum(eps>0.000001)>len/100 | max(eps) > 0.001) & nloop<10){
    f = mycCopula.joeCopula(v,u,a)-t
    f1 = mydCopula.joeCopula(u,v,a)
    eps = abs(f)
    v = v - f/f1
    v[v<0] <- -v[v<0]
    eps[v<0] <- 1
    v[v>1] <- 2-v[v>1]
    eps[v>1] <- 1
    nloop <- nloop+1
  }
  return(v)
}


#2.genLatentDat
#' Generation of data from the copula-based Markov model of order one
#'
#' Generate the latent response variable from the assumed copula-based
#' Markov model in Li, Tang and Wang (2018).
#' @importFrom  copula normalCopula
#' @importFrom  copula claytonCopula
#' @importFrom  copula gumbelCopula
#' @importFrom  copula joeCopula
#' @importFrom  copula frankCopula
#' @importFrom  stats runif
#' @importFrom  stats pnorm
#' @importFrom  stats qnorm
#' @importFrom  stats rnorm
#' @importFrom  stats qt
#' @usage genLatentY(cop="Gaussian",theta,N,MARGIN.inv=qnorm,...)
#' @param cop the choice of copula function.
#' There are currently five available copula funcitons,
#' including Clayton copula, Gaussian copula, Gumbel copula, Joe copula and Frank copula.
#' Specify one from "Clayton","Gaussian","Gumbel","Joe" and "Frank".
#' The default is "Gaussian".
#' @param theta the copula parameter.
#' @param N the length of the latent response.
#' @param MARGIN.inv the inverse marginal distribution function of the latent time series.
#' The default is \code{qnorm(p,mean=0,sd=1)}, i.e., the standard normal marginal.
#' @param ... additional parameters for the inverse marginal distribution funcion of the latent time series.
#' @return \code{genLatentY} returns a Nx1 vector of the latent response variable Y*
#' @references Li, F., Tang, Y. and Wang, H. (2018) Copula-based Semiparametric Analysis for Time Series Data with Detection Limits, technical report.
#' @export
genLatentY = function(cop="Gaussian",theta,N,MARGIN.inv=qnorm,...)
{
  copula <-
    switch(cop,
           "Clayton" = claytonCopula(),
           "Gaussian" = normalCopula(),
           "Gumbel" = gumbelCopula(),
           "Joe" = joeCopula(),
           "Frank" = frankCopula())
  uu <- genLatentU(copula = copula,theta = theta,N = N)
  Y <- MARGIN.inv(uu,...)
  return(Y)
}
genLatentU = function(copula,theta,N)
{
  UseMethod("genLatentU",copula)
}
genLatentU.normalCopula <- function(copula,theta,N){
  alpha = theta
  mu = 0
  Y <- rep(0,N)
  Y[1] <- rnorm(1,mu,1)
  for(i in 2:N){
    e <- rnorm(1,0,sqrt(1-alpha^2))
    p = pnorm(Y[i-1],mu,1)
    q = qnorm(p,0,1)
    p = pnorm(alpha * q + e , 0,1)
    q = qnorm(p,mu,1)
    Y[i] = q
  }
  # return(Y)
  return(pnorm(Y))
}
genLatentU.claytonCopula <- function(copula,theta,N){
  Y <- NULL
  v <- runif(1)
  for(i in 1:N){
    # Y <- c(Y, qt(v,df))
    Y <- c(Y,v)
    u = max(v, 0.000001)
    t = runif(1)
    v = myrCopula.claytonCopula(t,u,theta)
  }
  return(Y)
}
genLatentU.gumbelCopula <- function(copula,theta,N){
  a = theta
  vv <- NULL
  v <- runif(1)
  t = runif(N)
  for(i in 1:N){
    u = v
    v = myrCopula.gumbelCopula(t[i],u,a)
    vv <- c(vv,v)
  }
  vv <- pmax(vv,0.000001)
  vv <- pmin(vv,0.999999)
  # Y = qt(vv,df)
  return(vv)
}
genLatentU.joeCopula <- function(copula,theta,N){
  a = theta
  Y <- NULL
  v <- runif(1)
  for(i in 1:N){
    # Y <- c(Y,qt(v,df))
    Y = c(Y,v)
    u = v
    t = runif(1)
    v = myrCopula.joeCopula(t,u,a)
  }
  # Ym <- qt(Y,df)
  return(Y)
}
genLatentU.frankCopula <- function(copula,theta,N){
  a = theta
  Y <- NULL
  v <- runif(1)
  for(i in 1:N){
    # Y <- c(Y, qt(v,df))
    Y <- c(Y, v)
    u = max(v, 0.000001)
    t = runif(1)
    v = myrCopula.frankCopula(t,u,a)
  }
  return(Y)
}


rtcCopula.claytonCopula = function(u_vec, theta, Pi)
{
  n = length(u_vec)
  w = runif(n, 0, 1)
  w2 = w*mycCopula.claytonCopula(Pi, u_vec, theta)
  v = ((w2^(-theta/(theta+1))-1)*u_vec^(-theta)+1)^(-1/theta)
  return(v)
}
rtcCopula.gumbelCopula = function(u_vec, a, Pi)
{
  n=length(u_vec)
  u = u_vec
  w = runif(n, 0, 1)
  t = w*mycCopula.gumbelCopula(rep(Pi,n),u_vec,a)
  len = n
  v = u
  eps = rep(1,n)
  nloop = 0
  while(sum(eps>0.000001) & (sum(eps>0.000001)>len/100 | max(eps) > 0.001) & nloop<10){
    f = mycCopula.gumbelCopula(v,u,a)-t
    # f1 = mydCopula.gumbelCopula(u,v,a)
    f1 = dCopula(cbind(u,v),gumbelCopula(a))
    eps = abs(f)

    v = v - f/f1
    v=pmax(0,v) # to solve rcopula.gum.new(0.000001,0.000001,1.5) = -2.35329e-05
    v=pmin(1,v) # to solve rcopula.gum.new(1-0.000001,1-0.000001,1.5) = 1.00002
    nloop <- nloop+1
  }
  return(v)
}
rtcCopula.frankCopula = function(u_vec, theta, Pi)
{
  n = length(u_vec)
  a = theta
  w = runif(n, 0, 1)
  w2 = w* mycCopula.frankCopula(Pi, u_vec, theta)
  A = (1-exp(-a))/((1/w2-1)*exp(-a*u_vec)+1)
  A = pmin(1-1e-6,A)
  v = -1/a*log(1-A)
  return(v)
}
rtcCopula.joeCopula = function(u_vec, a, Pi)
{
  n = length(u_vec)
  u = u_vec
  w = runif(n, 0, 1)
  t = w* (1-u)^(a-1) *(1-(1-Pi)^a) * (((1-u)^a+(1-Pi)^a-(1-u)^a*(1-Pi)^a)^(1/a-1))
  v <- NULL
  len = n
  v = u
  eps = rep(1,len)
  nloop = 0
  while(sum(eps>0.000001) & (sum(eps>0.000001)>len/100 | max(eps) > 0.001) & nloop<10){
    f = mycCopula.joeCopula(v,u,a)-t
    f1 = mydCopula.joeCopula(u,v,a)
    # f1 = dCopula(cbind(u,v),joeCopula(a))
    eps = abs(f)
    v = v - f/f1
    v[v<0] <- -v[v<0]
    eps[v<0] <- 1
    v[v>1] <- 2-v[v>1]
    eps[v>1] <- 1
    # v=pmin(1,v)
    # v=pmax(0,v)
    nloop <- nloop+1
  }
  return(v)
}


#4.

CopInt.normalCopula <- function(start,upper,theta,nIS=2000,end=NA){
  rho = theta
  cop <- normalCopula(rho,dim = 2)
  Sigma = sqrt(1-rho^2)
  Upper = qnorm(upper)
  f = q = rep(1,nIS)
  m = length(upper)
  y2 = rep(qnorm(start),nIS)
  for(i in 1:m){
    y1 = y2
    mu = rho*y1
    y2 = rtnorm(nIS,mean = mu,sd = Sigma,lower = -Inf,upper = Upper[i])
    f <- f*dCopula(cbind(pnorm(y1),pnorm(y2)),cop)*dnorm(y2)
    q <- q*dtnorm(x = y2,mean = mu,sd = Sigma,lower = -Inf,upper = Upper[i],log = F)
  }
  if(!is.na(end)){
    f <- f*dCopula(cbind(pnorm(y2),end),cop)
  }
  mean(f/q)
}
CopInt.claytonCopula <- function(start,upper,theta,nIS=2000,end=NA)
{
  v2 = rep(start,nIS)
  h = rep(1,nIS)
  m = length(upper)
  for(i in 1:m)
  {
    v1 = v2
    v2=rtcCopula.claytonCopula(v1, theta, upper[i])
    h = h*mycCopula.claytonCopula(upper[i],v1,theta)
  }
  if(!is.na(end)){
    h = h*mydCopula.claytonCopula(v2,end,theta)
  }
  mean(h)
}
CopInt.gumbelCopula = function(start,upper,theta,nIS=2000,end=NA)
{
  m = length(upper)
  a = theta
  v2 = rep(start,nIS)
  h = rep(1,nIS)
  for(i in 1:m)
  {
    Pi_vec = rep(upper[i],nIS)
    v1 = v2
    v2=rtcCopula.gumbelCopula(v1,a,upper[i])
    v2 = pmax(v2,0.000001)
    h = h*mycCopula.gumbelCopula(Pi_vec,v1,a)
  }
  if(!is.na(end)){
    h = h*mydCopula.gumbelCopula(v2,rep(end,nIS),a)
  }
  mean(h)
}
CopInt.frankCopula = function(start,upper,theta,nIS=2000,end=NA)
{
  m = length(upper)
  v2 = rep(start,nIS)
  h = rep(1,nIS)
  for(i in 1:m)
  {
    v1 = v2
    v2=rtcCopula.frankCopula(v1, theta, upper[i])
    h = h*mycCopula.frankCopula(upper[i],v1,theta)
  }
  if(!is.na(end)){
    h = h* mydCopula.frankCopula(v2,end,theta)
  }
  mean(h)
}
CopInt.joeCopula = function(start,upper,theta,nIS=2000,end=NA)
{
  m = length(upper)
  a = theta
  v2 = rep(start,nIS)
  h = rep(1,nIS)
  for(i in 1:m)
  {
    v1 = v2
    v2=rtcCopula.joeCopula(v1,a,upper[i])
    v2 = pmax(v2,0.000001)
    h = h*mycCopula.joeCopula(upper[i],v1,a)
  }
  if(!is.na(end)){
    h = h*mydCopula.joeCopula(v2,end,a)
  }
  mean(h)
}

#5.





#6.
CopLikelihood <- function(copula,theta,Yc,d,delta,nIS=2000,jumps=NULL,MARGIN=NULL,...)
{
  # delta = (Y<=d) is different from wrapper
  UseMethod("CopLikelihood",copula)
}

CopLikelihood.normalCopula <- function(copula,theta,Yc,d,delta,nIS=2000,jumps=NULL,MARGIN=NULL,...)
{
  # if(!is.na(copula@parameters)) theta=copula@parameters
  copula@parameters=theta
  cop <- copula
  N = length(Yc)
  if(is.null(jumps)){jumps <- c(TRUE,rep(FALSE,N-1))}
  Info <- CopCens(delta,jumps)
  nUC = sum(Info[,4]==0)
  nCen = sum(Info[,4]==1 | Info[,4]==2)
  Info.UC = Info[Info[,4]==0,]
  Info.C = Info[Info[,4]!=0,]
  if(nUC==1) Info.UC=matrix(Info.UC, ncol=4)
  if(nCen==1) Info.C=matrix(Info.C, ncol=4)
  if(is.null(MARGIN)) MARGIN=function(x){ecdf(Yc)(x)*N/(N+1)}
  FF <- MARGIN(Yc,...)
  Pi = MARGIN(d,...)
  logL1 = 0
  for(j in 1:nUC){
    idx = Info.UC[j,1]:Info.UC[j,2]
    if(length(idx)>1){
      for(k in 2:length(idx)){
        logL1 <- logL1 + log(dCopula(cbind(FF[idx[k-1]],FF[idx[k]]),cop))
      }
    }
  }
  logL2 = 0
  if(nCen >= 1){
    for(j in 1:nCen){
      idx = Info.C[j,1]:Info.C[j,2]
      nidx = length(idx)
      if(1==Info.C[j,4]){
        a = FF[idx[1]-1];b = FF[idx[nidx]+1]
        INT = CopInt.normalCopula(start=a,upper=rep(Pi,nidx),theta,nIS,end=b)
      }
      else if(2==Info.C[j,4]){
        a = FF[idx[1]-1]
        INT = CopInt.normalCopula(start=a,upper=rep(Pi,nidx),theta,nIS)
      }
      logL2 <- logL2 + log(INT)
    }
  }
  logL = logL1+logL2
  return(-logL)
}
CopLikelihood.claytonCopula <- function(copula,theta,Yc,d,delta,nIS=2000,jumps=NULL,MARGIN=NULL,...)
{
  # if(!is.na(copula@parameters)) theta=copula@parameters
  N = length(Yc)
  if(is.null(jumps)){jumps <- c(TRUE,rep(FALSE,N-1))}
  Info <- CopCens(delta,jumps)
  nUC = sum(Info[,4]==0)
  nCen = sum(Info[,4]==1 | Info[,4]==2)
  Info.UC = Info[Info[,4]==0,]
  Info.C = Info[Info[,4]!=0,]
  if(nUC==1) Info.UC=matrix(Info.UC, ncol=4)
  if(nCen==1) Info.C=matrix(Info.C, ncol=4)
  if(is.null(MARGIN)) MARGIN=function(x){ecdf(Yc)(x)*N/(N+1)}
  FF <- MARGIN(Yc,...)
  Pi = MARGIN(d,...)
  logL1 = 0
  for(j in 1:nUC){
    idx = Info.UC[j,1]:Info.UC[j,2]
    if(length(idx)>1){
      for(k in 2:length(idx)){
        logL1 <- logL1 + log(mydCopula.claytonCopula(FF[idx[k-1]],FF[idx[k]],theta))
      }
    }
  }
  logL2 = 0
  if(nCen >= 1){
    for(j in 1:nCen){
      idx = Info.C[j,1]:Info.C[j,2]
      nidx = length(idx)
      if(1==Info.C[j,4]){
        a = FF[idx[1]-1];b = FF[idx[nidx]+1]
        INT = max(0.00001, CopInt.claytonCopula(start=a,upper=rep(Pi,nidx),theta,nIS,end=b))
      }
      else if(2==Info.C[j,4]){
        a = FF[idx[1]-1]
        INT = max(0.00001, CopInt.claytonCopula(start=a,upper=rep(Pi,nidx),theta,nIS))
      }
      logL2 <- logL2 + log(INT)
    }
  }
  logL = logL1+logL2
  return(-logL)
}
CopLikelihood.gumbelCopula <- function(copula,theta,Yc,d,delta,nIS=2000,jumps=NULL,MARGIN=NULL,...)
{
  # if(!is.na(copula@parameters)) theta=copula@parameters
  N = length(Yc)
  if(is.null(jumps)){jumps <- c(TRUE,rep(FALSE,N-1))}
  Info <- CopCens(delta,jumps)
  nUC = sum(Info[,4]==0)
  nCen = sum(Info[,4]==1 | Info[,4]==2)
  Info.UC = Info[Info[,4]==0,]
  Info.C = Info[Info[,4]!=0,]
  if(nUC==1) Info.UC=matrix(Info.UC, ncol=4)
  if(nCen==1) Info.C=matrix(Info.C, ncol=4)
  if(is.null(MARGIN)) MARGIN=function(x){ecdf(Yc)(x)*N/(N+1)}
  FF <- MARGIN(Yc,...)
  Pi = MARGIN(d,...)
  logL1 = 0
  for(j in 1:nUC){
    idx = Info.UC[j,1]:Info.UC[j,2]
    if(length(idx)>1){
      for(k in 2:length(idx)){
        logL1 <- logL1 + log(mydCopula.gumbelCopula(FF[idx[k-1]],FF[idx[k]],theta))
      }
    }
  }
  logL2 = 0
  if(nCen >= 1){
    for(j in 1:nCen){
      idx = Info.C[j,1]:Info.C[j,2]
      nidx = length(idx)
      if(1==Info.C[j,4]){
        a = FF[idx[1]-1];b = FF[idx[nidx]+1]
        INT = max(0.00001, CopInt.gumbelCopula(start=a,upper=rep(Pi,nidx),theta,nIS,end=b))
      }
      else if(2==Info.C[j,4]){
        a = FF[idx[1]-1]
        INT = max(0.00001, CopInt.gumbelCopula(start=a,upper=rep(Pi,nidx),theta,nIS))
      }
      logL2 <- logL2 + log(INT)
    }
  }
  logL = logL1+logL2
  return(as.numeric(-logL))
}
CopLikelihood.frankCopula <- function(copula,theta,Yc,d,delta,nIS=2000,jumps=NULL,MARGIN=NULL,...)
{
  # if(!is.na(copula@parameters)) theta=copula@parameters
  N = length(Yc)
  if(is.null(jumps)){jumps <- c(TRUE,rep(FALSE,N-1))}
  Info <- CopCens(delta,jumps)
  nUC = sum(Info[,4]==0)
  nCen = sum(Info[,4]==1 | Info[,4]==2)
  Info.UC = Info[Info[,4]==0,]
  Info.C = Info[Info[,4]!=0,]
  if(nUC==1) Info.UC=matrix(Info.UC, ncol=4)
  if(nCen==1) Info.C=matrix(Info.C, ncol=4)
  if(is.null(MARGIN)) MARGIN=function(x){ecdf(Yc)(x)*N/(N+1)}
  FF <- MARGIN(Yc,...)
  Pi = MARGIN(d,...)
  logL1 = 0
  for(j in 1:nUC){
    idx = Info.UC[j,1]:Info.UC[j,2]
    if(length(idx)>1){
      for(k in 2:length(idx)){
        logL1 <- logL1 + log(mydCopula.frankCopula(FF[idx[k-1]],FF[idx[k]],theta))
        # logL1 <- logL1 + log(pmax(1e-6,mydCopula.frankCopula(FF[idx[k-1]],FF[idx[k]],theta)))
      }
    }
  }
  logL2 = 0
  if(nCen >= 1){
    for(j in 1:nCen){
      idx = Info.C[j,1]:Info.C[j,2]
      nidx = length(idx)
      if(1==Info.C[j,4]){
        a = FF[idx[1]-1];b = FF[idx[nidx]+1]
        # INT = CopInt.frankCopula(start=a,upper=rep(Pi,nidx),theta,nIS,end=b)
        INT = max(0.00001, CopInt.frankCopula(start=a,upper=rep(Pi,nidx),theta,nIS,end=b))
      }
      else if(2==Info.C[j,4]){
        a = FF[idx[1]-1]
        # INT = CopInt.frankCopula(start=a,upper=rep(Pi,nidx),theta,nIS)
        INT = max(0.00001, CopInt.frankCopula(start=a,upper=rep(Pi,nidx),theta,nIS))
      }
      logL2 <- logL2 + log(INT)
    }
  }
  logL = logL1+logL2
  return(as.numeric(-logL))
}
CopLikelihood.joeCopula <- function(copula,theta,Yc,d,delta,nIS=2000,jumps=NULL,MARGIN=NULL,...)
{
  # if(!is.na(copula@parameters)) theta=copula@parameters
  N = length(Yc)
  if(is.null(jumps)){jumps <- c(TRUE,rep(FALSE,N-1))}
  Info <- CopCens(delta,jumps)
  nUC = sum(Info[,4]==0)
  nCen = sum(Info[,4]==1 | Info[,4]==2)
  Info.UC = Info[Info[,4]==0,]
  Info.C = Info[Info[,4]!=0,]
  if(nUC==1) Info.UC=matrix(Info.UC, ncol=4)
  if(nCen==1) Info.C=matrix(Info.C, ncol=4)
  if(is.null(MARGIN)) MARGIN=function(x){ecdf(Yc)(x)*N/(N+1)}
  FF <- MARGIN(Yc,...)
  Pi = MARGIN(d,...)
  logL1 = 0
  for(j in 1:nUC){
    idx = Info.UC[j,1]:Info.UC[j,2]
    if(length(idx)>1){
      for(k in 2:length(idx)){
        # logL1 <- logL1 + log(mydCopula.joeCopula(FF[idx[k-1]],FF[idx[k]],theta))
        logL1 <- logL1 + log(pmax(1e-6,mydCopula.joeCopula(FF[idx[k-1]],FF[idx[k]],theta)))
      }
    }
  }
  logL2 = 0
  if(nCen >= 1){
    for(j in 1:nCen){
      idx = Info.C[j,1]:Info.C[j,2]
      nidx = length(idx)
      if(1==Info.C[j,4]){
        a = FF[idx[1]-1];b = FF[idx[nidx]+1]
        INT = max(0.00001, CopInt.joeCopula(start=a,upper=rep(Pi,nidx),theta,nIS,end=b))
      }
      else if(2==Info.C[j,4]){
        a = FF[idx[1]-1]
        INT = max(0.00001, CopInt.joeCopula(start=a,upper=rep(Pi,nidx),theta,nIS))
      }
      logL2 <- logL2 + log(INT)
    }
  }
  logL = logL1+logL2
  return(as.numeric(-logL))
}


#7.
#' Pseudo maximum likelihood estimator of the copula parameter
#'
#' Obtains the pseudo maximum likelihood estimator of the copula parameter based on censored time series.
#' @importFrom  copula normalCopula
#' @importFrom  copula claytonCopula
#' @importFrom  copula gumbelCopula
#' @importFrom  copula joeCopula
#' @importFrom  copula frankCopula
#' @importFrom  copula dCopula
#' @importFrom  stats optimize
#' @importFrom  stats ecdf
#' @importFrom  stats pnorm
#' @importFrom  stats qnorm
#' @importFrom  stats dnorm
#' @importFrom  stats rnorm
#' @importFrom  msm dtnorm
#' @importFrom  msm rtnorm
#' @usage
#' estCopC(cop="Gaussian",Yc,d,delta,nIS=500,jumps=NULL,MARGIN=NULL,...,interval=c(-1,1))
#' @param cop the choice of copula function.
#' There are currently five available copula funcitons,
#' including Clayton copula, Gaussian copula, Gumbel copula, Joe copula and Frank copula.
#' Specify one from "Clayton","Gaussian","Gumbel","Joe" and "Frank".
#' The default is "Gaussian".
#' @param Yc the Nx1 vector of observed response variable
#' that is subject to lower detection limit.
#' @param d the lower detection limit.
#' @param delta the Nx1 vector of censoring indicator
#' with 1 indicating uncensored and 0 indicating left censored.
#' @param nIS the size for sequential importance sampling.
#' The default is 500.
#' @param jumps the Nx1 vector indicating whether
#' each time t is a start of a new time series, which is deemed to be
#' independent from the previous series.
#' By default, \code{jumps} = c(1,rep(0,n-1)) indicating the data is one Markov sequence.
#' @param MARGIN the marginal distribution function of the latent time series.
#' The default is the empirical cdf:
#' \deqn{\frac{1}{n+1}\sum_{t=1}^n I_{Y_t<=y}}.
#'  MARGIN can also be specified as
#'  other existing distribution functions such as pnorm.
#' @param ... additional parameters for the marginal distribution of the latent time series.
#' @param interval the lower and upper bound for the copula paraameter.
#' By default,  \code{interval}=
#' c(-1,1) for Gaussian copula,
#' c(-1,Inf) for Clayton copula,
#' c(1,Inf) for Gumbel and Joe copula and
#' c(-Inf,Inf) for Frank copula.
#' @return \code{estCopC} returns a list of components including.
#' \item{para}{the pseudo maximum likelihood estimator of the copula parameter.}
#' \item{likelihood}{the negative log-likelihood value corresponding to the estimated copula parameter.}
#' \item{copula}{the estimated copula object, with estimated copula parameter plugged in.}
#' @examples
#' ### Using a simulated data for demonstration:
#' set.seed(20)
#' Y <- genLatentY(cop="Clayton",1,500,MARGIN.inv = qt,df=3)
#' d <- -1
#' Yc <- pmax(d,Y)
#' delta <- (Y>d)
#' estCopC(cop = "Clayton",Yc,d,delta,nIS = 1000,interval = c(1,10)) ## CopC estimator
#' estCopC(cop = "Clayton",Y,d,delta=rep(TRUE,length(Y)),interval = c(1,10)) ## Omniscient estimator
#' estCopC(cop = "Clayton",Yc,d,delta,nIS = 1000,MARGIN=pt,df=3,interval = c(1,10)) ## CopC estimator under true marginal
#' ### Analyze the water quality data:
#' attach(water)
#' set.seed(1)
#' estCopC(cop="Joe",Yc=TNH3,d=0.02,delta=Delta,jumps=Indep,interval = c(1,10),nIS=500)
#' @references Li, F., Tang, Y. and Wang, H. (2018).
#' Copula-based Semiparametric Analysis for Time Series Data with Detection Limits, technical report.
#' @export
estCopC <- function(cop="Gaussian",Yc,d,delta,nIS=500,jumps=NULL,MARGIN=NULL,...,interval=NULL)
{
  copula <-
    switch(cop,
           "Clayton" = claytonCopula(),
           "Gaussian" = normalCopula(),
           "Gumbel" = gumbelCopula(),
           "Joe" = joeCopula(),
           "Frank" = frankCopula())
  if(is.null(interval)){
    interval <-
      switch(cop,
             "Clayton" = c(-1,Inf),
             "Gaussian" = c(-1,1),
             "Gumbel" = c(1,Inf),
             "Joe" = c(1,Inf),
             "Frank" = c(-Inf,Inf))
  }
  if(is.null(MARGIN)) MARGIN=function(x){ecdf(Yc)(x)*length(Yc)/(length(Yc)+1)}
  return(getCopC(copula,interval,Yc,d,!delta,nIS=nIS,jumps=jumps,MARGIN=MARGIN,...))
}

getCopC <- function(copula,interval,Yc,d,delta,nIS,jumps,MARGIN,...)
{
  # delta = (Y<=d) is different from wrapper
  UseMethod("getCopC", copula)
}

getCopC.normalCopula <- function(copula,interval,Yc,d,delta,nIS,jumps,MARGIN,...)
{
  t0 <- proc.time()
  interval[1] <- pmax(interval[1],-1)
  interval[2] <- pmin(interval[2],1)
  opt <- optimize(f = CopLikelihood.normalCopula,interval = interval,copula=copula,Yc=Yc,d=d,delta=delta,nIS=nIS,jumps=jumps,MARGIN=MARGIN,...)
  t1 <- proc.time()
  theta <- opt$minimum
  ll <- opt$objective
  tt <-t1-t0
  est = list(para = theta,
             likelihood = ll,
             copula = normalCopula(theta),
             cop = "Gaussian Copula",
             time = tt)
  class(est) <- "CopC"
  return(est)
}
getCopC.claytonCopula <- function(copula,interval,Yc,d,delta,nIS,jumps,MARGIN,...)
{
  t0 <- proc.time()
  interval[1] <- ifelse(interval[1]>=200,-1,interval[1])
  interval[2] <- ifelse(interval[2]<=-1,200,interval[2])
  interval[1] <- pmax(interval[1],-1)
  interval[2] <- pmin(interval[2],200)
  opt <- optimize(f = CopLikelihood.claytonCopula,interval = interval,copula=copula,Yc=Yc,d=d,delta=delta,nIS=nIS,jumps=jumps,MARGIN=MARGIN,...)
  t1 <- proc.time()
  theta <- opt$minimum
  ll <- opt$objective
  tt <-t1-t0
  est = list(para = theta,
             likelihood = ll,
             copula = claytonCopula(theta),
             cop = "Clayton Copula",
             time = tt)
  class(est) <- "CopC"
  return(est)
}
getCopC.gumbelCopula <- function(copula,interval,Yc,d,delta,nIS,jumps,MARGIN,...)
{
  t0 <- proc.time()
  interval[1] <- ifelse(interval[1]>=200,1,interval[1])
  interval[2] <- ifelse(interval[2]<=1,200,interval[2])
  interval[1] <- pmax(interval[1],1)
  interval[2] <- pmin(interval[2],200)
  opt <- optimize(f = CopLikelihood.gumbelCopula,interval = interval,copula=copula,Yc=Yc,d=d,delta=delta,nIS=nIS,jumps=jumps,MARGIN=MARGIN,...)
  t1 <- proc.time()
  theta <- opt$minimum
  ll <- opt$objective
  tt <-t1-t0
  est = list(para = theta,
             likelihood = ll,
             copula = gumbelCopula(theta),
             cop = "Gumbel Copula",
             time = tt)
  class(est) <- "CopC"
  return(est)
}
getCopC.frankCopula <- function(copula,interval,Yc,d,delta,nIS,jumps,MARGIN,...)
{
  t0 <- proc.time()
  interval[1] <- ifelse(interval[1]>=200,-200,interval[1])
  interval[2] <- ifelse(interval[2]<=-200,200,interval[2])
  interval[1] <- pmax(interval[1],-200)
  interval[2] <- pmin(interval[2],200)
  opt <- optimize(f = CopLikelihood.frankCopula,interval = interval,copula=copula,Yc=Yc,d=d,delta=delta,nIS=nIS,jumps=jumps,MARGIN=MARGIN,...)
  t1 <- proc.time()
  theta <- opt$minimum
  ll <- opt$objective
  tt <-t1-t0
  est = list(para = theta,
             likelihood = ll,
             copula = frankCopula(theta),
             cop = "Frank Copula",
             time = tt)
  class(est) <- "CopC"
  return(est)
}
getCopC.joeCopula <- function(copula,interval,Yc,d,delta,nIS,jumps,MARGIN,...)
{
  t0 <- proc.time()
  interval[1] <- ifelse(interval[1]>=100,1,interval[1])
  interval[2] <- ifelse(interval[2]<=1,100,interval[2])
  interval[1] <- pmax(interval[1],1)
  interval[2] <- pmin(interval[2],100)
  opt <- optimize(f = CopLikelihood.joeCopula,interval = interval,copula=copula,Yc=Yc,d=d,delta=delta,nIS=nIS,jumps=jumps,MARGIN=MARGIN,...)
  t1 <- proc.time()
  theta <- opt$minimum
  ll <- opt$objective
  tt <-t1-t0
  est = list(para = theta,
             likelihood = ll,
             copula = joeCopula(theta),
             cop = "Joe Copula",
             time = tt)
  class(est) <- "CopC"
  return(est)
}

# 8. Copula selection
#' The selection of copula function
#'
#' Among a list of copulas, select the one that gives the estimates
#' closest to the empirical copula function.
#' @importFrom  copula normalCopula
#' @importFrom  copula claytonCopula
#' @importFrom  copula gumbelCopula
#' @importFrom  copula joeCopula
#' @importFrom  copula frankCopula
#' @importFrom  copula dCopula
#' @importFrom  copula pCopula
#' @importFrom  copula iTau
#' @importFrom  stats optimize
#' @importFrom  stats ecdf
#' @importFrom  stats pnorm
#' @importFrom  stats qnorm
#' @importFrom  stats dnorm
#' @importFrom  stats rnorm
#' @importFrom  msm dtnorm
#' @importFrom  msm rtnorm
#' @importFrom  copBasic EMPIRcop
#' @usage
#' selectCopC(cop.type=c("Clayton","Gaussian","Gumbel","Joe","Frank"),Yc,d,delta,nIS=500,jumps=NULL,MARGIN=NULL,...,intervals=NULL)
#' @param cop.type a Kx1 vector containing the candidate copulas,
#' where K = length(cop.type) is the number of candidate copulas.
#' There are currently five available copula funcitons,
#' including Clayton copula, Gaussian copula, Gumbel copula, Joe copula and Frank copula.
#' Select each by specifying a vector consisting of at least one element from
#' c("Clayton","Gaussian","Gumbel","Joe","Frank").
#' @param Yc the Nx1 vector of observed responses
#' that are subject to lower detection limit.
#' @param d the lower detection limit.
#' @param delta the Nx1 vector of censoring indicator
#' with 1 indicating uncensored and 0 indicating left censored.
#' @param nIS the size for sequential importance sampling.
#' The default is 500.
#' @param jumps the Nx1 vector indicating whether
#' each time t is a start of a new time series, which is deemed to be
#' independent from the previous series.
#' @param MARGIN the marginal distribution of the latent time series.
#' @param ... additional parameters for the marginal distribution of the latent time series.
#' @param intervals a 2xK matrix specifying
#' the lower and upper bound for the copula parameter of each candidate copula, where K is the number
#' of candidate copulas.
#' @return \code{selectCopC} returns a list of components including
#' \item{paras}{a Kx1 vector containing the estimated copula parameters for each candidate copula.}
#' \item{likelihoods}{a Kx1 vector containing
#' the negative log-likelihood value corresponding to the estimated copula parameter
#' for each candidate copula.}
#' \item{estCop}{a list containing the estimated copula object for each candidate.}
#' \item{L2distance}{a Kx1 vector containing the L2 distance between each copula
#' with estimated copula parameter
#' and the empirical copula function.}
#' \item{Selected}{The selected copula object.}
#' @examples
#' ### Example with simulated data
#' set.seed(20)
#' Y <- genLatentY("Clayton",1,200,MARGIN.inv = qt,df=3)
#' d = -1
#' Yc = pmax(d,Y)
#' delta = (Y>d)
#' selectCopC(Yc = Yc,d = d,delta = delta,nIS=100)
#' ### Example with water data
#' attach(water)
#' set.seed(1)
#' intv.Gaussian = c(-1,1)
#' intv.Clayton = c(0,20)
#' intv.Frank = c(0,15)
#' intervals = cbind(intv.Gaussian,intv.Clayton,intv.Frank)
#' cop.type = c("Gaussian","Clayton","Frank")
#' selCopC <- selectCopC(cop.type=cop.type,Yc=TNH3,d=0.02,delta=Delta,jumps=Indep,intervals=intervals)
#' selCopC$Selected
#' @references Li, F., Tang, Y. and Wang, H. (2018) Copula-based Semiparametric Analysis for Time Series Data with Detection Limits
#' @seealso \code{\link{estCopC}}.
#' @export
selectCopC = function(cop.type=c("Clayton","Gaussian","Gumbel","Joe","Frank"),Yc,d,delta,nIS=500,jumps=NULL,MARGIN=NULL,...,intervals=NULL)
{
  ncop = length(cop.type)
  Cop.List <- lapply(cop.type,FUN = function(cop) switch(cop,
                                                         "Clayton" = claytonCopula(),
                                                         "Gaussian" = normalCopula(),
                                                         "Gumbel" = gumbelCopula(),
                                                         "Joe" = joeCopula(),
                                                         "Frank" = frankCopula()))
  if(is.null(intervals)){
    ints <- lapply(cop.type,FUN = function(cop) switch(cop,
                                                       "Clayton" = c(-1,Inf),
                                                       "Gaussian" = c(-1,1),
                                                       "Gumbel" = c(1,Inf),
                                                       "Joe" = c(1,Inf),
                                                       "Frank" = c(-Inf,Inf)))
    intMat = matrix(unlist(ints),nrow = 2)
  }else{
    intMat <- as.matrix(cbind(intervals))
    if(ncop!=ncol(intMat)) stop("Number of intervals does not match number of copula types.")
  }
  paras <- NULL
  ll <- NULL
  copulas <- NULL
  pCopC <- NULL
  emp = CopEmp(Yc,delta)
  if(emp$nEmp>0){#TODO: other criterion?
    p0 <- EMPIRcop(emp$u,emp$v,emp$UV,ctype = "hazen")
  }
  for(icop in 1:ncop){
    cand = Cop.List[[icop]]
    lower = intMat[1,icop]
    upper = intMat[2,icop]
    est <- getCopC(cand,interval = c(lower,upper),Yc=Yc,d=d,delta=!delta,nIS=nIS,jumps=jumps,MARGIN = MARGIN,...)
    paras <- c(paras,est$para)
    ll <- c(ll,est$likelihood)
    cop <- do.call(class(cand)[1],list(est$para))
    copulas <- c(copulas,cop)
    if(emp$nEmp>0){#TODO: other criterion?
      pp <- pCopula(cbind(emp$u,emp$v),cop)
      pCopC <- cbind(pCopC,pp)
    }
  }
  res = list(paras = paras,
             likelihoods = ll,
             estCop = copulas)
  if(emp$nEmp>0){#TODO: other criterion?
    L2distance <- sqrt(colMeans((pCopC-p0)^2))
    selection <- as.numeric(which.min(L2distance))
    bestCop <- copulas[[selection]]
    res <- c(res,
             list(L2distance=as.numeric(L2distance),
                  Selected = bestCop)
    )
    class(res) <- "CS_Comp"
  }else{
    class(res) <- "CS_Fail"
  }
  return(res)
}


CopSelect <- function(Cop.List=c("Clayton","Gaussian","Gumbel","Joe","Frank"),lower.tau,upper.tau,Yc,d,delta,nIS=2000,jumps=NULL,MARGIN=NULL,...)
{
  paras <- NULL
  ll <- NULL
  copulas <- NULL
  pCopC <- NULL
  emp = CopEmp(Yc,delta)
  if(emp$nEmp>0){#TODO: other criterion?
    p0 <- EMPIRcop(emp$u,emp$v,emp$UV,ctype = "hazen")
  }
  for(cand in Cop.List){
    lower = iTau(cand,tau = lower.tau)
    upper = iTau(cand,tau = upper.tau)
    est <- getCopC(cand,interval = c(lower,upper),Yc=Yc,d=d,delta=delta,nIS=nIS,jumps=jumps,MARGIN = MARGIN,...)
    paras <- c(paras,est$para)
    ll <- c(ll,est$likelihood)
    cop <- do.call(class(cand)[1],list(est$para))
    copulas <- c(copulas,cop)
    if(emp$nEmp>0){#TODO: other criterion?
      pp <- pCopula(cbind(emp$u,emp$v),cop)
      pCopC <- cbind(pCopC,pp)
    }
  }
  res = list(paras = paras,
             likelihoods = ll,
             estCop = copulas)
  if(emp$nEmp>0){#TODO: other criterion?
    L2distance <- sqrt(colMeans((pCopC-p0)^2))
    selection <- as.numeric(which.min(L2distance))
    bestCop <- copulas[[selection]]
    res <- c(res,
             list(L2distance=as.numeric(L2distance),
                  Selected = bestCop)
    )
    class(res) <- "CS_Comp"
  }else{
    class(res) <- "CS_Fail"
  }
  return(res)
}

#9. Quantile Estimation
#' Conditional Quantile Estimation
#'
#' Given estiamted copula with copula parameter and specified marginal distribution,
#' obtain the conditional qth quantile of Y_{n+1} given {Y1,...,Yn}.
#' @importFrom methods is
#' @importFrom utils tail
#' @importFrom  copula normalCopula
#' @importFrom  copula claytonCopula
#' @importFrom  copula gumbelCopula
#' @importFrom  copula joeCopula
#' @importFrom  copula frankCopula
#' @importFrom  copula dCopula
#' @importFrom  stats quantile
#' @importFrom  stats ecdf
#' @importFrom  stats pnorm
#' @importFrom  stats qnorm
#' @importFrom  stats dnorm
#' @importFrom  stats rnorm
#' @importFrom  stats uniroot
#' @importFrom  msm dtnorm
#' @importFrom  msm rtnorm
#' @usage
#' condQestCopC(tao,Yc,d,delta,copula,cop=NULL,theta=NULL,nIS=10000,MARGIN=NULL,MARGIN.inv=NULL,...)
#' @param tao the desired quantile level, a numeric value between 0 and 1.
#' @param Yc the Nx1 vector of observed responses that are subject to lower detection limit.
#' @param d the lower detection limit.
#' @param delta the Nx1 vector of censoring indicator
#' with 1 indicating uncensored and 0 indicating left censored.
#' @param copula the input copula object with copula parameter plugged in.
#' If specified, \code{cop} and \code{theta} can be omitted.
#' @param cop the choice of copula function.
#' There are currently five available copula funcitons,
#' including Clayton copula, Gaussian copula, Gumbel copula, Joe copula and Frank copula.
#' Specify one from "Clayton","Gaussian","Gumbel","Joe" and "Frank".
#' @param theta the copula parameter.
#' @param nIS the size for sequential importance sampling.
#' The default is 10000.
#' @param MARGIN the marginal distribution of the latent time series.
#' @param MARGIN.inv the inverse marginal distribution of the latent time series.
#' @param ... additional parameters for the marginal distribution of the latent time series.
#' @return \code{condQestCopC} returns the conditional tao-th quantile of Y_{n+1} given {Y1,...,Yn}
#' based on the specified copula function and marginal distribution.
#' @references Li, F., Tang, Y. and Wang, H. (2018).
#' Copula-based Semiparametric Analysis for Time Series Data with Detection Limits, technical report.
#' @examples
#' set.seed(20)
#' Y <- genLatentY(theta = 0.5,N = 100)
#' d <- -0.5
#' delta <- (Y>d)
#' Yc <- pmax(d,Y)
#' cq60.real <- condQestCopC(0.6,Yc,d,delta,copula=normalCopula(0.5),MARGIN=pnorm,MARGIN.inv=qnorm)
#' ### Use selected copula
#' selCop <- selectCopC(cop.type = c("Clayton","Gaussian"),Yc,d,delta,nIS=200)
#' cq60.est <- condQestCopC(0.6,Yc,d,delta,selCop$Selected)
#' @export
condQestCopC=function(tao,Yc,d,delta,copula=NULL,cop=NULL,theta=NULL,nIS=10000,MARGIN=NULL,MARGIN.inv=NULL,...)
{
  if(!is(copula,'Copula')){
    if(!is.null(cop) & !is.null(theta)){
      copula <- switch(cop,
                       "Clayton" = claytonCopula(theta),
                       "Gaussian" = normalCopula(theta),
                       "Gumbel" = gumbelCopula(theta),
                       "Joe" = joeCopula(theta),
                       "Frank" = frankCopula(theta))
    }else{
      stop("No copula function specified")
    }
  }
  return(cqCopC(tao,Yc,d,!delta,nIS,copula,MARGIN,MARGIN.inv,...))
}

cqCopC <- function(tao,Yc,d,delta,nIS,copula,MARGIN,MARGIN.inv,...)
{
  # delta = (Y<=d) is different from wrapper
  UseMethod("cqCopC",copula)
}
cqCopC.normalCopula <- function(tao,Yc,d,delta,nIS,copula,MARGIN,MARGIN.inv,...)
{
  theta = copula@parameters
  N = length(Yc)
  clen = ifelse(delta[N],tail(rle(delta)$length,1),0)
  if(is.null(MARGIN)) MARGIN=function(x) {ecdf(Yc)(x)*N/(N+1)}
  Pi = MARGIN(d,...)
  u0 = MARGIN(tail(Yc,clen+1)[1],...)
  if(clen==0){
    q1 = qnorm(u0)
    cmu = 0 + theta*1*(q1-0)
    csigma = sqrt(1-theta^2)
    q2 = qnorm(tao,cmu,csigma)
    vv = pnorm(q2)
    qq = vv
    qq = pmax(Pi,vv)
  }else{
    ff1 <- function(q){
      numer = CopInt.normalCopula(start = u0,upper = c(rep(Pi,clen),q),theta,nIS)
      denum = CopInt.normalCopula(start = u0,upper = rep(Pi,clen),theta,nIS)
      return(numer/denum-tao)
    }
    if(ff1(Pi)>=0){
      qq <- Pi
    }else{
      solvethis <- try(uniroot(ff1,interval = c(Pi,1-1e-6)),silent = T)
      psolve <- ifelse(is.character(solvethis),1-1e-6,solvethis$root)
      qq <- psolve
    }
  }
  if(is.null(MARGIN.inv)) MARGIN.inv=function(x){quantile(Yc,pmin(1,qq*(N+1)/N),type = 1)}
  Qest = MARGIN.inv(qq,...)
  return(as.numeric(Qest))
}
cqCopC.claytonCopula <- function(tao,Yc,d,delta,nIS,copula,MARGIN,MARGIN.inv,...)
{
  theta = copula@parameters
  N = length(Yc)
  clen = ifelse(delta[N],tail(rle(delta)$length,1),0)
  if(is.null(MARGIN)) MARGIN=function(x) {ecdf(Yc)(x)*N/(N+1)}
  Pi = MARGIN(d,...)
  u0 = MARGIN(tail(Yc,clen+1)[1],...)
  if(clen==0){
    vv = myrCopula.claytonCopula(tao,u0,theta)
    qq = vv
    qq = pmax(Pi,vv)
  }else{
    ff1 <- function(q){
      numer = CopInt.claytonCopula(start = u0,upper = c(rep(Pi,clen),q),theta,nIS)
      denum = CopInt.claytonCopula(start = u0,upper = rep(Pi,clen),theta,nIS)
      return(numer/denum-tao)
    }
    if(ff1(Pi)>=0){
      qq <- Pi
    }else{
      solvethis <- try(uniroot(ff1,interval = c(Pi,1-1e-6)),silent = T)
      psolve <- ifelse(is.character(solvethis),1-1e-6,solvethis$root)
      qq <- psolve
    }
  }
  if(is.null(MARGIN.inv)) MARGIN.inv=function(x){quantile(Yc,pmin(1,qq*(N+1)/N),type = 1)}
  Qest = MARGIN.inv(qq,...)
  return(as.numeric(Qest))
}
cqCopC.gumbelCopula <- function(tao,Yc,d,delta,nIS,copula,MARGIN,MARGIN.inv,...)
{
  theta = copula@parameters
  N = length(Yc)
  clen = ifelse(delta[N],tail(rle(delta)$length,1),0)
  if(is.null(MARGIN)) MARGIN=function(x) {ecdf(Yc)(x)*N/(N+1)}
  Pi = MARGIN(d,...)
  u0 = MARGIN(tail(Yc,clen+1)[1],...)
  if(clen==0){
    vv = myrCopula.gumbelCopula(tao,u0,theta)
    qq = vv
    qq = pmax(Pi,vv)
  }else{
    ff1 <- function(q){
      numer = CopInt.gumbelCopula(start = u0,upper = c(rep(Pi,clen),q),theta,nIS)
      denum = CopInt.gumbelCopula(start = u0,upper = rep(Pi,clen),theta,nIS)
      return(numer/denum-tao)
    }
    if(ff1(Pi)>=0){
      qq <- Pi
    }else{
      solvethis <- try(uniroot(ff1,interval = c(Pi,1-1e-6)),silent = T)
      psolve <- ifelse(is.character(solvethis),1-1e-6,solvethis$root)
      qq <- psolve
    }
  }
  if(is.null(MARGIN.inv)) MARGIN.inv=function(x){quantile(Yc,pmin(1,qq*(N+1)/N),type = 1)}
  Qest = MARGIN.inv(qq,...)
  return(as.numeric(Qest))
}
cqCopC.frankCopula <- function(tao,Yc,d,delta,nIS,copula,MARGIN,MARGIN.inv,...)
{
  theta = copula@parameters
  N = length(Yc)
  clen = ifelse(delta[N],tail(rle(delta)$length,1),0)
  if(is.null(MARGIN)) MARGIN=function(x) {ecdf(Yc)(x)*N/(N+1)}
  Pi = MARGIN(d,...)
  u0 = MARGIN(tail(Yc,clen+1)[1],...)
  if(clen==0){
    vv = myrCopula.frankCopula(tao,u0,theta)
    qq = vv
    qq = pmax(Pi,vv)
  }else{
    ff1 <- function(q){
      numer = CopInt.frankCopula(start = u0,upper = c(rep(Pi,clen),q),theta,nIS)
      denum = CopInt.frankCopula(start = u0,upper = rep(Pi,clen),theta,nIS)
      return(numer/denum-tao)
    }
    if(ff1(Pi)>=0){
      qq <- Pi
    }else{
      solvethis <- try(uniroot(ff1,interval = c(Pi,1-1e-6)),silent = T)
      psolve <- ifelse(is.character(solvethis),1-1e-6,solvethis$root)
      qq <- psolve
    }
  }
  if(is.null(MARGIN.inv)) MARGIN.inv=function(x){quantile(Yc,pmin(1,qq*(N+1)/N),type = 1)}
  Qest = MARGIN.inv(qq,...)
  return(as.numeric(Qest))
}
cqCopC.joeCopula <- function(tao,Yc,d,delta,nIS,copula,MARGIN,MARGIN.inv,...)
{
  theta = copula@parameters
  N = length(Yc)
  clen = ifelse(delta[N],tail(rle(delta)$length,1),0)
  if(is.null(MARGIN)) MARGIN=function(x) {ecdf(Yc)(x)*N/(N+1)}
  Pi = MARGIN(d,...)
  u0 = MARGIN(tail(Yc,clen+1)[1],...)
  if(clen==0){
    vv = myrCopula.joeCopula(tao,u0,theta)
    qq = vv
    qq = pmax(Pi,vv)
  }else{
    ff1 <- function(q){
      numer = CopInt.joeCopula(start = u0,upper = c(rep(Pi,clen),q),theta,nIS)
      denum = CopInt.joeCopula(start = u0,upper = rep(Pi,clen),theta,nIS)
      return(numer/denum-tao)
    }
    if(ff1(Pi)>=0){
      qq <- Pi
    }else{
      solvethis <- try(uniroot(ff1,interval = c(Pi,1-1e-6)),silent = T)
      psolve <- ifelse(is.character(solvethis),1-1e-6,solvethis$root)
      qq <- psolve
    }
  }
  if(is.null(MARGIN.inv)) MARGIN.inv=function(x){quantile(Yc,pmin(1,qq*(N+1)/N),type = 1)}
  Qest = MARGIN.inv(qq,...)
  return(as.numeric(Qest))
}


























































