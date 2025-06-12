dbssn2   <- function(ti,alpha=0.5,beta=1,delta)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(delta))
    if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0) {stop("beta must be positive")}
  at     <- (1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti))
  At     <- (ti^(-1.5)*(ti+beta))/(2*alpha*sqrt(beta))
  pdf    <- 2*dnorm(at)*At*pnorm((delta/sqrt(1 + delta^2))*at)
  return(pdf)
}

dbssn   <- function(ti,alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0) {stop("beta must be positive")}
  at     <- (1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti))
  At     <- (ti^(-1.5)*(ti+beta))/(2*alpha*sqrt(beta))
  pdf    <- 2*dnorm(at)*At*pnorm(lambda*at)
  return(pdf)
}


# Birnbaum Saunders Skew Normal cumulative distribution function
pbssn    <- function(q,alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){ stop("alpha must be positive")}
  if(beta<=0) { stop("beta must be positive") }
  I      <- vector(mode = "numeric", length = length(q))
  for(i in 1:length(q))
  {
    pdf  <- function(x) 2*dnorm((1/alpha)*(sqrt(x/beta)-sqrt(beta/x)))*(x^(-1.5)*(x+beta))/(2*alpha*sqrt(beta))*pnorm(lambda*(1/alpha)*(sqrt(x/beta)-sqrt(beta/x)))
    I[i] <- integrate(pdf,0,q[i])$value
  }
  return(I)
}

qbssn <- function(p,alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){ stop("alpha must be positive")}
  if(beta<=0) { stop("beta must be positive") }
  zp     <- qsn(p,xi=0,omega=1,lambda) #quantile for the skew-normal distribution
  tp     <- (beta/4)*(alpha*zp + sqrt(alpha^2*zp^2 + 4))^2
  return(tp)
}


rbssn<-function(n,alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(n)||!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
  {stop("non-numeric argument to mathematical function")}
  if(alpha<=0){ stop("alpha must be positive")}
  if(beta<=0) { stop("beta must be positive") }
  z      <- rsn(n,0,1,lambda)
  r      <- beta*((alpha*z*0.5)+sqrt((alpha*z*0.5)^2+1))^2
  return(r)
}

#------------------------------------------------#
#  Random numbers generator function for mixture #
#------------------------------------------------#

rmixbssn <- function(n,alpha,beta,lambda,pii)
{
  y <- vector()
  G <- length(alpha)

  z <- sample(G,size=n,replace=TRUE,prob=pii)
  for(i in 1:n)
  {
    y[i] = rbssn(1,alpha[z[i]],beta[z[i]],lambda[z[i]])
  }
  return(list(z=z,y=y))
}

#---------------------------------#
# Log-likelihood function of bssn #
#---------------------------------#

logLikbssn <- function(x,alpha=1, beta=1, lambda=0)
{
  return(sum(log(dbssn(x, alpha = alpha,
                       beta     = beta,
                       lambda    = lambda))))
}
#logLikbssn(c(1,2,4,5),alpha=1, beta=3, delta=2)


#--------------------------#
# BS-SN reliability function  #
#--------------------------#

Rebssn <- function(ti,alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){ stop("alpha must be positive")}
  if(beta<=0) { stop("beta must be positive") }
  at     <- (1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti))
  S      <- 1-psn(at,0,1,lambda)
  return(S)
}

#y<-seq(0,2,0.01)
#f1<-Rebssn(y,0.75,1,1)
#f2<-Rebssn(y,1,1,1)
#f3<-Rebssn(y,1.5,1,1)
#f4<-Rebssn(y,2,1,1)
#den<-cbind(f1,f2,f3,f4)

#matplot(y,den,type="l",col=c("deepskyblue4","firebrick1","darkmagenta","aquamarine4"),ylab="S(t)",xlab="t",lwd=2)
#legend(1.5,1,c(expression(alpha==0.75),expression(alpha==1),expression(alpha==1.5),expression(alpha==2)),col=c("deepskyblue4","firebrick1","darkmagenta","aquamarine4"),lty=1:4,lwd=2,seg.len=2,cex=0.9,box.lty=1,bg=NULL)

Fbssn <- function(ti,alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0) {stop("beta must be positive") }
  at     <- (1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti))
  At     <- (ti^(-1.5)*(ti+beta))/(2*alpha*sqrt(beta))
  f      <- 2*dnorm(at)*At*pnorm(lambda*at)
  S      <- 1-psn(at,0,1,lambda)
  R      <- f/S
  return(R)
}

#y<-seq(0,2,0.01)
#f1<-Fbssn(y,0.5,1,-1)
#f2<-Fbssn(y,0.5,1,-2)
#f3<-Fbssn(y,0.5,1,-3)
#f4<-Fbssn(y,0.5,1,-4)
#den<-cbind(f1,f2,f3,f4)
#matplot(y,den,type="l",col=c("deepskyblue4","firebrick1","darkmagenta","aquamarine4"),ylab="h(t)",xlab="t",lwd=2)
#legend(0.1,23,c(expression(lambda==0.75),expression(lambda==1),expression(lambda==1.5),expression(lambda==2)),col=c("deepskyblue4","firebrick1","darkmagenta","aquamarine4"),lty=1:4,lwd=2,seg.len=2,cex=0.9,box.lty=1,bg=NULL)


#--------------------------#
# BS-SN hazard function  #
#--------------------------#

Hbssn <- function(ti,alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0) {stop("beta must be positive") }
  at     <- (1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti))
  At     <- (ti^(-1.5)*(ti+beta))/(2*alpha*sqrt(beta))
  f      <- 2*dnorm(at)*At*pnorm(lambda*at)
  S      <- 1-psn(at,0,1,lambda)
  R      <- f/S
  return(R)
}



meanbssn <- function(alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0) {stop("beta must be positive") }
  k      <- 1
  W      <- vector(mode = "numeric", length = length(k))
  for(i in 1:length(k))
  {
    f<-function (z) z^k[i]*sqrt(alpha^2*z^2 + 4)*dsn(z,0,1,lambda)
    W[i]<-integrate(f,lower = -Inf, upper = Inf)$value
  }

  meanbssnresult = (beta/2)*(2+alpha^2+alpha*W[1])
  return(meanbssnresult)
}

varbssn    <- function(alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0) {stop("beta must be positive") }
  k        <- seq(1,3,2)
  W        <- vector(mode = "numeric", length = length(k))

  for(i in 1:length(k))
  {
    f      <-function (z) z^k[i]*sqrt(alpha^2*z^2 + 4)*dsn(z,0,1,lambda)
    W[i]   <-integrate(f,lower = -Inf, upper = Inf)$value
  }

  ET       <- (beta/2)*(2+alpha^2+alpha*W[1]) #Expectation
  ET2      <- (beta^2/2)*(2+4*alpha^2+3*alpha^4+2*alpha*W[1]+alpha^3*W[2])

  varbssnresult  <- ET2-(ET)^2 #Variance
  return(varbssnresult)
}

skewbssn   <- function(alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0) {stop("beta must be positive") }
  k        <- seq(1,7,2)
  W        <- vector(mode = "numeric", length = length(k))
  for(i in 1:length(k))
  {
    f      <-function (z) z^k[i]*sqrt(alpha^2*z^2 + 4)*dsn(z,0,1,lambda)
    W[i]   <-integrate(f,lower = -Inf, upper = Inf)$value
  }

  ET       <- (beta/2)*(2+alpha^2+alpha*W[1]) #Expectation
  ET2      <- (beta^2/2)*(2+4*alpha^2+3*alpha^4+2*alpha*W[1]+alpha^3*W[2])
  ET3      <- (beta^3/2)*(2+9*alpha^2+18*alpha^4+15*alpha^6+3*alpha*W[1]+4*alpha^3*W[2]+alpha^5*W[3])
  var      <- ET2-(ET)^2 #Variance
  skewbssnresult <- (ET3-3*ET*ET2+2*(ET)^3)/(var)^1.5 #skewness
  return(skewbssnresult)
}
#skewbssn(alpha=0.5,beta=1,lambda=1.5)

kurtbssn <- function(alpha=0.5,beta=1,lambda=1.5)
{
  if(!is.numeric(alpha)||!is.numeric(beta)||!is.numeric(lambda))
    if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0) {stop("beta must be positive") }
  k        <- seq(1,7,2)
  W        <- vector(mode = "numeric", length = length(k))

  for(i in 1:length(k))
  {
    f      <- function (z) z^k[i]*sqrt(alpha^2*z^2 + 4)*dsn(z,0,1,lambda)
    W[i]   <- integrate(f,lower = -Inf, upper = Inf)$value
  }

  ET       <- (beta/2)*(2+alpha^2+alpha*W[1])
  ET2      <- (beta^2/2)*(2+4*alpha^2+3*alpha^4+2*alpha*W[1]+alpha^3*W[2])
  ET3      <- (beta^3/2)*(2+9*alpha^2+18*alpha^4+15*alpha^6+3*alpha*W[1]+4*alpha^3*W[2]+alpha^5*W[3])
  ET4      <- (beta^4/2)*(2+16*alpha^2+60*alpha^4+120*alpha^6+105*alpha^8+4*alpha*W[1]+10*alpha^3*W[2]+6*alpha^5*W[3]+alpha^7*W[4])
  var      <- ET2-(ET)^2 #Variance
  kurtbssnresult <- (ET4-4*ET*ET3+6*ET^2*ET2-3*ET^4)/(var)^2 #kurtosis
  return(kurtbssnresult)
}




#Observed Information Matrix

Infmatrix <- function(ti,alpha,beta,lambda)
{
  n           <- length(ti)

  soma       <- 0
  for (i in 1:n)
  {
    S         <- c() #vetor com todas as derivadas em relacao a cada parametro desconhecido do modelo

    Ti        <- ti[i]
    at        <- (1/alpha)*(sqrt(Ti/beta)-sqrt(beta/Ti))
    At        <- Ti^(-1.5)*(Ti + beta)/(2*alpha*beta^0.5)

    #derivadinhas
    datalpha     <- -(1/alpha)*at
    datbeta      <- -(1/(2*alpha*beta))*(sqrt(Ti/beta)+sqrt(beta/Ti))

    dAAtalpha    <- -(1/alpha)*At
    dAAtbeta     <- Ti^(-1.5)*(beta - Ti)/(4*alpha*beta^1.5)

    dPsi.dalpha  <- 2*(dAAtalpha*dnorm(at)*pnorm(lambda*at)+At*datalpha*dnorm(at)*(lambda*dnorm(lambda*at)-at*pnorm(lambda*at)))
    dPsi.dbeta   <- 2*(dAAtbeta*dnorm(at)*pnorm(lambda*at)+At*datbeta*dnorm(at)*(lambda*dnorm(lambda*at)-at*pnorm(lambda*at)))
    dPsi.dlambda <- 2*at*At*dnorm(at)*dnorm(lambda*at)

    Ssialpha     <- (1/dbssn(Ti,alpha,beta,lambda))*dPsi.dalpha
    Ssibeta      <- (1/dbssn(Ti,alpha,beta,lambda))*dPsi.dbeta
    Ssilambda     <- (1/dbssn(Ti,alpha,beta,lambda))*dPsi.dlambda

    S            <- c(S, Ssialpha, Ssibeta, Ssilambda)
    soma         <- soma + S%*%t(S)
  }

  EP             <-  sqrt(diag(solve(soma)))

  return(list(IM=soma,EP=EP))
}

Infmatrix1 <- function(ti,alpha,beta,lambda)
{
  n    <- length(ti)
  at   <- (1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti))
  WPhi <- dnorm(lambda*at)/pnorm(lambda*at)
  W1Phi <- -WPhi*(lambda*at+WPhi)
  Iaa  <- n/alpha^2+(6*n/alpha^4)-(3/(alpha^4*beta))*sum(ti)-(3*beta/alpha^4)*sum(1/ti)+(2*lambda/alpha^3)*sum((sqrt(ti/beta)-sqrt(beta/ti))*WPhi)+(lambda^2/alpha^4)*sum((ti/beta+beta/ti-2)*W1Phi)
  Iab  <- -1/(alpha^3*beta^2)*sum(ti)+(1/alpha^3)*sum(1/ti)+(0.5*lambda/alpha^2)*sum((sqrt(ti)/beta^1.5+1/(beta^0.5*ti^0.5))*WPhi)+lambda^2/(2*alpha^3)*sum((ti/beta^2-1/ti)*W1Phi)
  Ibb  <- n/(2*beta^2)-sum(1/(ti+beta)^2)-(1/(alpha^2*beta^3))*sum(ti)+(lambda/(4*alpha))*sum(((3*sqrt(ti)/beta^2.5)+(1/(sqrt(ti)*beta^1.5)))*WPhi)+(lambda^2/(4*alpha^2))*sum((ti/beta^3+2/beta^2+1/(beta*ti))*W1Phi)
  Ial  <- -(1/alpha^2)*sum((sqrt(ti/beta)-sqrt(beta/ti))*WPhi)-(lambda/alpha^3)*sum((ti/beta+beta/ti-2)*W1Phi)
  Ibl  <- -(0.5/alpha)*sum((sqrt(ti)/beta^1.5+1/(sqrt(beta*ti)))*WPhi)+(0.5*lambda/alpha^2)*sum((1/ti-ti/beta^2)*W1Phi)
  Ill  <- (1/alpha^2)*sum((ti/beta+beta/ti-2)*W1Phi)
  f1   <- cbind(Iaa,Iab,Ial)
  f2   <- cbind(Iab,Ibb,Ibl)
  f3   <- cbind(Ial,Ibl,Ill)
  soma <- -rbind(f1,f2,f3)
  EP   <-  sqrt(diag(solve(soma)))
  return(list(IM=soma,EP=EP))
}
