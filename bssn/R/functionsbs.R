################################################################
# Birnbaum Saunders  density function

bsd <-
  function(
    x,           # evaluation points
    m,           # mode parameter
    h            # variability parameter (denoted with "v" in the reference paper)
    ) { #print(m)
    return(
      dbs(x, alpha=(m+h)*sqrt(abs(h-m)/(h*m*(m+3*h))), beta=h)
    )
  }

#bsd(1,1,2)
bsd2 <- #Funcion en funcion de la moda
  function(
    x,
    m,
    h,
    logar=FALSE
  ){

    xim <- sqrt(m/h)-sqrt(h/m)
    a = M1  <- h*m*(m+3*h)/((m-h)*(m+h)^2)
    N       <- (h^0.5 + 3*h*xim^2)/xim^4
    aa = N1  <- N - h*M1
    bb = b = M2  <- xim^-2/h^0.5 + 1 - xim^2*M1
    dd = d = A1  <- - xim^2/(h^2.5)
    cc = c = A2  <- -1/(h^1.5)

    #rootSolution[1]^6*N1 + rootSolution[1]^4*M2 + A2*rootSolution[1]^2 + A1

    response  <- c()
    k  <- 0
    rootSolution <- polyroot(c(A1,0,A2,0,M2,0,N1))
    for(r in 1:6)
    {
      if(abs(Im(rootSolution[r])) < 0.00001 && Re(rootSolution[r])>0  )
      {
        k <- k +1
        response[k] <- Re(rootSolution[r])
      }
    }
    ###
    solve1 <- 2*bb^3 - 9*aa*bb*cc + 27*aa^2*dd
    solve2 <- (2*bb^3 - 9*aa*bb*cc + 27*aa^2*dd)^2 - 4*(bb^2 - 3*aa*cc)^3
    if((solve1 + sqrt(solve2) < 0) && (solve1 - sqrt(solve2) < 0)){
     alpha_result <- -bb/(3*aa) + 1/(3*aa)*(0.5*abs(solve1 + sqrt(solve2)) )^0.33333333333 + 1/(3*aa)*(0.5*abs(solve1 - sqrt(solve2)) )^0.33333333333
    }else{
     alpha_result <-  -bb/(3*aa) - 1/(3*aa)*(0.5*(solve1 + sqrt(solve2)) )^0.33333333333 - 1/(3*aa)*(0.5*(solve1 - sqrt(solve2)) )^0.33333333333
    }
    ###

    return(
      dbs(x,alpha=alpha_result,beta=h)
    )
  }

#bsd(1,1,2)

########################

alpham2 <- function(m,h)
{
  xim <- sqrt(m/h)-sqrt(h/m)
  a = M1  <- h*m*(m+3*h)/((m-h)*(m+h)^2)
  N       <- (h^0.5 + 3*h*xim^2)/xim^4
  aa = N1  <- N - h*M1
  bb = b = M2  <- xim^-2/h^0.5 + 1 - xim^2*M1
  dd = d = A1  <- - xim^2/(h^2.5)
  cc = c = A2  <- -1/(h^1.5)

 response  <- c()
 k  <- 0
 rootSolution <- polyroot(c(A1,0,A2,0,M2,0,N1))
 for(r in 1:6)
 {
   if(abs(Im(rootSolution[r])) < 0.00001 && Re(rootSolution[r])>0  )
   {
     k <- k +1
     response[k] <- Re(rootSolution[r])
   }
 }
 print(response)
 alpha_result <- min(response)

 return(c(alpha_result))
}

alpham <- function(m,h)
{

  alpha_result <- (m+h)*sqrt(abs(h-m)/(h*m*(m+3*h)))
  return(c(alpha_result))
}

#alpham(m = 5.8,h = 7)

############################

dbs   <- function(ti,alpha=0.5,beta=1)
{
 if(!is.numeric(alpha)||!is.numeric(beta))
 if(alpha<=0){stop("alpha must be positive")}
 if(beta<=0) {stop("beta must be positive")}
 at     <- (1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti))
 At     <- (ti^(-1.5)*(ti+beta))/(2*alpha*sqrt(beta))
 pdf   <- dnorm(at)*At
 return(pdf)
}



pbs    <- function(q,alpha=0.5,beta=1)
{
  if(!is.numeric(alpha)||!is.numeric(beta))
  if(alpha<=0){ stop("alpha must be positive")}
  if(beta<=0) { stop("beta must be positive") }
  I      <- vector(mode = "numeric", length = length(q))
  for(i in 1:length(q))
  {
    pdf  <- function(ti) dnorm((1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti)))*(ti^(-1.5)*(ti+beta))/(2*alpha*sqrt(beta))
    I[i] <- integrate(pdf,0,q[i])$value
  }
  return(I)
}

rbs <-function(n,alpha=1.0,beta=1.0)
{
  if(!is.numeric(n)||!is.numeric(alpha)||!is.numeric(beta))
  {stop("non-numeric argument to mathematical function")}
  if(n<=0){stop("value of n must be greater or equal then 1")}
  if(alpha<=0){stop("alpha must be positive")}
  if(beta<=0){stop("beta must be positive")}
  z<-rnorm(n,0,1)
  t<-beta*(1+(((alpha^2)*(z^2))/2)+(alpha*z*sqrt((((alpha^2)*(z^2))/4)+1)))
  return(t)
}

auxq_bs <- function(q, alpha,beta){

  a <- alpha
  b <- beta

  quasiqdig <- function(x, alpha, beta, pe){
    ecuacion <- pbs(x,alpha,beta) - pe
    return(ecuacion)
  }
  res <- uniroot(quasiqdig,
                 c(1e-12, 100),
                 tol = 1e-33,
                 alpha = a,
                 beta = b,
                 pe = q)
  quantile <- res$root
  return(quantile)
}

qbs <- function(q, alpha, beta){
  a <- alpha
  b <- beta
  n <- length(q)
  qres <- rep(1, n)
  for(i in 1:n){
    h <- q[i]
    val <- auxq_bs(q = h, alpha=a,beta)
    qres[i] <- val
  }
  return(qres)
}
#qbs(q, alpha, beta)
Infmatrixbs<- function(ti,alpha,beta){
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

    dPsi.dalpha  <- -at*At*datalpha*dnorm(at)+dAAtalpha*dnorm(at)
    dPsi.dbeta   <- -at*At*datbeta*dnorm(at)+dAAtbeta*dnorm(at)


    Ssialpha     <- (1/dbs(Ti,alpha,beta))*dPsi.dalpha
    Ssibeta      <- (1/dbs(Ti,alpha,beta))*dPsi.dbeta


    S            <- c(S, Ssialpha, Ssibeta)
    soma         <- soma + S%*%t(S)
  }

  EP             <-  sqrt(diag(solve(soma)))

  return(list(IM=soma,EP=EP))
}




#For computer initial values of beta

mmmeth <- function(ti)
{
  S <- function(ti)
  {
    n <- length(ti)
    return((1/n)*sum(ti))
  }

  R <- function(ti)
  {
    n <- length(ti)
    return(((1/n)*sum(ti^(-1)))^(-1))
  }

  beta0ini  <- (S(ti)*R(ti))^0.5
  alpha0ini <- sqrt(2)*((S(ti)/R(ti))^0.5 - 1)^0.5

  result   <- list(beta0init = beta0ini,alpha0ini=alpha0ini, n = length(ti))
  return(result)
}


Infmatrixbs <- function(ti,alpha,beta)
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

    if(dnorm(at)==0)
    {
     at           <- .Machine$double.xmin
     dPsi.dalpha  <- dnorm(-at)*(dAAtalpha-at*At*datalpha)
     dPsi.dbeta   <- dnorm(-at)*(dAAtbeta-at*At*datbeta)
    }else{
     dPsi.dalpha  <- dnorm(at)*(dAAtalpha-at*At*datalpha)
     dPsi.dbeta   <- dnorm(at)*(dAAtbeta-at*At*datbeta)
    }

    Ssialpha     <- (1/dbs(Ti, alpha, beta))*dPsi.dalpha
    Ssibeta      <- (1/dbs(Ti, alpha, beta))*dPsi.dbeta
    #print(dbs(Ti, alpha, beta))

    S            <- c(S, Ssialpha, Ssibeta)
    soma         <- soma + S%*%t(S);
  }

  EP             <-  sqrt(diag(solve(soma)))

  return(list(IM=soma,EP=EP))
}

#Infmatrixbs(ti, c(25.72751),c(25.72751))

qqbs <-  function(x,alpha,beta,line=FALSE, xlab='Empirical quantiles',ylab='Theoretical quantiles',ns=0.05)
{
  #estimate <- est1bs(x)
  #alpha    <- estimate$alpha
  #beta     <- estimate$beta
  n        <- length(x)
  k        <- seq(1,n,by=1)
  P        <-( k-0.5)/n
  Finv     <- qbs(P,alpha,beta)
  quantile <- sort(x)
  plot(quantile,Finv,xlab=xlab,ylab=ylab,col="black",lwd=1)

  if(line==TRUE)
  {
    quant <- quantile(x)
    x1    <- quant[2]
    x2    <- quant[4]
    y1    <- qbs(0.25,alpha,beta)
    y2    <- qbs(0.75,alpha,beta)
    m     <- ((y2-y1)/(x2-x1))
    inter <- y1-(m*x1)
    abline(inter,m,col="black",lwd=2,lty=1)
  }
}



envel    <- function(ti,alpha,beta,ns)
{
 n       <- length(ti)
 at      <- (1/alpha)*(sqrt(ti/beta)-sqrt(beta/ti))
 d2      <- at^2
 d2s     <- sort(d2)
 d2s     <- t(d2s)
 xq2     <- qchisq(ppoints(n), 1)
 replic  <- 200
 Xsim    <- matrix(0,replic,n)
 for(i in 1:replic) {Xsim[i,]<-rchisq(n, 1)}
 Xsim2   <- apply(Xsim,1,sort)
 d21     <- rep(0,n)
 d22     <- rep(0,n)
 for(i in 1:n)
 {
  d21[i] <- quantile(Xsim2[i,],ns/2)
  d22[i] <- quantile(Xsim2[i,],1-ns/2)
 }
 d2med   <-apply(Xsim2,1,mean)
 fy      <- range(d2s,d21,d22)

 # postscript("envelsn.eps", width=5.75, height=5.75, horizontal=FALSE, onefile=TRUE)
 plot(xq2,d2s,xlab = expression(paste("Theoretical ",chi[1]^2, " quantiles")), ylab="Sample values and simulated envelope",pch=20,ylim=fy,main="BS")
 par(new=T)
 plot(xq2,d21,type="l",ylim=fy,xlab="",ylab="")
 par(new=T)
 plot(xq2,d2med,type="l",ylim=fy,xlab="",ylab="",lty="dashed")
 par(new=T)
 plot(xq2,d22,type="l",ylim=fy,xlab="",ylab="")
 #   dev.off() #Fechando o dispositivo potscript
}


#envel(ti[fitBS_g_3$result$cluster==1],fitBS_g_3$result$alpha[1] ,fitBS_g_3$result$beta[1],ns=0.05)
#envel(ti[fitBS_g_3$result$cluster==2],fitBS_g_3$result$alpha[2] ,fitBS_g_3$result$beta[2],ns=0.05)
#envel(ti[fitBS_g_3$result$cluster==3],fitBS_g_3$result$alpha[3] ,fitBS_g_3$result$beta[3],ns=0.05)
