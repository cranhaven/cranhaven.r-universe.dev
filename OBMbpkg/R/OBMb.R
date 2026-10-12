#' Objective Bayesian Analysis for the Mb Capture-Recapture Model
#'
#' Applies an objective Bayesian method on to the Mb capturere-capture model to estimate the population size N.
#'
#' @param k Number of sampling occasions
#' @param n Total number of distinct animals captured
#' @param M Number of marked animals captured in all sampling occasions
#' @param x The number of new animals captured at each sampling occasion
#' @param CI1 Lower confidence level
#' @param CI2 Upper confidence level
#' @param max The maximum of function evaluations used for computing the integrated likelihood L(N|X)
#' @param IFMLE Logical, will also print MLE results if TRUE
#'
#'
#' @return
#'
#' \itemize{
#'  \item{EMEAN: }{Posterior mean for N}
#'  \item{EMEDIAN: }{Posterior median for N}
#'  \item {OBCI: }{Credible interval values based on the quantiles specified by CI1 and CI2}
#'  \item {MLE: }{If IFMLE==TRUE, this is the frequentist MLE for N}
#'  \item {Ep: }{If IFMLE==TRUE, the frequentist estimate of the initial capture probability p}
#'  \item {MLECI: }{If IFMLE==TRUE, confidence interval for the MLE quantile specified by CI2}
#' }
#'
#'
#'
#' @examples
#'
#' # Data simulation example
#' k=10
#' tN=600   #True N
#' p=0.06
#' JN=rep(0,k+1)
#'
#' N=rep(0,k)
#' x=rep(0,k)
#' for (j in 1:k){
#'   N[j]=tN-JN[j]
#'   x[j]=rbinom(1,N[j],p)
#'   JN[j+1]=JN[j]+x[j]
#' }
#' M=sum(JN[1:k])
#' n=JN[k+1]
#'
#' OBMb(k=k,n=n,M=M,x=x)
#'
#' #Deer mouse example from Otis et al 1978
#' Data<-c(15, 8, 6, 3, 3, 3)   #new animals captured at each sampling occasion
#'
#' OBMb(k=6,n=38,M=134,x=Data)
#' @export


#################################################
# Objective Bayesian for capture-recapture Mb   #
# Purpose: packaging functions                  #
# By Dan Zheng                                  #
# Date: 08/09/2013                              #
#################################################

OBMb=function(k,n,M,x,CI1=0.025,CI2=0.975,max=10000,IFMLE=TRUE){
  sterm1=c(0,max)
  esterm1=c(0,max)
  sterm2=c(0,max)
  esterm2=c(0,max)

  for (i in 1:max){
    JN=i+n-1
    fp=function(p){
      p^(n-1)*(1-p)^(k*JN-M-n-0.5)*sqrt(1-(1-p)^k)
    }
    sterm1[i]=lgamma(JN+1)-lgamma(JN-n+1)+lgamma(k*JN-M-n+0.5)-lgamma(k*JN-M+1)-(k/2)*log(JN)
    sterm2[i]=sterm1[i]+log(JN)
    esterm1[i]=exp(sterm1[i]-sterm1[1])
    esterm2[i]=exp(sterm2[i]-sterm1[1])
  }
  con=sum(esterm1)
  smean=sum(esterm2)
  EMEAN=smean/con

  halfcon=c(0.5*con, CI1*con,CI2*con)
  iterm=rep(1,3)
  for(i in 1:3){
    sum=0
    while (sum <=halfcon[i]){
      sum=sum+esterm1[iterm[i]]
      iterm[i]=iterm[i]+1}
  }
  EMED=n+iterm[1]-1
  LCI1=n+iterm[2]-1
  UCI1=n+iterm[3]-1

  j=seq(1,k)
  Constain=sum(x*(k+1-2*j))

  if (IFMLE==TRUE & Constain>0){
    #MLE
    fmle=function(N){
      (k*N-n-M)*log(1-n/(k*N-M))+n*log(n/(k*N-M))+lgamma(N+1)-lgamma(N-n+1)
    }

    NMLE0=n
    NMLE=n+1
    ALF0=fmle(NMLE0)
    ALF=fmle(NMLE)
    while(ALF0<ALF){
      NMLE0=NMLE
      NMLE=NMLE+1
      ALF0=fmle(NMLE0)
      ALF=fmle(NMLE)
    }

    MLE=NMLE0
    Ep=n/(k*MLE-M)
    F=(1-(1-Ep)^k)/(MLE*(1-Ep)^k)
    VAR=(1-Ep)*n/(F*(1-Ep)*n)-(Ep^k)^2
    LCI2=MLE-sqrt(VAR)*(stats::qnorm(CI2,0,1))
    UCI2=MLE+sqrt(VAR)*(stats::qnorm(CI2,0,1))
    list(k=k,
         n=n,
         EMEAN=EMEAN,
         EMEDIAN=EMED,
         OBCI=c(LCI1,UCI1),
         MLE=MLE,
         Ep=Ep,
         MLECI=c(LCI2,UCI2)
    )
  }
  else if (IFMLE==TRUE & Constain<=0){
    list(k=k,
         n=n,
         EMEAN=EMEAN,
         EMEDIAN=EMED,
         OBCI=c(LCI1,UCI1),
         MLE='Note: MLE Constraint is not satisfied '
    )
  }
  else{
    list(k=k,
         n=n,
         EMEAN=EMEAN,
         EMEDIAN=EMED,
         OBCI=c(LCI1,UCI1)
    )
  }

}
