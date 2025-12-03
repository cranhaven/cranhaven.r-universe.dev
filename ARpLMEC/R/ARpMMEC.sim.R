#' @title Generating Censored Autoregressive Dataset with Mixed Effects, for normal distribution.
#' @import TruncatedNormal
#' @import LaplacesDemon
#' @import mnormt
#' @import numDeriv
#' @import tcltk
#' @import stats
#' @import relliptical
#' @description This function simulates a censored response variable with autoregressive errors of order \code{p}, with mixed effect and a established censoring rate. This function returns the censoring vector and censored response vector. 
#' @param m Number of individuals
#' @param x Design matrix of the fixed effects of order \code{n x s}, corresponding to vector of fixed effects.  
#' @param z Design matrix of the random effects of order\code{n x b}, corresponding to vector of random effects. 
#' @param tt Vector \code{1 x n} with the time the measurements were made, where \code{n} is the total number of measurements for all individuals.
#' @param nj Vector \code{1 x m} with the number of observations for each subject,  where \code{m} is the total number of individuals.
#' @param beta Vector of values fixed effects.
#' @param sigmae It's the value for sigma. 
#' @param D Covariance Matrix for the random effects.
#' @param phi Vector of length \code{Arp}, of values for autoregressive parameters. 
#' @param struc  Correlation structure. This must be one of \code{UNC},\code{ARp},\code{DEC},\code{SYM} or \code{DEC(AR)}.
#' @param order Order of the autoregressive process. Must be a positive integer value.
#' @param typeModel \code{Normal} for Normal distribution and \code{Student} for t-Student distribution. Default is \code{Normal}
#' @param p.cens Censoring percentage for the process. Default is \code{NULL}
#' @param n.cens Censoring level for the process. Default is \code{NULL}
#' @param cens.type \code{left} for left censoring, \code{right} for right censoring and \code{interval} for intervalar censoring. Default is \code{left}
#' @param nu degrees of freedom for t-Student distibution (nu > 0, maybe non-integer). 
#' @return returns list:
#' \item{cc}{Vector of censoring indicators.}
#' \item{y_cc}{Vector of responses censoring.}
#' @examples
#' \dontrun{
#'p.cens   = 0.1
#'m           = 10
#'D = matrix(c(0.049,0.001,0.001,0.002),2,2)
#'sigma2 = 0.30
#'phi    = 0.6
#'beta   = c(1,2,1)
#'nj=rep(4,10)
#'tt=rep(1:4,length(nj))
#'x<-matrix(runif(sum(nj)*length(beta),-1,1),sum(nj),length(beta))
#'z<-matrix(runif(sum(nj)*dim(D)[1],-1,1),sum(nj),dim(D)[1])
#'data=ARpMMEC.sim(m,x,z,tt,nj,beta,sigma2,D,phi,struc="ARp",typeModel="Normal",p.cens=p.cens)
#'  y<-data$y_cc
#'  cc<-data$cc
#' }
#' @export
ARpMMEC.sim=function(m,x=NULL,z=NULL,tt=NULL,nj,beta,sigmae,D,phi,struc="ARp",order=1,typeModel="Normal",p.cens= NULL,n.cens= NULL,cens.type="left",nu=NULL)
{
  
  if(m==sum(nj))                        stop("not compatible sizes between m and nj")
  
  if(!is.null(x)){
    if(!is.numeric(x))                    stop("x must be a numeric matrix. Check documentation!")
    if(sum(is.na(x))>0)                   stop("There are some NA values in x.")
    if(!is.matrix(x))                     stop("x must be a matrix. Check documentation!")
    if(det(t(x)%*%x)==0)                  stop("the columns of x must be linearly independent.")
    if(length(x)==0)                      stop("The parameter x must be provided.")
    if(dim(x)[1]!=dim(z)[1])              stop("not compatible sizes between x and z")
    if(dim(x)[1]!=sum(nj))                stop("not compatible sizes between x and nj")
    if(dim(x)[2]!=length(beta))           stop("not compatible sizes between x and beta")
    if(dim(x)[1]!=length(tt))             stop("not compatible sizes between x and tt")
    
  }
  
  if(!is.null(z)){
    if(!is.numeric(z))                    stop("z must be a numeric matrix. Check documentation!")
    if(sum(is.na(z))>0)                   stop("There are some NA values in z.")
    if(!is.matrix(z))                     stop("z must be a matrix. Check documentation!")
    if(length(z)==0)                      stop("The parameter z must be provided.")
    if(dim(z)[1]!=sum(nj))                stop("not compatible sizes between z and nj")
    if(dim(z)[2]!=dim(D)[2])             stop("not compatible sizes between z and D")
  }
  
  if(!is.numeric(nj))                   stop("nj must be a numeric vector. Check documentation!")
  if(!is.vector(nj))                    stop("nj must be a vector. Check documentation!")
  if(sum(is.na(nj))>0)                  stop("There are some NA values in nj")
  if(length(nj)==0)                     stop("The parameter nj must be provided.")
  
  if(!is.numeric(beta))             stop("beta must be a numeric vector. Check documentation!")
  if(!is.vector(beta))              stop("beta must be a vector. Check documentation!")
  if(!is.numeric(sigmae))           stop("sigmae must be a scalar.")
  if(length(sigmae)>1)              stop("beta must be a scalar.")
  if(!is.matrix(D))                stop("D must be a matrix.")
  if(D[upper.tri(D)]!=D[lower.tri(D)])stop("D must be a simetric matrix.")
  if(!is.numeric(phi))              stop("phi must be a numeric vector. Check documentation!")

  if(cens.type!="left" & cens.type!="right" & cens.type!="interval")stop('cens.type must be left, right or interval. Check documentation!')
  if(typeModel!='Normal'& typeModel!='Student')   stop('typeModel must be Normal or Student. Check documentation!')
  
  if(struc!="DEC"&struc!="DEC(AR)"&struc!="SYM"&struc!="ARp"&struc!="UNC") stop("Struc must be UNC, DEC, DEC(AR), SYM or ARp. Check documentation!")
  if(!is.null(p.cens)){
  if(p.cens>1| p.cens<0)       stop("the p.cens must be between 0 and 1 . Check documentation!")
  if(!is.numeric(p.cens))                   stop("p.cens must be a numeric. Check documentation!")
  }
  if(!is.null(n.cens)){
    if(!is.null(p.cens))         stop("For the censoring only need the parameter n.cens or p.cents. Please, choose to specify n.cens or p.cens. Check documentation!")
    if(!is.numeric(n.cens))                   stop("n.cens must be a numeric. Check documentation!")
  }
  if(!is.null(nu)){ 
    if(!is.numeric(nu))                   stop("nu must be a numeric. Check documentation!")}
  
  if(struc=="ARp"){ 
    if(length(phi)!=order)                   stop("not compatible information between phi and order. Check documentation!")}
  
  MMsimu(m=m,x=x,z=z,tt=tt,nj=nj,beta=beta,sigmae=sigmae,D=D,phi=phi,struc=struc,typeModel=typeModel,percCensu=p.cens,nivel.Censu=n.cens,cens.type=cens.type,nu=nu)
  
}

