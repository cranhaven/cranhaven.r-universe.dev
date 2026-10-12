#' Marginal Density of \eqn{\beta}
#' 
#' This function computes the marginal density of \eqn{\beta} and for \eqn{\beta} on an equidistant grid specified by the user.
#' Currently only implemented for \eqn{dim(\beta)=1,2}.



#' 
#' @param D dimension of \eqn{\beta}. 
#' @param rangebeta a vector containing the start and ending point of \eqn{\beta} to be computed for.  
#' @param ngridbeta the number of grid values. 
#' @param a shape parameter of inverse gamma prior of \eqn{\psi^2}. 
#' @param b scale parameter of inverse gamma prior of \eqn{\psi^2}. 
#' @param r the scaling parameter \eqn{r(\delta=1)} in the variance \eqn{r(\delta)\psi^2} of prior of \eqn{\tau^2}. 
#' @param a0 shape parameter of beta prior of \eqn{\omega}. 
#' @param b0 scale parameter of beta prior of \eqn{\omega}. 
#' @param plot logical value (default is \code{FALSE}). If \code{TRUE}, a plot is also returned as the function \code{pl()}. 
#' @param log logical value (default is \code{FALSE}). If \code{TRUE}, \eqn{log(p(\beta))} is also returned in \code{logval}.
#' 						as well as, if necessary, a plot function \code{logpl()}.
#' @return the marginal density, the sequence of \eqn{\beta} and depending on specified \code{plot}, \code{log} arguments also the log-density and plot functions.
#' @author Nadja Klein
#' @references Nadja Klein, Thomas Kneib, Stefan Lang and Helga Wagner (2016). Spike and Slab Priors for Effect Selection in Distributional Regression. 
#' \emph{Working Paper}.
#' 
#' @import splines
#' @import pscl
#' @import mvtnorm
#' @export
#' @examples
#' set.seed(123)
#' #1-dimensional example
#' D = 1
#' ngridbeta = 1000
#' rangebeta = c(0.000001,1)
#' a0 = b0 = 0.5
#' a = 5
#' b = 50
#' r = 0.005
#' mdf <- mdbeta(D=1,rangebeta,ngridbeta,a=a,b=b,r=r,a0=a0,b0=b0) 
#'
#' #2-dimensional example
#' D = 2
#' ngridbeta = 100
#' rangebeta = c(0.000001,8)
#' a0 = b0 = 0.5
#' a = 5
#' b = 50
#' r = 0.005
#' mdf <- mdbeta(D=2,rangebeta,ngridbeta,a=a,b=b,r=r,a0=a0,b0=b0,plot=TRUE,log=TRUE) 
#' mdf$logpl()
#'

mdbeta <- function(D=1,rangebeta,ngridbeta,a=5,b=25,r=0.00025,a0=0.5,b0=0.5,plot=FALSE,log=FALSE) 
  {
	if(D==1)
		{
		mbetas = seq(rangebeta[1],rangebeta[2],length=ngridbeta)
		res = rep(NA,ngridbeta)
		funcintegrated0 <- function(tau,betai)
			{
			C = b^a*gamma(a+0.5)/((2*pi)^(D/2+0.5)*gamma(a))
			ret = C*(b+0.5*tau^2/r)^(-a-0.5)/abs(tau)*exp(-betai*betai/tau^2)
			return((b0/(a0+b0))*ret)
			}
		funcintegrated1 <- function(tau,betai)
			{
			C = b^a*gamma(a+0.5)/((2*pi)^(D/2+0.5)*gamma(a))
			ret = C*(b+0.5*tau^2)^(-a-0.5)/abs(tau)*exp(-betai*betai/tau^2)
			return((a0/(a0+b0))*ret)
			}
		for(betai1 in 1:ngridbeta)
			{
			temp1 = integrate(funcintegrated0,0,Inf,betai=mbetas[betai1])$val
			temp2	=	integrate(funcintegrated1,0,Inf,betai=mbetas[betai1])$val
			res[betai1] = temp1+temp2
			rm(temp1,temp2)
			}
		if(plot && !log) {
			pl <- function() {plot(mbetas,res,type="l",xlab=expression(paste(beta,sep="")),ylab=expression(paste(p(beta),sep="")),
						main=expression(paste("Marginal density ",p(beta),sep=""))) 	}
			return(list(betas=mbetas,val=res,pl=pl))
			} else if (plot && log) {
			pl <- function() {plot(mbetas,res,type="l",xlab=expression(paste(beta,sep="")),ylab=expression(paste(p(beta),sep="")),
						main=expression(paste("Marginal density ",p(beta),sep=""))) 	}
			logpl <- function() {plot(mbetas,logres,type="l",xlab=expression(paste(beta,sep="")),ylab=expression(paste(log(p(beta)),sep="")),
						main=expression(paste("Log-marginal density ",log(p(beta)),sep=""))) 	}			
			logres = log(res)
			return(list(betas=mbetas,val=res,pl=pl,logval=logres,logpl=logpl))
			} else if (!plot && log) {
			logres = log(res)
			return(list(betas=mbetas,val=res,logval=logres))
			} else {
				return(list(betas=mbetas,val=res))}	
		
		} else if(D==2) {
		
		betas = seq(rangebeta[1],rangebeta[2],length=ngridbeta)
		mbetas = expand.grid(betas,betas)
		mbetas = matrix(as.data.frame(t(mbetas)),nrow=ngridbeta)
		res = matrix(NA,nrow=dim(mbetas)[1],ncol=dim(mbetas)[2])
		funcintegrated0 <- function(tau,betai)
			{
			C = b^a*gamma(a+0.5)/((2*pi)^(D/2+0.5)*gamma(a))
			ret = C*(b+0.5*tau^2/r)^(-a-0.5)/abs(tau)*exp(-sum(betai*betai)/tau^2)
			return((b0/(a0+b0))*ret)
			}
		funcintegrated1 <- function(tau,betai)
			{
			C = b^a*gamma(a+0.5)/((2*pi)^(D/2+0.5)*gamma(a))
			ret = C*(b+0.5*tau^2)^(-a-0.5)/abs(tau)*exp(-sum(betai*betai)/tau^2)
			return((a0/(a0+b0))*ret)
			}
		for(rowi in 1:ngridbeta)
			{
			for(coli in 1:ngridbeta)
			  {
				temp1 = integrate(funcintegrated0,0,Inf,betai=mbetas[rowi,coli][[1]])$val
				temp2	=	integrate(funcintegrated1,0,Inf,betai=mbetas[rowi,coli][[1]])$val
				res[rowi,coli] = temp1+temp2
				rm(temp1,temp2)
				}
			}
		if(plot && !log) {
			pl <- function() {contour(betas,betas,res,xlab=expression(paste(beta[1],sep="")),ylab=expression(paste(beta[2],sep="")),
						main=expression(paste("Marginal density ",p(beta[1],beta[2]),sep="")),drawlabels=FALSE) 	}
			return(list(betas=betas,val=res,pl=pl))
			} else if (plot && log) {
			pl <- function() {contour(betas,betas,res,xlab=expression(paste(beta[1],sep="")),ylab=expression(paste(beta[2],sep="")),
						main=expression(paste("Marginal density ",p(beta[1],beta[2]),sep="")),drawlabels=FALSE) 	}
			logpl <- function() {contour(betas,betas,log(res),xlab=expression(paste(beta[1],sep="")),ylab=expression(paste(beta[2],sep="")),
						main=expression(paste("Log-marginal density ",log(p(beta[1],beta[2])),sep="")),drawlabels=FALSE) 	}			
			logres = log(res)
			return(list(betas=betas,val=res,pl=pl,logval=logres,logpl=logpl))
			} else if (!plot && log) {
			logres = log(res)
			return(list(betas=betas,val=res,logval=logres))
			} else {
			return(list(betas=betas,val=res))}	
			
	  } else {
		stop("D has to be either 1 or 2.")
		}
  }
