#' Find Scale Parameter for Inverse Gamma Hyperprior of Linear Effects with Spike and Slab Prior
#' 
#' This function implements a optimisation routine that computes the scale parameter \eqn{b} and selection parameter
#' \eqn{r}. Here, we assume an inverse gamma prior IG(\eqn{a},\eqn{b}) for \eqn{\tau^2} and \eqn{\beta|\delta,\tau^2\sim N(0,r(\delta)\tau^2)}. 
#' For given shape paramter \eqn{a} the user gets \eqn{b}, \eqn{r}
#' such that approximately \eqn{P(\beta\le c2|spike)\ge 1-\alpha2} and \eqn{P(\beta\ge c1|slab)\ge 1-\alpha1} hold.\cr
#' Note that if you observe numerical instabilities try not to specify \eqn{\alpha1} and \eqn{\alpha2} smaller than 0.1.  



#' 
#' @param alpha1 denotes the 1-\eqn{\alpha1} level for \eqn{b}.  
#' @param alpha2 denotes the 1-\eqn{\alpha2} level for \eqn{r}.  
#' @param c1 denotes the expected range of the linear effect in the slab part. 
#' @param c2 denotes the expected range of the linear effect in the spike part.
#' @param eps denotes the error tolerance of the result, default is \code{.Machine$double.eps}. 
#' @param a is the shape parameter of the inverse gamma distribution, default is 5.
#' @return an object of class \code{list} with root values \eqn{r}, \eqn{b} from \code{\link{uniroot}}.
#' @author Nadja Klein
#' @references Nadja Klein, Thomas Kneib, Stefan Lang and Helga Wagner (2016). Automatic Effect Selection in Distributional Regression via Spike and Slab Priors. 
#' \emph{Working Paper}.
#' @section Warning:
#' \eqn{\alpha1} and \eqn{\alpha2} should not be smaller than 0.1 due to numerical sensitivity and possible instability. Better change \eqn{c1}, \eqn{c2}.	
	
#' @import stats	
#' @import pscl
#' @import mvtnorm	
#' @export
#' @examples
#' set.seed(123)
#' result <- hyperparlin()
#' r <- result$r
#' b <- result$b
#'
#' hyperparlin(alpha1=0.1,alpha2=0.1,c1=0.5,c2=0.1,a=5) 
#'

hyperparlin <- function(alpha1=0.1,alpha2=0.1,c1=0.1,c2=0.1,eps=.Machine$double.eps,a=5) 
  {
  eps1 <- eps
  marginal_df1 <- function(f,a,b)
    {
	  df <- 2 * a
	  mu <- 0
		sigma <- sqrt(b/a)
    res <- pt((f-mu) / sigma,df=df) #- pt((-f-mu) / sigma,df=df) 
    return(res)
    }
  
  
  marginal_Pf1 <- function(b,alpha1)
    {
		contri <- marginal_df1(f=c1,a=a,b=b)-marginal_df1(f= -c1,a=a,b=b)
    contri-alpha1
    }
	cat("Optimising b\n")
  (result <- try(uniroot(marginal_Pf1,interval=c(0,1/eps),alpha1=alpha1),TRUE))
   
  while(inherits(result,"try-error")) 
    {
    eps1 <- eps1 * 100
		if(eps1>(1/eps))
			stop("No solution for given parameters found. Uniroot did not converge. Please try another set of parameter values.")
    (result <- try(uniroot(marginal_Pf1,interval=c(eps1,1/eps),alpha1=alpha1),TRUE))
    } 
	ret <- list(b=result$root)
	cat("Parameter b =",ret$b," with function value ",result$f.root,".\n","The interval to be searched in was [",
				eps1,",",1/eps,"].\n\n",sep="")
	
	b <- ret$b
	
	marginal_df2 <- function(f,a,b,r)
    {
	  df <- 2 * a
	  mu <- 0
		sigma <- sqrt(b*r/a)
    res <- pt((f-mu) / sigma,df=df) #- pt((-f-mu) / sigma,df=df) 
    return(res)
    }
  marginal_Pf2 <- function(r,alpha2)
    {
		contri <- marginal_df2(f=c2,a=a,b=b,r=r)-marginal_df2(f=-c2,a=a,b=b,r=r)
    1 - alpha2 - contri
    }
	eps2 <- 100
	cat("Optimising r\n")
  (result <- try(uniroot(marginal_Pf2,interval=c(eps,1),alpha2=alpha2),TRUE))
  
  while(inherits(result,"try-error")) 
    {
    eps2 <- eps2 / 100
		if(eps>eps2)
			stop("No solution for given parameters found. Uniroot did not converge. Please try another set of parameter values.")
    result <- try(uniroot(marginal_Pf2,interval=c(eps,eps2),alpha2=alpha2),TRUE)
    } 
	ret$r <- result$root
	cat("Parameter r =",ret$r," with function value ",result$f.root,".\n","The interval to be searched in was [",
				eps,",",eps2,"]\n\n",sep="")
  
  return(ret)
}
