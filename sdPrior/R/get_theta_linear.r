#' Find Scale Parameter for Inverse Gamma Hyperprior of Linear Effects with Spike and Slab Prior
#' 
#' This function implements a optimisation routine that computes the scale parameter \eqn{v_2} and selection parameter
#' \eqn{r} of the inverse gamma prior IG(\eqn{v_1},\eqn{v_2}) for \eqn{\tau^2} when \eqn{\tau^2\sim N(0,r(\delta)\tau^2)} 
#' and given shape paramter 
#' such that approximately \eqn{P(\beta\le c_2|spike)\ge 1-\alpha_2} and \eqn{P(\beta\ge c_1|slab)\ge 1-\alpha1}.\cr
#' \eqn{\alpha_1} and \eqn{\alpha_2} should not be smaller than 0.1 due to numerical sensitivity and possible instability. Better change \eqn{c_1}, \eqn{c_2}.		



#' 
#' @param alpha1 denotes the 1-\eqn{\alpha_1} level for \eqn{v_2}.  
#' @param alpha2 denotes the 1-\eqn{\alpha_2} level for \eqn{r}.  
#' @param c1 denotes the expected range of the linear effect in the slab part. 
#' @param c2 denotes the expected range of the linear effect in the spike part.
#' @param eps denotes the error tolerance of the result, default is \code{.Machine$double.eps}. 
#' @param v1 is the shape parameter of the inverse gamma distribution, default is 5.
#' @return an object of class \code{list} with values from \code{\link{uniroot}}.
#' @author Nadja Klein
#' @references Nadja Klein, Thomas Kneib, Stefan Lang and Helga Wagner (2016). Automatic Effect Selection in Distributional Regression via Spike and Slab Priors. 
#' \emph{Working Paper}.
#' @section Warning:
#' \eqn{\alpha_1} and \eqn{\alpha_2} should not be smaller than 0.1 due to numerical sensitivity and possible instability. Better change \eqn{c_1}, \eqn{c_2}.	
	
#' @import stats		
#' @export
#' @examples
#' set.seed(123)
#' result <- get_theta_linear()
#' r <- result$r
#' v2 <- result$v2
#'
#' get_theta_linear(alpha1=0.1,alpha2=0.1,c1=0.5,c2=0.1,v1=5) 
#'

get_theta_linear <- function(alpha1 = 0.1, alpha2=0.1, c1 = 0.1, c2 = 0.1, eps = .Machine$double.eps, v1 = 5) 
  {
  eps1 <- eps
  marginal_df1 <- function(f, v1, v2)
    {
	  df <- 2 * v1
	  mu <- 0
		sigma <- sqrt(v2/v1)
    res <- pt((f-mu) / sigma, df = df) #- pt((-f-mu) / sigma, df = df) 
    return(res)
    }
  
  
  marginal_Pf1 <- function(v2, alpha1)
    {
		contri <- marginal_df1(f=c1, v1 = v1,v2=v2)-marginal_df1(f= -c1, v1 = v1,v2=v2)
    contri-alpha1
    }

  result <- try(uniroot(marginal_Pf1, interval = c(0, 1/eps), alpha1 = alpha1), TRUE)
   
  while(inherits(result, "try-error")) 
    {
    eps1 <- eps1 * 100
		if(eps1>(1/eps))
			stop("No solution for given parameters found. Uniroot did not converge. Please try another set of parameter values.")
    result <- try(uniroot(marginal_Pf1, interval = c(eps1, 1/eps), alpha1 = alpha1), TRUE)
    } 
	ret <- list(v2=result$root)
	cat("Parameter v2 =",ret$v2," with function value ",result$f.root,".\n","The interval to be searched in was [",
				eps1,",",1/eps,"].\n\n",sep="")
	
	v2 <- ret$v2
	
	marginal_df2 <- function(f, v1, v2, r)
    {
	  df <- 2 * v1
	  mu <- 0
		sigma <- sqrt(v2*r/v1)
    res <- pt((f-mu) / sigma, df = df) #- pt((-f-mu) / sigma, df = df) 
    return(res)
    }
  marginal_Pf2 <- function(r, alpha2)
    {
		contri <- marginal_df2(f=c2, v1 = v1,v2=v2,r=r)-marginal_df2(f= -c2, v1 = v1,v2=v2,r=r)
    1 - alpha2 - contri
    }
	eps2 <- 100
  result <- try(uniroot(marginal_Pf2, interval = c(eps, 1), alpha2 = alpha2), TRUE)
  
  while(inherits(result, "try-error")) 
    {
    eps2 <- eps2 / 100
		if(eps>eps2)
			stop("No solution for given parameters found. Uniroot did not converge. Please try another set of parameter values.")
    result <- try(uniroot(marginal_Pf2, interval = c(eps, eps2), alpha2 = alpha2), TRUE)
    } 
	ret$r <- result$root
	cat("Parameter r =",ret$r," with function value ",result$f.root,".\n","The interval to be searched in was [",
				eps,",",eps2,"]\n\n",sep="")
  
  return(ret)
}
