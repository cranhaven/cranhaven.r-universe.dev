#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################

#################################################################################
##### Raffaele Functions
#################################################################################

#' Compute the logarithm of the absolute value of the  generalized Sriling number of second Kind (mi pare) See charambeloides, using a recursive formula Devo vedere la formula
#' 
#' There are no default values.
#'
#' @param n      The sample size
#' @param gamma    A positive real number \code{gamma} 
#'
#' @return A vector of length n, reporting the values \code{C(gamma,n,k)} for \code{k=1,...,n}
#'
#' @keywords internal
#'
IAM_compute_stirling_ricor_abs <- function (n,gamma) {
	return(compute_stirling_ricor_abs(n,gamma));
}


#' Compute stirling ricor log
#' 
#' There are no default values.
#'
#' @param n      The sample size
#' @param gamma    A positive real number \code{gamma} 
#'
#' @return A vector of length n, reporting the values ... for \code{k=1,...,n}
#'
#' @keywords internal

IAM_compute_stirling_ricor_log<- function (n,gamma) {
	return(compute_stirling_ricor_log(n,gamma));
}

#' Compute the value V(n,k), needed to caclulate the eppf of a Finite Dirichlet process when the prior on the component-weigts of the mixture is a Dirichlet with parameter \code{gamma} (i.e. when unnormailized weights are distributed as Gamma(\eqn{\gamma},1) ) when the prior on the number of componet is Shifted Poisson of parameter \code{Lambda}. See Section 9.1.1 of the Paper Argiento de Iorio 2019.
#' 
#' There are no default values.
#'
#' @param n        The sample size
#' @param Lambda   The \code{Lambda} parameter of the Poisson
#' @param gamma    The \code{gamma} parameter of the Dirichlet 
#'
#' @return A vector of length n, reporting the values \code{V(n,k)} for \code{k=1,...,n}
#'
#' @keywords internal
#'
IAM_VnkPoisson <- function (n,Lambda,gamma) {
	return(VnkPoisson(n,Lambda,gamma));
}


#' Compute the value V(n,k), needed to caclulate the eppf of a Finite Dirichlet process when the prior on the component-weigts of the mixture is a Dirichlet with parameter \code{gamma} (i.e. when unnormailized weights are distributed as Gamma(\eqn{\gamma},1) ) when the prior on the number of componet is Negative Binomial with parameter \code{r} and \code{p}with  mean is mu =1+ r*p/(1-p) TODO: CHECK THIS FORMULA!!!. See Section 9.1.1 of the Paper Argiento de Iorio 2019 for more details
#' 
#' There are no default values.
#'
#' @param n      The sample size
#' @param r      The dispersion parameter \code{r} of Negative Binomial
#' @param p      The probability of failure parameter \code{p} of Negative Binomial
#' @param gam    The \code{gamma} parameter of the Dirichlet 
#'
#' @return A vector of length n, reporting the values \code{V(n,k)} for \code{k=1,...,n}
#'
#' @keywords internal
#'
IAM_VnkNegBin <- function (n,r,p,gam) {
	return(VnkNegBin(n,r,p,gam));
}


#' Compute the value V(n,k), needed to caclulate the eppf of a Finite Dirichlet process when the prior on the component-weigts
#'  of the mixture is a Dirichlet with parameter \code{gamma} (i.e. when unnormailized weights are distributed as Gamma(\eqn{\gamma},1) )
#'   when the number of component are fixed to \code{M^*}, i.e. a Dirac prior assigning mass only to \code{M^*} is assumed. 
#'   See Section 9.1.1 of the Paper Argiento de Iorio 2019 for more details.
#' 
#' There are no default values.
#'
#' @param n      The sample size
#' @param Mstar  The number of component of the mixture 
#' @param gamma    The \code{gamma} parameter of the Dirichlet 
#'
#' @return A vector of length n, reporting the values \code{V(n,k)} for \code{k=1,...,n}
#'
#' @keywords internal
#'
IAM_VnkDelta <- function (n,Mstar,gamma) {
	return(VnkDelta(n,Mstar,gamma));
}



#' IAM_mcmc_neff MCMC Parameters
#' 
#' TBD
#' 
#'@keywords internal
#'@param  unichain  
#'@return Effective Sample Size
IAM_mcmc_neff <- function(unichain){ # Using 11.5 of Bayesian Data Analysis
	

	
	# Dunson Ventari Rubin Section 11.5
	# Whit rho estimated using the acf function of R
	G <- length(unichain)
	
	rho=acf(unichain,lag.max=G, plot=FALSE)$acf
	
	differenza  <- rho[1:(G-1)]+rho[2:(G)]
	wh_neg=which(differenza<0)
	if (length(wh_neg) == 0) {
		return (NaN);
	}
	TT=min(wh_neg)
	
	if( TT%%2!=1){TT= TT+1}
	denom=1+2*sum(rho[2:(TT)])
	
	return(G/denom)
}

#' Internal function used to compute the MCMC Error as a batch mean.
#' 
#'@keywords internal
#'@param  X is a chain
#'@return the MCMC Error (sqrt(sigma2) / sqrt(N))
IAM_mcmc_error <- function(X){
	N <- length(X)
	b   = max(1, round(sqrt(N)))
	a   = ceiling(N/b)
	m   = matrix(c(X,rep(0,a*b - N)) , nrow = a, ncol=b) 
	count_matrix =  matrix(c(rep(1,N),rep(0,a*b - N)), nrow = a, ncol=b)
	count_row = rowSums(count_matrix)
	Yks = rowSums(m) / count_row # NA
	mu = mean(X) 
	sigma2 = (b / (a - 1)) * sum((Yks - mu)**2) # NA
	return ( sqrt(sigma2) / sqrt(N) ) ; # NA
}
