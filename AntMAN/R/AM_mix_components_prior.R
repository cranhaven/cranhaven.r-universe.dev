#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################

#' S3 class AM_mix_components_prior
#' @description Object returned by \code{AM_mix_components_prior_*}. 
#' @seealso \code{\link{AM_mix_components_prior_dirac}},
#'          \code{\link{AM_mix_components_prior_negbin}},
#'          \code{\link{AM_mix_components_prior_pois}}
#' @name AM_mix_components_prior
#' @return \code{\link{AM_mix_components_prior}}
NULL




#'  summary information of the AM_mix_components_prior object
#'  
#'
#' Given an \code{\link{AM_mix_components_prior}} object, this function prints the summary information
#' of the specified prior on the number of components. 
#'  
#'@param object an \code{\link{AM_mix_components_prior}} object.
#'@param ... all additional parameters are ignored.
#'@return NULL. Called for side effects.
#'  
#'@importFrom utils head
#'@seealso \code{\link{AM_mix_components_prior}}
#'@method summary AM_mix_components_prior 
#'@export
summary.AM_mix_components_prior = function(object, ...){
	cat("\n", "AM_mix_components_prior\n", sep = "")	
	for (item in names(object)) {
		cat(' -', item , ': ' , head(object[[item]]), "\n")
	}
}




#################################################################################
##### Prior on Number of Components Configuration functions
#################################################################################


#' Generate a configuration object that contains a Point mass prior
#' 
#' Generate a configuration object that assigns a Point mass prior to the number of mixture components.
#' This is the simplest option and it requires users to specify a value \eqn{M^*} 
#' such that \eqn{Pr(M=M^* =1}. 
#'
#'@param Mstar   Fixed value  \eqn{M^*} for the number of components. 
#'@return An \code{\link{AM_mix_components_prior}} object. This is a configuration list to be used as \code{mix_components_prior} argument for \code{\link{AM_mcmc_fit}}. 
#'
#'@keywords prior
#'@seealso \code{\link{AM_mcmc_fit}}
#'@export
#' 
#'@examples
#' 
#' AM_mix_components_prior_dirac (Mstar=3)
AM_mix_components_prior_dirac <- function(Mstar) {
	
	parameters = list(type = "AM_mix_components_prior_dirac", Mstar = Mstar);
	
	return ( structure(
					parameters, 
					class = "AM_mix_components_prior")
	) ;
};




#' Generate a configuration object for a Shifted Negative Binomial prior on the number of mixture components
#'
#' 
#' This generates a configuration object for a Shifted Negative Binomial prior on the number of mixture components such that 
#'  \deqn{q_M(m)=Pr(M=m) =\frac{\Gamma(r+m-1)}{(m-1)!\Gamma(r)} p^{m-1}(1-p)^r, \quad m=1,2,3,\ldots}
#' The hyperparameters \eqn{p\in (0,1)}  (probability of success) and \eqn{r>0} (size) can either be fixed using \code{r} and \code{p}
#' or assigned appropriate prior distributions. 
#' In the latter case, we assume \eqn{p \sim Beta(a_P,b_P)} and \eqn{r \sim  Gamma(a_R,b_R)}. In AntMAN we assume the following 
#' parametrization of the Gamma density: 
#' \deqn{p(x\mid a,b )= \frac{b^a x^{a-1}}{\Gamma(a)} \exp\{ -bx \}, \quad x>0.}
#' 
#' 
#' If no arguments are provided, the default is \eqn{r = 1 , a_P = 1, b_P = 1}.
#' 
#' Additionally, when init_R and init_P are not specified, there are default values: 
#' \eqn{init_R = 1} and \eqn{init_P = 0.5}.
#'
#'@param a_R      The shape parameter \eqn{a}  of the \eqn{Gamma(a,b)} prior distribution for \eqn{r}.
#'@param b_R      The  rate parameter \eqn{b} of the \eqn{Gamma(a,b)} prior distribution for \eqn{r}.
#'@param init_R   The initial value of \eqn{r}, when specifying \code{a_R} and \code{b_R}.
#'@param a_P      The parameter \eqn{a}  of the \eqn{Beta(a,b)} prior distribution for \eqn{p}.
#'@param b_P      The parameter \eqn{b}  of the \eqn{Beta(a,b)} prior distribution for \eqn{p}.
#'@param init_P   The inivial  value of \eqn{p}, when specifying \code{a_P} and \code{b_P}.
#'@param R        It allows  to fix  \eqn{r} to a specific value.
#'@param P        It allows  to fix  \eqn{p} to a specific value.
#'
#'@return An \code{\link{AM_mix_components_prior}} object. This is a configuration list to be used as \code{mix_components_prior} argument for \code{\link{AM_mcmc_fit}}. 
#'
#'
#'@keywords prior
#'@seealso \code{\link{AM_mcmc_fit}}
#'@export
#' 
#'@examples
#' 
#' AM_mix_components_prior_negbin (R=1, P=1)
#' AM_mix_components_prior_negbin ()

AM_mix_components_prior_negbin <- function(a_R = NULL, b_R = NULL, a_P = NULL, b_P = NULL, R = NULL, P = NULL, 
		init_R = NULL, init_P = NULL) {
	
	paradox_error_R = "Please note that you cannot specify a_R,b_R and R. R specifies a fixed value.";
	paradox_error_P = "Please note that you cannot specify a_P,b_P and P. P specifies a fixed value.";
	
	parameters = list(type = "AM_mix_components_prior_negbin");
	
	if (!is.null(a_R) & !is.null(b_R) & !is.null(init_R) &  is.null(R)) {parameters = c(parameters, list(a_R = a_R, b_R = b_R, init_R = init_R));}
	else if (!is.null(a_R) & !is.null(b_R) &  is.null(init_R) &  is.null(R)) {parameters = c(parameters, list(a_R = a_R, b_R = b_R                 ));}
	else if ( is.null(a_R) &  is.null(b_R) &  is.null(init_R) & !is.null(R)) {parameters = c(parameters, list(fixed_R = R));}
	else if ( is.null(a_R) &  is.null(b_R) &  is.null(init_R) &  is.null(R)) {parameters = c(parameters, list(fixed_R = 1));}
	else {stop ( paradox_error_R );}
	
	if (!is.null(a_P) & !is.null(b_P) & !is.null(init_P) &  is.null(P)) {parameters = c(parameters, list(a_P = a_P, b_P = b_P, init_P = init_P));}
	else if (!is.null(a_P) & !is.null(b_P) &  is.null(init_P) &  is.null(P)) {parameters = c(parameters, list(a_P = a_P, b_P = b_P                 ));}
	else if ( is.null(a_P) &  is.null(b_P) &  is.null(init_P) & !is.null(P)) {parameters = c(parameters, list(fixed_P = P));}
	else if ( is.null(a_P) &  is.null(b_P) &  is.null(init_P) &  is.null(P)) {parameters = c(parameters, list(a_P = 1, b_P = 1));}
	else {stop ( paradox_error_P );}
	
	return ( structure(
					parameters, 
					class = "AM_mix_components_prior")
	) ;
};




#' Generate a configuration object for a Poisson prior on the number of mixture components
#' 
#'
#' This function generates a configuration object for a Shifted Poisson prior on the number 
#' of mixture components such that  
#' \deqn{q_M(m)=     Pr (M=m)= \frac{e^{-\Lambda}\Lambda^{m-1} }{(m-1)!}    ,      \quad m=1,2,3,\ldots}
#' The hyperparameter \eqn{\Lambda} can either be fixed using \code{Lambda} 
#' or assigned a \eqn{Gamma(a,b)} prior distribution with \code{a} and \code{b}.
#' In AntMAN we assume the following parametrization of the Gamma density: 
#' \deqn{p(x\mid a,b )= \frac{b^a x^{a-1}}{\Gamma(a)} \exp\{ -bx \}, \quad x>0.}
#' 
#' If no arguments are provided, the default is a prior distribution with \code{a = 1} and \code{b = 1}.
#'
#'@param a      The shape parameter \code{a} of  the \eqn{Gamma(a,b)} prior distribution.
#'@param b      The rate  parameter \code{b} of the \eqn{Gamma(a,b)} prior distribution.
#'@param init   The  initial value for \eqn{\Lambda}, when specifying \code{a} and \code{b}.
#'@param Lambda It allows to set the  hyperparameter \eqn{\Lambda} to be assigned a fixed value.
#'
#'@return An \code{\link{AM_mix_components_prior}} object. This is a configuration list to be used as \code{mix_components_prior} argument for \code{\link{AM_mcmc_fit}}. 
#'
#'@keywords prior
#'@seealso \code{\link{AM_mcmc_fit}}
#'@export
#' 
#'@examples
#' 
#' components_prior = AM_mix_components_prior_pois (init=3,  a=1, b=1) 
#' 

AM_mix_components_prior_pois <- function(a=NULL, b=NULL, Lambda=NULL, init=NULL) {
	
	paradox_error = "Please note that you cannot specify a,b,init and Lambda. Lambda specifies a fixed value.";
	
	### Default value ###
	parameters = list(type = "AM_mix_components_prior_pois",  a = 1, b = 1);
	
	if (!is.null(a) & !is.null(b)) {
		parameters = list(type = "AM_mix_components_prior_pois",  a = a, b = b);
		if (!is.null(init)) {
			parameters = list(type = "AM_mix_components_prior_pois",  a = a, b = b, init = init)
		};
		
		if (!is.null(Lambda)) {
			stop ( paradox_error );
		};
		
	} else if (!is.null(Lambda)) {
		parameters = list(type = "AM_mix_components_prior_pois", Lambda = Lambda);
		
		if (!is.null(a) | !is.null(b) | !is.null(init)) {
			stop ( paradox_error );
		}
		
	} else {
		if (!is.null(a) | !is.null(b) | !is.null(init)) {
			stop ( paradox_error );
		}
	};
	
	return ( structure(
					parameters, 
					class = "AM_mix_components_prior")
	) ;
};
