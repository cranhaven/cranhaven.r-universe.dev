#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################

#' S3 class AM_mix_weights_prior
#' @description Object type returned by \code{AM_mix_weights_prior_*} commands.
#' @seealso \code{\link{AM_mix_weights_prior_gamma}}
#' @name AM_mix_weights_prior
#' @return \code{\link{AM_mix_weights_prior}}
NULL


#################################################################################
##### Configuration functions for the prior on the mixture weights
#################################################################################

#'  summary information of the AM_mix_weights_prior object 
#'  
#'
#' Given an \code{\link{AM_mix_weights_prior}} object, this function prints the summary information
#' of the specified mixture weights prior. 
#'  
#'@param object an \code{\link{AM_mix_weights_prior}} object.
#'@param ... all additional parameters are ignored.
#'@return NULL. Called for side effects.
#'  
#'  
#'@method summary AM_mix_weights_prior 
#'@seealso \code{\link{AM_mix_weights_prior}}
#'@importFrom utils head
#'@export
summary.AM_mix_weights_prior = function(object, ...){
	cat("\n", "AM_mix_weights_prior\n", sep = "")	
	for (item in names(object)) {
		cat(' -', item , ': ' , head(object[[item]]), "\n")
	}
}



#' specify a prior on the hyperparameter \eqn{\gamma} for the Dirichlet mixture weights prior 
#'
#' 
#' Generate a configuration object to specify a prior on the hyperparameter \eqn{\gamma} for the Dirichlet prior on the 
#' mixture weights. 
#' We assume \eqn{\gamma \sim  Gamma(a,b)}. Alternatively, we can fix \eqn{\gamma} to a specific value.
#' Default is \eqn{\gamma=1/N}, where N is the number of observations. 
#'In AntMAN we assume the following 
#' parametrization of the Gamma density: 
#' \deqn{p(x\mid a,b )= \frac{b^a x^{a-1}}{\Gamma(a)} \exp\{ -bx \}, \quad x>0.}
#' 
#'@param a      The shape parameter a of the Gamma prior.
#'@param b      The rate parameter b of the Gamma prior.
#'@param init   The init value for \eqn{\gamma}, when we assume \eqn{\gamma} random.
#'@param gamma  It allows to fix \eqn{\gamma}  to a specific value.
#'
#'@return A \code{\link{AM_mix_weights_prior}} object. This is a configuration list to be used as \code{mix_weight_prior} argument for \code{\link{AM_mcmc_fit}}. 
#'
#'@examples 
#' AM_mix_weights_prior_gamma (a=1, b=1)
#' AM_mix_weights_prior_gamma (a=1, b=1, init=1)
#' AM_mix_weights_prior_gamma (gamma = 3)
#' AM_mix_weights_prior_gamma () 
#'@export
#'@keywords prior
AM_mix_weights_prior_gamma <- function(a = NULL, b = NULL, gamma = NULL, init = NULL) {
	
	paradox_error = "Please note that you cannot specify a,b,init and gamma. gamma specifies a fixed value.";
	
	parameters = list(type = "AM_mix_weights_prior_gamma");
	if (!is.null(a) & !is.null(b)) {
		parameters = list(type = "AM_mix_weights_prior_gamma",  a = a, b = b);
		if (!is.null(init)) {
			parameters = list(type = "AM_mix_weights_prior_gamma",  a = a, b = b, init = init)
		};
		
		if (!is.null(gamma)) {
			stop ( paradox_error );
		};
		
	} else if (!is.null(gamma)) {
		parameters = list(type = "AM_mix_weights_prior_gamma",  gamma = gamma);
		
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
					class = "AM_mix_weights_prior")
	) ;
	
};
