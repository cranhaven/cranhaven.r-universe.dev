#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################

#' S3 class AM_mix_hyperparams
#' @description Object type returned by \code{AM_mix_hyperparams_*} commands.
#' @seealso \code{\link{AM_mix_hyperparams_unipois}}, \code{\link{AM_mix_hyperparams_uninorm}}, \code{\link{AM_mix_hyperparams_multiber}},
#' \code{\link{AM_mix_hyperparams_multinorm}}
#' @name AM_mix_hyperparams
#' @return \code{\link{AM_mix_hyperparams}}
NULL



#' summary information of the AM_mix_hyperparams object 
#'  
#'
#' Given an \code{\link{AM_mix_hyperparams}} object, this function prints the summary information
#' of the specified mixture hyperparameters. 
#'  
#'@param object an \code{\link{AM_mix_hyperparams}} object.
#'@param ... all additional parameters are ignored.
#'@return NULL. Called for side effects.
#'  
#'@method summary AM_mix_hyperparams 
#'@seealso \code{\link{AM_mix_hyperparams}}
#'@importFrom utils head
#'@export
summary.AM_mix_hyperparams = function(object, ...){
	cat("\n", "AM_mix_hyperparams\n", sep = "")	
	for (item in names(object)) {
		cat(' -', item , ': ' , head(object[[item]]), "\n")
	}
}




#################################################################################
##### Mixture Kernel Hyperparameters.
#################################################################################


#'univariate Poisson mixture hyperparameters
#'
#' Generate a configuration object that specifies a univariate Poisson mixture kernel, where users can
#' specify the hyperparameters of the conjugate Gamma prior, i.e. the kernel is a \eqn{Poisson(\tau) }
#' and \eqn{\tau\sim Gamma(\alpha_0,\beta_0)}. 
#' In AntMAN we assume the following 
#' parametrization of the Gamma density: 
#' \deqn{p(x\mid a,b )= \frac{b^a x^{a-1}}{\Gamma(a)} \exp\{ -bx \}, \quad x>0.}
#'
#' Note that by default, alpha0=1 and beta0=1.
#'
#'
#'
#'@param alpha0       The shape  hyperparameter \eqn{\alpha_0}.
#'@param beta0        The  rate hyperparameter \eqn{\beta_0}.
#'@return An \code{\link{AM_mix_hyperparams}} object. This is a configuration list to be used as \code{mix_kernel_hyperparams} argument for \code{\link{AM_mcmc_fit}}.
#'@examples 
#' AM_mix_hyperparams_unipois (alpha0=2, beta0=0.2)
#'@export
AM_mix_hyperparams_unipois <- function(alpha0, beta0) {
	parameters = list ( type = "AM_mix_hyperparams_unipois", alpha0 = alpha0, beta0 = beta0 );
	return ( structure(
					parameters, 
					class = "AM_mix_hyperparams")
	) ;
}

#' univariate Normal mixture hyperparameters
#' 
#' Generate a configuration object that specifies a univariate Normal mixture kernel, where users can specify the hyperparameters of the Normal-InverseGamma conjugate prior. 
#' As such, the kernel is a Gaussian distribution with mean \eqn{\mu} and variance \eqn{\sigma^2}. The prior on \eqn{(\mu,\sigma^2)} the Normal-InverseGamma:
#' \deqn{\pi(\mu,\sigma^2\mid m_0,\kappa_0,\nu_0,\sigma^2_0) = \pi_{\mu}(\mu|\sigma^2,m_0,\kappa_0)\pi_{\sigma^2}(\sigma^2\mid \nu_0,\sigma^2_0),}
#'  \deqn{\pi_{\mu}(\mu|\sigma^2,m_0,\kappa_0)  =\frac{\sqrt{\kappa_0}}{\sqrt{2\pi\sigma^2},} 
#'  \exp^{-\frac{\kappa_0}{2\sigma^2}(\mu-m_0)^2 }, \qquad \mu\in\mathcal{R},}
#'  \deqn{\pi_{\sigma^2}(\sigma^2\mid \nu_0,\sigma^2_0)= {\frac {\sigma_0^{2^{\nu_0 }}}{\Gamma (\nu_0 )}}(1/\sigma^2)^{\nu_0 +1}\exp \left(-\frac{\sigma_0^2}{\sigma^2}\right), \qquad \sigma^2>0.}
#' 
#' 
#' 
#'  \eqn{m_0} corresponds \code{m0}, 
#'  \eqn{\kappa_0} corresponds \code{k0}, 
#'  \eqn{\nu_0} corresponds \code{nu0}, and 
#'  \eqn{\sigma^2_0} corresponds \code{sig02}. 
#' 
#'If hyperparameters are not specified, the default is \code{m0=0}, \code{k0=1}, \code{nu0=3}, \code{sig02=1}.
#'
#'@param m0      The \eqn{m_0} hyperparameter.
#'@param k0      The \eqn{\kappa_0} hyperparameter.
#'@param nu0     The \eqn{\nu_0} hyperparameter.
#'@param sig02   The \eqn{\sigma^2_0} hyperparameter.
#'@return An \code{\link{AM_mix_hyperparams}} object. This is a configuration list to be used as \code{mix_kernel_hyperparams} argument for \code{\link{AM_mcmc_fit}}.
#'@examples 
#'      
#'      #### This example ...
#'      
#'      data(galaxy)
#'      y_uvn = galaxy
#'      mixture_uvn_params = AM_mix_hyperparams_uninorm  (m0=20.83146, k0=0.3333333,
#'                                                        nu0=4.222222, sig02=3.661027)
#'      
#'      mcmc_params        = AM_mcmc_parameters(niter=2000, burnin=500, thin=10, verbose=0)
#'      components_prior   = AM_mix_components_prior_pois (init=3,  a=1, b=1) 
#'      weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)
#'      
#'      fit <- AM_mcmc_fit(
#'        y = y_uvn,
#'        mix_kernel_hyperparams = mixture_uvn_params,
#'        mix_components_prior =components_prior,
#'        mix_weight_prior = weights_prior,
#'        mcmc_parameters = mcmc_params)
#'      
#'      summary (fit)
#'      plot (fit)
#'      
#'@export
AM_mix_hyperparams_uninorm <- function(m0, k0, nu0, sig02) { 
	parameters = list ( type = "AM_mix_hyperparams_uninorm", m0 = m0 , k0 = k0 , nu0 = nu0 , sig02 = sig02 );
	return ( structure(
					parameters, 
					class = "AM_mix_hyperparams")
	) ;
}


#' multivariate Bernoulli mixture hyperparameters (Latent Class Analysis)
#' 
#'
#' Generate a configuration object that defines the prior hyperparameters for a mixture of multivariate Bernoulli.
#' If the dimension of the data is P, then the prior is a product of P independent Beta distributions, Beta(\eqn{a_{0i},b_{0i}}). Therefore,
#' the vectors of hyperparameters, a0 and b0,  are P-dimensional. Default is (a0= c(1,....,1),b0= c(1,....,1)).
#'
#'@param a0        The a0 hyperparameters.
#'@param b0        The b0 hyperparameters.
#'@return An \code{\link{AM_mix_hyperparams}} object. This is a configuration list to be used as \code{mix_kernel_hyperparams} argument for \code{\link{AM_mcmc_fit}}.
#'@examples 
#' AM_mix_hyperparams_multiber (a0= c(1,1,1,1),b0= c(1,1,1,1))
#'@export
AM_mix_hyperparams_multiber <- function(a0, b0) {

	parameters =  list ( type = "AM_mix_hyperparams_multiber", a0 = a0 , b0 = b0  );
	return ( structure(
					parameters, 
					class = "AM_mix_hyperparams")
	) ;
}




#' multivariate Normal mixture hyperparameters
#' 
#' 
#' Generate a configuration object that specifies a multivariate Normal mixture kernel, where users can specify the hyperparameters for the conjugate prior of the multivariate 
#' Normal mixture. We assume that the data are d-dimensional vectors \eqn{\boldsymbol{y}_i}, where \eqn{\boldsymbol{y}_i} are i.i.d 
#' Normal random variables with mean \eqn{\boldsymbol{\mu}} and covariance matrix \eqn{\boldsymbol{\Sigma}}.
#' The conjugate prior is 
#' \deqn{\pi(\boldsymbol \mu, \boldsymbol \Sigma\mid\boldsymbol m_0,\kappa_0,\nu_0,\boldsymbol \Lambda_0)= 
#' \pi_{\mu}(\boldsymbol \mu|\boldsymbol \Sigma,\boldsymbol m_0,\kappa_0)\pi_{\Sigma}(\boldsymbol \Sigma \mid \nu_0,\boldsymbol \Lambda_0),}
#'  \deqn{ \pi_{\mu}(\boldsymbol \mu|\boldsymbol \Sigma,\boldsymbol m_0,\kappa_0)  = 
#'  \frac{\sqrt{\kappa_0^d}}{\sqrt {(2\pi )^{d}|{\boldsymbol \Sigma }|}} \exp \left(-{\frac {\kappa_0}{2}}(\boldsymbol\mu -{\boldsymbol m_0 })^{\mathrm {T} }{\boldsymbol{\Sigma }}^{-1}(\boldsymbol\mu-{\boldsymbol m_0 })\right),
#' \qquad \boldsymbol \mu\in\mathcal{R}^d,}
#' \deqn{\pi_{\Sigma}(\boldsymbol \Sigma\mid \nu_0,\boldsymbol \Lambda_0)= {\frac {\left|{\boldsymbol \Lambda_0 }\right|^{\nu_0 /2}}{2^{\nu_0 d/2}\Gamma _{d}({\frac {\nu_0 }{2}})}}\left|\boldsymbol \Sigma \right|^{-(\nu_0 +d+1)/2}e^{-{\frac {1}{2}}\mathrm {tr} (\boldsymbol \Lambda_0 \boldsymbol \Sigma^{-1})}
#', \qquad \boldsymbol \Sigma^2>0,}
#' where \code{mu0} corresponds to \eqn{\boldsymbol m_0}, \code{ka0} corresponds to  \eqn{\kappa_0}, 
#' \code{nu0} to \eqn{\nu_0}, and \code{Lam0} to \eqn{\Lambda_0}.
#' 
#' Default is \code{(mu0=c(0,..,0)}, \code{ka0=1}, \code{nu0=Dim+2}, \code{Lam0=diag(Dim))} with \code{Dim} is the dimension of the data \code{y}.
#' We advise the user to set \eqn{\nu_0} equal to at least the dimension of the data, \code{Dim}, plus 2.
#'
#'@param mu0    The hyperparameter \eqn{\boldsymbol m_0}.
#'@param ka0    The hyperparameter \eqn{\kappa_0}.
#'@param nu0    The hyperparameter \eqn{\nu_0}.
#'@param Lam0   The hyperparameter \eqn{\Lambda_0}.
#'@return An \code{\link{AM_mix_hyperparams}} object. This is a configuration list to be used as \code{mix_kernel_hyperparams} argument for \code{\link{AM_mcmc_fit}}.
#'@examples 
#' AM_mix_hyperparams_multinorm ()
#'@export
AM_mix_hyperparams_multinorm <- function(mu0 = NULL, ka0 = NULL, nu0 = NULL, Lam0 = NULL) {
	parameters = list ( type = "AM_mix_hyperparams_multinorm", mu0 = mu0 , ka0 = ka0 , nu0 = nu0 , Lam0 = Lam0 );
	return ( structure(
					parameters, 
					class = "AM_mix_hyperparams")
	) ;
}


