#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################

#' Returns an example of \code{\link{AM_mcmc_fit}} output produced by the multivariate bernoulli model
#'  
#'
#' This function allows us to generate a sample output of fitting the multivariate Bernoulli model. No arguments are needed to be passed.
#' The purpose of this function is to serve as a demo for users to understand the model's output, without diving too deep into details. By default,
#' this demo generates a sample dataset of dimension 500x4, where the MCMC sampler is specified to run for 2000 iterations, with a burn-in of 1000, and a thinning interval of 10. All possible outputs
#' that can be produced by \code{\link{AM_mcmc_fit}} are returned (see return value below).   
#'  
#' @return A list containing the following items:
#' \itemize{
#' \item the vector (or matrix) containing the synthetic data used to fit the model.
#' \item the vector containing the final cluster assignment of each observation.
#' \item an \code{\link{AM_mcmc_output}} object, which is the typical output of \code{\link{AM_mcmc_fit}}.
#' }
#'
#' @keywords demo

#' @export
#'
#' @examples
#' \donttest{
#'  mvb_output <- AM_demo_mvb_poi()
#' }
AM_demo_mvb_poi = function () {
	
	
	d <- 4
	k <- 3
	TH <- matrix(nrow=k,ncol=d)
	TH[1,] <- c(0.9,0.0,0.2,0.1)
	TH[2,] <- c(0.0,0.9,0.1,0.2)
	TH[3,] <- c(0.0,0.0,0.9,0.9)
	demo_multivariate_binomial <- AM_sample_multibin(n=500,d,c(0.3,0.3,0.4),TH)
	
	y_mvb  <- demo_multivariate_binomial$y
	ci_mvb <- demo_multivariate_binomial$ci
	
	mixture_mvb_params = AM_mix_hyperparams_multiber  (a0= c(1,1,1,1),b0= c(1,1,1,1))
	
	mcmc_params        = AM_mcmc_parameters(niter=2000, burnin=1000, thin=10, verbose=0, output=c("ALL"))
	components_prior   = AM_mix_components_prior_pois (init=3,  a=1, b=1) 
	weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)
	
	fit <- AM_mcmc_fit(
			y = y_mvb, 
			mix_kernel_hyperparams = mixture_mvb_params,
			mix_components_prior =components_prior,
			mix_weight_prior = weights_prior,
			mcmc_parameters = mcmc_params)
	
	return (list(input = y_mvb, clusters = ci_mvb, fit = fit))
}


#' Returns an example of \code{\link{AM_mcmc_fit}} output produced by the multivariate gaussian model
#'  
#'
#'This function allows us to generate a sample output of fitting the multivariate Gaussian model. No arguments are needed to be passed.
#' The purpose of this function is to serve as a demo for users to understand the model's output, without diving too deep into details. By default,
#' this demo generates a sample dataset of dimension 500x2, where the MCMC sampler is specified to run for 2000 iterations, with a burn-in of 1000, and a thinning interval of 10. All possible outputs
#' that can be produced by \code{\link{AM_mcmc_fit}} are returned (see return value below).
#'  
#' @return A list containing the following items:
#' \itemize{
#' \item the vector (or matrix) containing the synthetic data used to fit the model.
#' \item the vector containing the final cluster assignment of each observation.
#' \item an \code{\link{AM_mcmc_output}} object, which is the typical output of \code{\link{AM_mcmc_fit}}.
#' }
#'
#' @keywords demo

#' @export
#'
#' @examples
#' \donttest{
#'  mvn_output <- AM_demo_mvn_poi()
#' }
AM_demo_mvn_poi = function () {
	
	
	
	MU <- matrix(nrow=3,ncol=2)
	
	MU[1,] <- c(0,0)
	MU[2,] <- c(-3,-3)
	MU[3,] <- c(4,4)
	
	
	sig1 <- c(1,1)
	rho1 <- 0
	Sig1 <- matrix(c(sig1[1]^2,rho1*sig1[1]*sig1[2], rho1*sig1[1]*sig1[2],sig1[2]^2),byrow=TRUE,nrow=2) 
	
	sig2 <- c(1,1)
	rho2 <- -0.7
	Sig2 <- matrix(c(sig2[1]^2,rho2*sig2[1]*sig2[2], rho2*sig2[1]*sig2[2],sig2[2]^2),byrow=TRUE,nrow=2) 
	
	sig3 <- c(1,1)
	rho3 <- -0.3
	Sig3 <- matrix(c(sig3[1]^2,rho3*sig3[1]*sig3[2], rho3*sig3[1]*sig3[2],sig3[2]^2),byrow=TRUE,nrow=2) 
	
	
	SIG <- array(0,dim=c(3,2,2))
	SIG[1,,] <- Sig1
	SIG[2,,] <- Sig2
	SIG[3,,] <- Sig3
	
	
	
	demo_multivariate_normal <-AM_sample_multinorm(n = 500 ,d = 2,c(0.3,0.3,0.4),MU,SIG)
	y_mvn  <- demo_multivariate_normal$y
	ci_mvn <- demo_multivariate_normal$ci
	
	mixture_mvn_params = AM_mix_hyperparams_multinorm   (mu0=c(0,0),ka0=1,nu0=4,Lam0=diag(2))
	
	mcmc_params        = AM_mcmc_parameters(niter=2000, burnin=1000, thin=10, verbose=0, output=c("ALL"))
	components_prior   = AM_mix_components_prior_pois (init=3,  a=1, b=1) 
	weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)
	
	fit <- AM_mcmc_fit(
			y = y_mvn, 
			mix_kernel_hyperparams = mixture_mvn_params,
			mix_components_prior =components_prior,
			mix_weight_prior = weights_prior,
			mcmc_parameters = mcmc_params)
	
	
	
	return (list(input = y_mvn, clusters = ci_mvn, fit = fit))
}


##' Returns an example of \code{\link{AM_mcmc_fit}} output produced by the univariate Gaussian model
#'  
#'
#'This function allows us to generate a sample output of fitting the univariate gaussian model. No arguments are needed to be passed.
#' The purpose of this function is to serve as a demo for users to understand the model's output, without diving too deep into details. By default,
#' this demo generates a sample dataset of dimension 500x1, where the MCMC sampler is specified to run for 2000 iterations, with a burn-in of 1000, and a thinning interval of 10. All possible outputs
#' that can be produced by \code{\link{AM_mcmc_fit}} are returned (see return value below).
#'  
#' @return A list containing the following items:
#' \itemize{
#' \item the vector (or matrix) containing the synthetic data used to fit the model.
#' \item the vector containing the final cluster assignment of each observation.
#' \item an \code{\link{AM_mcmc_output}} object, which is the typical output of \code{\link{AM_mcmc_fit}}.
#' }
#'
#' @keywords demo

#' @export
#'
#' @examples
#' \donttest{
#'  mvn_output <- AM_demo_uvn_poi()
#' }
AM_demo_uvn_poi = function () {
	

	demo_univariate_normal <-AM_sample_uninorm(n = 500, pro=c(0.2,0.5,0.3),mmu=c(-2.1,0,2.3),ssd=c(0.5,0.5,0.5))
	y_uvn  <- demo_univariate_normal$y
	ci_uvn <- demo_univariate_normal$ci
	
	##############################################################################
	### PREPARE THE GIBBS for Normal mixture with poisson gamma priors
	##############################################################################
	
	mixture_uvn_params = AM_mix_hyperparams_uninorm  (m0=0,k0=0.1,nu0=1,sig02=1.5)
	
	mcmc_params        = AM_mcmc_parameters(niter=2000, burnin=1000, thin=10, verbose=0, output=c("ALL"))
	components_prior   = AM_mix_components_prior_pois (init=3,  a=1, b=1) 
	weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)
	
	fit <- AM_mcmc_fit(
			y = y_uvn, 
			mix_kernel_hyperparams = mixture_uvn_params,
			mix_components_prior =components_prior,
			mix_weight_prior = weights_prior,
			mcmc_parameters = mcmc_params)
	
	
	
	
	return (list(input = y_uvn, clusters = ci_uvn, fit = fit))
}


##' Returns an example of \code{\link{AM_mcmc_fit}} output produced by the univariate Poisson model
#'  
#'
#'This function allows us to generate a sample output of fitting the univariate poisson model. No arguments are needed to be passed.
#' The purpose of this function is to serve as a demo for users to understand the model's output, without diving too deep into details. By default,
#' this demo generates a sample dataset of dimension 500x1, where the MCMC sampler is specified to run for 2000 iterations, with a burn-in of 1000, and a thinning interval of 10. All possible outputs
#' that can be produced by \code{\link{AM_mcmc_fit}} are returned (see return value below).
#'  
#' @return A list containing the following items:
#' \itemize{
#' \item the vector (or matrix) containing the synthetic data used to fit the model.
#' \item the vector containing the final cluster assignment of each observation.
#' \item an \code{\link{AM_mcmc_output}} object, which is the typical output of \code{\link{AM_mcmc_fit}}.
#' }
#'
#' @keywords demo

#' @export
#'
#' @examples
#' \donttest{
#'  mvn_output <- AM_demo_uvn_poi()
#' }
AM_demo_uvp_poi = function () {
	
	
	demo_univariate_poisson <-AM_sample_unipois(n = 500, pro=c(0.2,0.5,0.3))
	y_uvp  <- demo_univariate_poisson$y
	ci_uvp <- demo_univariate_poisson$ci
	
	mixture_uvn_params = AM_mix_hyperparams_unipois  (alpha0=1, beta0=1)
	
	mcmc_params        = AM_mcmc_parameters(niter=2000, burnin=1000, thin=10, verbose=0, output=c("ALL"))
	components_prior   = AM_mix_components_prior_pois (init=3, a=1, b=1) 
	weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)
	
	fit <- AM_mcmc_fit(
			y = y_uvp, 
			mix_kernel_hyperparams = mixture_uvn_params,
			mix_components_prior =components_prior,
			mix_weight_prior = weights_prior,
			mcmc_parameters = mcmc_params)
	
	
	return (list(input = y_uvp, clusters = ci_uvp, fit = fit))
}
