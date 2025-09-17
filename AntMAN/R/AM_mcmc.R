#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################
#' S3 class AM_mcmc_output
#' @description Output type of return values from  \code{\link{AM_mcmc_fit}}. 
#' @seealso \code{\link{AM_mcmc_fit}}
#' @name AM_mcmc_output
#' @return \code{\link{AM_mcmc_output}}
NULL

#' S3 class AM_mcmc_configuration
#' @description Output type of return values from  \code{\link{AM_mcmc_parameters}}. 
#' @seealso \code{\link{AM_mcmc_fit}}
#' @name AM_mcmc_configuration
#' @return \code{\link{AM_mcmc_configuration}}
NULL

#################################################################################
##### AM_mcmc_configuration function
#################################################################################

#'  summary information of the AM_mcmc_configuration object 
#'  
#'  
#'
#' Given an \code{\link{AM_mcmc_configuration}} object, this function prints the summary information
#' of the specified mcmc configuration. 
#'  
#'@param object an \code{\link{AM_mcmc_configuration}} object. 
#'@param ... all additional parameters are ignored
#'  
#'  
#'@method summary AM_mcmc_configuration 
#'@seealso \code{\link{AM_mcmc_parameters}}
#'@return NULL. Called for side effects.
#'@export
summary.AM_mcmc_configuration = function(object, ...){
	cat("\n", "AM_mcmc_configuration\n", sep = "")	
	for (item in names(object)) {
		cat(' -', item , ': ' , head(unlist(object[[item]], use.names=FALSE)), "\n")
	}
}



#################################################################################
##### AM_mcmc_output function
#################################################################################



#' plot AM_mcmc_output  
#' 
#'
#' Given an \code{\link{AM_mcmc_output}} object, this function plots some useful information about the MCMC results
#' regarding \eqn{M} and \eqn{K}. Besides the PMFs, some of the diagnostic plots of the MCMC chain are visualised.
#'  
#'@param x an \code{\link{AM_mcmc_output}} object.
#'@param ... all additional parameters are ignored.
#'@return NULL. Called for side effects.
#'  
#'@method plot AM_mcmc_output 
#'@importFrom graphics image
#'@importFrom grDevices gray.colors
#'@export
plot.AM_mcmc_output=function(x, ...){

	print(AM_plot_pairs(x));readline(prompt="Press [enter] to continue")
	print(AM_plot_pmf(x));readline(prompt="Press [enter] to continue")
	print(AM_plot_traces(x));readline(prompt="Press [enter] to continue")
	print(AM_plot_values(x));readline(prompt="Press [enter] to continue")
	print(AM_plot_chaincor(x));
	return(NULL)
}


#' Internal function that produces a string from a list of values
#'  
#'@param x a list of values
#'  
#'@importFrom utils head
#' @keywords internal
list_values = function (x) { 
	arguments = vector();
	for (item in names(x)) {
		arguments = append(arguments,sprintf("%s = %s",item, head(x[[item]]) )) ;
	}
	return (paste(arguments, collapse=", "));
}

#'  summary information of the AM_mcmc_output object 
#'  
#'
#' Given an \code{\link{AM_mcmc_output}} object, this function prints the summary information
#' pertaining to the given model output. 
#'  
#'@param object a \code{\link{AM_mcmc_output}} object
#'@param ... all additional parameters are ignored
#'@return NULL. Called for side effects.
#'  
#'  
#'@method summary AM_mcmc_output 
#'@seealso \code{\link{AM_mcmc_fit}}, \code{\link{AM_mcmc_refit}}
#'@export
summary.AM_mcmc_output=function(object,...){
	cat("\n","Fitted model:","\n");
	cat(" -mix_kernel_hyperparams(",list_values(attr(object,'mix_kernel_hyperparams')),")\n", sep = "");
	cat(" -mix_components_prior(",list_values(attr(object,'mix_components_prior')),")\n", sep = "");
	cat(" -mix_weight_prior(",list_values(attr(object,'mix_weight_prior')),")\n", sep = "");
	cat(" -mcmc_parameters(",list_values(attr(object,'mcmc_parameters')),")\n", sep = "");
	cat("\n - Summary of the MCMC output:\n\n");
	cat(sprintf("    %10s%10s%10s%10s%10s%10s%10s%10s\n", "Name", "Mean", "StdDev", "2.5%","50%","97.5%", "ESS", "MCMC Err."));
	invisible = c("CI","W","mu","Sig","sig2","theta","R","P")
	
	
	
	# If fixed clustering 
	for (item in names(object)) {
		if (!item %in% invisible) { 
			allcols = AM_extract(object,c(item))
			for (subitem in names(allcols)) {
				e = allcols[[subitem]]
				emean = mean(e)
				esd = sd(e)
				elen = length(e)
				neff = NA 
				if (anyNA(e) == FALSE) neff = IAM_mcmc_neff(e)
				mcmcerror = NA 
				if (anyNA(e) == FALSE) mcmcerror = IAM_mcmc_error(e)
				q = quantile(e,prob=c(0.025, 0.5,0.975), names=FALSE, na.rm=TRUE)
				cat(sprintf("    %10s%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f\n", subitem, emean, esd, q[1], q[2], q[3], neff, mcmcerror));
			}
		}
	}
	
}

## INTERNAL
# AM_reshape is an internal function for reshaping the fit output into the correct form 

AM_reshape <- function(fit, y){

	y_dim = dim(y)[2]
	if (is.null(y_dim)){
			y_dim = 1}

  if (!is.null(fit$theta)){
	theta_mat = as.matrix(fit$theta)
    theta = apply(theta_mat, 1, function(x){
    x_vec = as.numeric(unlist(x))
      matrix(x_vec, ncol=y_dim, nrow=length(x_vec)/y_dim, byrow=F)})
    fit$theta = theta
  }
  
  if (!is.null(fit$mu)){
      mu = mapply(function(x, m){
      mu_size = length(x)/m
      mu_individual = split(x, ceiling(seq_along(x)/mu_size))
      mu_combined = lapply(mu_individual, function(mu_vec){
        matrix(unlist(mu_vec), nrow=y_dim, byrow=F)
      })
    }, fit$mu, fit$M)
    fit$mu = mu
  }

   if (!is.null(fit$sig2)){
    sig = mapply(function(x, m){
      sig_size = length(x)/m
      sig_individual = split(x, ceiling(seq_along(x)/sig_size))
      sig_combined = lapply(sig_individual, function(sig_vec){
        matrix(unlist(sig_vec), ncol=y_dim, nrow=y_dim, byrow=F)
      })
    }, fit$sig2, fit$M)
    fit$sig2 = sig
  }
  
  if (!is.null(fit$Sig)){
    sig = mapply(function(x, m){
      sig_size = length(x)/m
      sig_individual = split(x, ceiling(seq_along(x)/sig_size))
      sig_combined = lapply(sig_individual, function(sig_vec){
        matrix(unlist(sig_vec), ncol=y_dim, nrow=y_dim, byrow=F)
      })
    }, fit$Sig, fit$M)
    fit$Sig = sig
  }
  return(fit)
}





#################################################################################
##### AM_mcmc_fit function
#################################################################################


#' Performs a Gibbs sampling
#' 
#' The \code{AM_mcmc_fit} function performs a Gibbs sampling in order to estimate the mixture comprising the sample data \code{y}.
#' The mixture selected must be of a predefined type \code{mix_kernel_hyperparams} (defined with \code{AM_mix_hyperparams_*} functions, where star 
#' \code{*} denotes the chosen kernel). 
#' Additionally, a prior distribution on the number of mixture components  
#' must be specified through \code{mix_components_prior}
#' (generated with  \code{AM_mix_components_prior_*} functions, where \code{*}  denotes the chosen prior). Similarly,  
#' a prior on the weights of the mixture should be specified through \code{mix_weight_prior} 
#' (defined with  \code{AM_mix_weights_prior_*} functions). Finally, with \code{mcmc_parameters}, the user sets
#' the MCMC parameters for the Gibbs sampler (defined with \code{\link{AM_mcmc_parameters}} functions). 
#' 
#' If no initial clustering is specified (either as \code{init_K} or \code{init_clustering}), 
#' then every observation is allocated to a different cluster. 
#' If \code{init_K} is specified then AntMAN initialises the clustering through K-means. 
#' 
#' **Warning**: if the user does not specify init_K or initial_cluster, the first steps can be be time-consuming because of default setting of the initial clustering. 
#' 
#'
#'@param y input data, can be a vector or a matrix.
#'@param mix_kernel_hyperparams is a configuration list, defined by *_mix_hyperparams functions, where * denotes the chosen kernel. 
#'See \code{\link{AM_mix_hyperparams_multiber}}, \code{\link{AM_mix_hyperparams_multinorm}}, \code{\link{AM_mix_hyperparams_uninorm}}, \code{\link{AM_mix_hyperparams_unipois}} for more details.
#'@param initial_clustering is a vector CI of initial cluster assignement. If no clustering is specified (either as \code{init_K} or \code{init_clustering}), then every observation is 
#' assigned to its own cluster.
#'@param fixed_clustering if specified, this is the vector CI containing the cluster assignments. This will remain unchanged for every iteration.
#'@param init_K initial value for  the number of cluster. When this is specified, AntMAN intitialises the clustering assign usng K-means.
#'@param mix_components_prior is a configuration list defined by AM_mix_components_prior_* functions, where * denotes the chosen prior.
#' See \code{\link{AM_mix_components_prior_dirac}}, 
#' \cr \code{\link{AM_mix_components_prior_negbin}}, \code{\link{AM_mix_components_prior_pois}} for more \cr
#' details.
#'@param mix_weight_prior is a configuration list defined by AM_weight_prior_* functions, where * denotes the chosen prior specification. 
#' See \code{\link{AM_mix_weights_prior_gamma}} for more \cr details.
#'@param mcmc_parameters is a configuration list defined by AM_mcmc_parameters. See \code{\link{AM_mcmc_parameters}} for more details.
#'@return The return value is an \code{\link{AM_mcmc_output}} object.
#'@examples
#' \donttest{
#'  AM_mcmc_fit( AM_sample_unipois()$y, 
#'               AM_mix_hyperparams_unipois (alpha0=2, beta0=0.2), 
#'               mcmc_parameters = AM_mcmc_parameters(niter=50, burnin=0, thin=1, verbose=0))
#' }
#'@useDynLib AntMAN
#'@export

AM_mcmc_fit <- function(
		y, 
		mix_kernel_hyperparams, 
		initial_clustering = NULL, 
		init_K = NULL, 
		fixed_clustering = NULL, 
		mix_components_prior = AM_mix_components_prior_pois() , 
		mix_weight_prior = AM_mix_weights_prior_gamma(), 
		mcmc_parameters = AM_mcmc_parameters() ) {
	fixed_cluster = FALSE
	if (is.null(fixed_clustering) & is.null(init_K) & !is.null(initial_clustering)) {
		fixed_cluster = FALSE
		# initial_clustering is set
	} else if (!is.null(init_K) & is.null(initial_clustering)& is.null(fixed_clustering)) {
		fixed_cluster = FALSE
		initial_clustering <- kmeans(y, init_K)$cluster
		# initial_clustering is set
	} else if (is.null(init_K) & is.null(initial_clustering)& is.null(fixed_clustering)) {
		fixed_cluster = FALSE
		initial_clustering <- 0:(NROW(y)-1)
		# initial_clustering is set
	} else if (is.null(init_K) & is.null(initial_clustering)& !is.null(fixed_clustering)) { 
		fixed_cluster = TRUE
		initial_clustering = fixed_clustering
	} 
	else {
		stop("Please provide only one of K_init or initial_clustering or fixed_clustering.")
	}

	fit_result = (structure(
			IAM_mcmc_fit(y = y, mix_kernel_hyperparams = mix_kernel_hyperparams, initial_clustering = initial_clustering, fixed_clustering=  fixed_cluster , mix_components_prior = mix_components_prior, mix_weight_prior = mix_weight_prior, mcmc_parameters = mcmc_parameters)
			, class = "AM_mcmc_output",
			mix_kernel_hyperparams = mix_kernel_hyperparams, 
			initial_clustering = initial_clustering, 
			init_K = init_K, 
			fixed_clustering = fixed_clustering, 
			mix_components_prior =mix_components_prior , 
			mix_weight_prior = mix_weight_prior, 
			mcmc_parameters =mcmc_parameters));

	fit_result = AM_reshape(fit_result, y)

	return (fit_result)
}


#' Performs a Gibbs sampling reusing previous configuration
#' 
#' Similar to \code{\link{AM_mcmc_fit}}, the \code{AM_mcmc_refit} function performs a Gibbs sampling in order to estimate 
#' a mixture. However parameters will be reused from a previous result from \code{\link{AM_mcmc_fit}}.
#' 
#' In practice this function will call AM_mcmc_fit(y, fixed_clustering = fixed_clustering, ...); with the same parameters as previously
#' specified.
#'
#'@param y input data, can be a vector or a matrix.
#'@param fit previous output from \code{\link{AM_mcmc_fit}} that is used to setup kernel and priors.
#'@param fixed_clustering is a vector CI of cluster assignment that will remain unchanged for every iterations.
#'@param mcmc_parameters is a configuration list defined by \code{\link{AM_mcmc_parameters}}. 
#'@return The return value is an \code{\link{AM_mcmc_output}} object.
#'@examples
#' \donttest{
#'  y = AM_sample_unipois()$y
#'  fit = AM_mcmc_fit( y , 
#'               AM_mix_hyperparams_unipois (alpha0=2, beta0=0.2), 
#'               mcmc_parameters = AM_mcmc_parameters(niter=20, burnin=0, thin=1, verbose=0))
#'  eam = AM_coclustering(fit)
#'  cluster = AM_salso(eam, "binder")
#'  refit = AM_mcmc_refit(y , fit, cluster, 
#'          mcmc_parameters = AM_mcmc_parameters(niter=20, burnin=0, thin=1, verbose=0));
#' }
#'@export

AM_mcmc_refit <- function(
		y, fit,
		fixed_clustering, 
		mcmc_parameters = AM_mcmc_parameters() ) {
	
	mcp = attr(fit,'mix_components_prior')
	mwp = attr(fit,'mix_weight_prior')
	mkh = attr(fit,'mix_kernel_hyperparams')
	
	## TODO : check input data size 
	
	AM_mcmc_fit(y, 
				mix_kernel_hyperparams = mkh, 
				fixed_clustering = fixed_clustering, 
				mix_components_prior = mcp , 
				mix_weight_prior = mwp, 
				mcmc_parameters = mcmc_parameters );
	
}

#################################################################################
##### AM_mcmc_parameters function
#################################################################################



#' MCMC Parameters
#' 
#' This function generates an MCMC parameters list to be used as \code{mcmc_parameters} argument within \code{\link{AM_mcmc_fit}}. 
#'   
#' 
#' 
#'@param niter        Total number of MCMC iterations to be carried out. 
#'@param burnin       Number of iterations to be considered as burn-in. Samples from this burn-in period are discarded.
#'@param thin         Thinning rate. This argument specifies how often a draw from the posterior distribution is stored after
#' burnin, i.e. one every -th samples is saved. Therefore, the toral number of MCMC samples saved is 
#' (\code{niter} -\code{burnin})/\code{thin}. If thin =1, then AntMAN stores every iteration. 
#'@param verbose      A value from 0 to 4, that specifies the desired level of verbosity (0:None, 1:Warnings, 2:Debug, 3:Extras).
#'@param output       A list of parameters output to return.
#'@param output_dir   Path to an output dir, where to store all the outputs.
#'@param parallel     Some of the algorithms can be run in parallel using OpenMP. When set to True, this parameter triggers the parallelism.
#'@return An \code{\link{AM_mcmc_configuration}} Object. This is a list to be used as \code{mcmc_parameters} argument with \code{\link{AM_mcmc_fit}}. 
#'@examples 
#' AM_mcmc_parameters (niter=1000, burnin=10000, thin=50)
#' AM_mcmc_parameters (niter=1000, burnin=10000, thin=50, output=c("CI","W","TAU"))
#'@export
AM_mcmc_parameters <- function(  niter=5000,
		burnin=2500, ## niter / 2
		thin=1,
		verbose = 1,
		output=c("CI","K"),
		parallel=TRUE,
		output_dir = NULL) {
	
	
	return (structure(list(type="AM_MCMC_PARAMETERS", 
					niter=niter, burnin=burnin, thin=thin,
					verbose=verbose, output=output, parallel=parallel,
					output_dir=output_dir), class = "AM_mcmc_configuration") );
	
}





