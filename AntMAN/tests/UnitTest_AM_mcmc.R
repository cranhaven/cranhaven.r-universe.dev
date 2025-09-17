#######################################################################################
###############
############### AntMAN Package : Tests and Examples
###############
###############
#######################################################################################

##############################################
### Load the AntMan package
##############################################

library("AntMAN")
set.seed(123)

##############################################
### BUILD THE UNIVARIATE POISSON DATA
##############################################

set.seed(123)
demo_univariate_poisson <-AM_sample_unipois(n=1000,pro=c(0.2,0.5,0.3),mth=c(5,25,50)) 
y_uvp  <- demo_univariate_poisson$y
ci_uvp <- demo_univariate_poisson$ci

hist(y_uvp,freq=FALSE,nclass=15,col=colors()[4])
plot(1:length(y_uvp),y_uvp,col=ci_uvp+1)



##############################################
### Run
##############################################


mcmc_params        = AM_mcmc_parameters(niter=2000, burnin=1000, thin=10, verbose=0, output=c("ALL"))
mixture_uvp_params = AM_mix_hyperparams_unipois (alpha0=2, beta0=0.2)
components_prior   = AM_mix_components_prior_dirac (Mstar=5) 
weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)
init_ci_uvp <- 0:(length(y_uvp)-1);

fit <- AM_mcmc_fit(
		y = y_uvp, init_K=1,
		mix_kernel_hyperparams = mixture_uvp_params,
		mix_components_prior =components_prior,
		mix_weight_prior = weights_prior,
		mcmc_parameters = mcmc_params)


summary (fit)
plot (fit)

eam = AM_clustering(fit)
cluster = AM_salso(eam, "binder")
#cluster = AM_binder(fit)$Labels 

#refit = AM_mcmc_refit(y = y_uvp , 
#		fit = fit , 
#		fixed_clustering = cluster, 
#		mcmc_parameters = mcmc_params )


#summary (refit)
#plot (refit)



##############################################
### Verify
##############################################

stopifnot(is.list(fit))
#stopifnot(is.list(refit))

