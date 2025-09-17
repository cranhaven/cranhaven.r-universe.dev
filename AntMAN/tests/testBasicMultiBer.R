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
### BUILD THE MULTIVARIATE BINOMIAL DATA
##############################################


d <- 4
k <- 3
TH <- matrix(nrow=k,ncol=d)

TH[1,] <- c(0.9,0.0,0.2,0.1)
TH[2,] <- c(0.0,0.9,0.1,0.2)
TH[3,] <- c(0.0,0.0,0.9,0.9)
n <- 1000
demo_multivariate_binomial <- AM_sample_multibin(n,d,c(0.3,0.3,0.4),TH)

y_mvb  <- demo_multivariate_binomial$y
ci_mvb <- demo_multivariate_binomial$ci

hist(y_mvb,freq=FALSE,nclass=15,col=colors()[4])
plot(y_mvb,col=ci_mvb+1)


##############################################################################
### PREPARE THE GIBBS for multivariate BINOMIAL mixture with poisson gamma priors
##############################################################################


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

summary (fit)
plot (fit)
