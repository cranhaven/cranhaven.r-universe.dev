#######################################################################################
###############
############### AntMAN Package : Tests and Examples
###############
###############
#######################################################################################

quit() ## Skip this test - too long
##############################################
### Load the AntMan package
##############################################

library("AntMAN")


##############################################
### BUILD THE MULTIVARIATE NORMAL DATA
##############################################

set.seed(123)


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



demo_multivariate_normal <-AM_sample_multinorm(n = 1000 ,d = 2,c(0.3,0.3,0.4),MU,SIG)
y_mvn  <- demo_multivariate_normal$y
ci_mvn <- demo_multivariate_normal$ci

hist(y_mvn,freq=FALSE,nclass=15,col=colors()[4])
plot(y_mvn,col=ci_mvn+1)


##############################################################################
### PREPARE THE GIBBS for multivariate Normal mixture with poisson gamma priors
##############################################################################


mixture_mvn_params = AM_mix_hyperparams_multinorm   (mu0=c(0,0),ka0=1,nu0=4,Lam0=diag(2))

mcmc_params        = AM_mcmc_parameters(niter=4000, burnin=2000, thin=10, verbose=0, output=c("ALL"))
components_prior   = AM_mix_components_prior_pois (init=3,  a=1, b=1) 
weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)

fit <- AM_mcmc_fit(
       			y = y_mvn, 
                        mix_kernel_hyperparams = mixture_mvn_params,
                        mix_components_prior =components_prior,
                        mix_weight_prior = weights_prior,
                        mcmc_parameters = mcmc_params)


summary (fit)
plot (fit)

