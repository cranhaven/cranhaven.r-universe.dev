#######################################################################################
###############
############### AntMAN Package : Tests and Examples
###############
###############
#######################################################################################

quit() ## Skip this test - too long
pdf(file.path(tempdir(), "Galaxy.pdf"))

library("AntMAN")

data(galaxy)
y_uvn = galaxy
mixture_uvn_params = AM_mix_hyperparams_uninorm  (m0=20.83146, k0=0.3333333, nu0=4.222222, sig02=3.661027)

mcmc_params        = AM_mcmc_parameters(niter=20000, burnin=5000, thin=10, verbose=0, output=c("ALL"))
components_prior   = AM_mix_components_prior_pois (init=3,  a=1, b=1) 
weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)

fit <- AM_mcmc_fit(
  y = y_uvn,
  mix_kernel_hyperparams = mixture_uvn_params,
  mix_components_prior =components_prior,
  mix_weight_prior = weights_prior,
  mcmc_parameters = mcmc_params)

summary (fit)
plot (fit)
