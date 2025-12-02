
# EXAMPLE OF TARGET DEFINITION
# target density of the MCMC, user-defined
# x = d-dim vector, bivariate (d=2) example
# param = list of ALL parameters, model-dependent
target_norm = function(x,param){
  mixtools::dmvnorm(x, mu=param$mean, sigma=param$v)}
target_norm_param = list(mean=rep(0,2),v=diag(1:2))
