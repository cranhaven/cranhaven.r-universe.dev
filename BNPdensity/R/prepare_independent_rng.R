# prepare_independent_rng = function(nchains, niter){
#   seeds = rep(0, nchains)
#   runif(1) #Makes sure that the random number generator has been used at least once so that .Random.seed exists
#
#   for (c in seq(nchains)){
#     seeds[c] = .Random.seed
#     runif(niter)
#   }
#
# }
