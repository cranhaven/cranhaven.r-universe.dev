bind.mcmclists = function(pr, r){
  l = length(pr)
  b = list()
  b[[1]] = mcmc(rbind ( as.mcmc(pr[1]), as.mcmc(r[1]) ))
  if(l > 1){
  for( i in 2:l){
    b[[i]] = mcmc(rbind ( as.mcmc(pr[i]), as.mcmc(r[i]) ) )
  }}
  
  mcmc.list(b)
}
