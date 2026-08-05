as_coda <- function(mcmc, burnin=0, thin=1){
  if(!inherits(mcmc, "clustglmm")){
    stop("mcmc is not 'clustglmm' object")
  }
  
  if(mcmc$howsave == "data.frame"){
    codamcmc <- from_matrix_to_coda(mcmc, burnin=burnin, thin=thin)
  } else if(mcmc$howsave == "list"){
    codamcmc <- from_list_to_coda(mcmc, burnin=burnin, thin=thin)
  } else {
      stop("Not implemented for howsave = ", sQuote(mcmc$howsave),
           " yet.")
  }
  
  return(codamcmc)
}
