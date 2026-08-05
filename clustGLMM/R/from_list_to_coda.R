from_list_to_coda <- function(mcmc, burnin=0, thin=1){
  mcmc <- from_list_to_matrix(mcmc)
  codamcmc <- from_matrix_to_coda(mcmc, burnin=burnin, thin=thin)
  return(codamcmc)
}