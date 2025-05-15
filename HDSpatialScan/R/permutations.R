################################################################
##' @title Permutates the data
##'
##' @description This function will permit to permute the data for the MC simulations
##'
##' @param to_permute vector. Vector of indices we want to permute.
##' @param nb_permu numeric. Number of permutations.
##'
##'
##' @return matrix. Matrix of nb_permu rows and length(to_permute) columns.
##'
##'
permutate <- function(to_permute, nb_permu){
  simulation <- matrix(sapply(1:nb_permu, function(i) sample(x = to_permute, size = length(to_permute), replace = FALSE)), ncol = nb_permu)
  return(t(simulation))
}
