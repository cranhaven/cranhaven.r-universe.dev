#' Function to transform travel times into iceberg commuting costs
#' @param t_ij NxN matrix - Travel time matrix across locations
#' @param epsilon Float - Parameter that transforms travel times to commuting costs
#' 
#' @return A NxN matrix of commuting costs
commuting_matrix = function(t_ij,
                            epsilon){
  tau = exp(epsilon*t_ij)
  return(list(tau=tau))
}