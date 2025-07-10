#' Function to estimate amenity parameters of locations where users live.
#'
#' @param theta Float - Parameter that governs the reallocation of workers across
#'     locations in the city. This parameter measures how sensible are migration
#'     flows within the city to changes in real income.
#' @param N Integer - Number of locations.
#' @param L_i Nx1 matrix - Total residents.
#' @param W_i Nx1 matrix - Market access measure in each location.
#' @param Q Nx1 matrix - Floor space prices.
#' @param K Nx1 matrix - Land area
#' @param alpha Float - Para     
#' @param t_ij NxN matrix - Travel times across locations.
#' @param rho Float - decay parameter for amenities.
#' @param eta Float - congestion force
#'
#' @return Matrix with the amenity distribution of living in each location.
living_amenities_simple = function(theta,
                                   N,
                                   L_i,
                                   W_i,
                                   Q,
                                   K,
                                   alpha,
                                   t_ij,
                                   rho,
                                   eta){
  Q_mean = exp(mean(log(Q)));
  Q_norm = Q/Q_mean;
  L_i_mean = exp(mean(log(L_i)));
  L_i_norm = L_i/L_i_mean;
  W_i_mean = exp(mean(log(W_i)));
  W_i_norm = W_i/W_i_mean;
  B = array_operator(array_operator(L_i_norm^(1/theta), Q_norm^(1-alpha), '*'), W_i_norm^((-1)), '*');
  L_i_dens = (array_operator(L_i, K, '/'));
  L_i_dens_per = aperm(array(L_i_dens, dim=c(N,1)), c(2,1));
  L_i_dens_rep = kronecker(L_i_dens_per, array(1, dim=c(N, 1)));
  Omega = sumDims2(array_operator(exp(-rho*t_ij), L_i_dens_rep, '*'), 2);  
  b = array_operator(B, Omega^(-eta), "/");
  b = b*(L_i>0);
  return(list(B = B, b = b))
}
