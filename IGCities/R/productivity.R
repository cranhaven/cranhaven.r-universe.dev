#' Computes productivity levels in each location
#'
#' @param N Float - Number of locations.
#' @param Q Nx1 matrix - Floorspace prices in each location.
#' @param w Nx1 matrix - wages in each location.
#' @param L_j Nx1 matrix - Employment in each location.
#' @param K Nx1 matrix - Land in each location.
#' @param t_ij NxN matrix - Travel times matrix.
#' @param delta Float - decay parameter agglomeration.
#' @param lambda Float - agglomeration force.
#' @param beta Float - Output elasticity wrt labor
productivity = function(N,
                        Q,
                        w,
                        L_j,
                        K,
                        t_ij,
                        delta,
                        lambda,
                        beta){
  
  Q_mean = exp(mean(log(Q)));
  Q_norm = Q/Q_mean;
  beta_tilde = ((1-beta)^(1-beta))*(beta^beta); 
  A = (1/beta_tilde)*(array_operator(Q_norm^(1-beta), w^beta, '*'));
  L_j_dens = (array_operator(L_j, K, '/'));
  L_j_dens_per = aperm(array(L_j_dens, dim=c(N,1)), c(2,1));
  L_j_dens_rep = kronecker(L_j_dens_per, array(1, dim=c(N, 1)));
  Upsilon = sumDims2(array_operator(exp(-delta*t_ij), L_j_dens_rep, '*'), 2);
  a = array_operator(A, Upsilon^lambda, "/");
  a = a*(L_j>0)
  return(list(A = A, a = a))
}