#' Function to compute equilibrium wages that make the model labor in every
#' location in equal to the observed data. It finds the w's
#' such that equation (3.2) holds.
#'
#' @param N Integer - Number of locations.
#' @param w_init Initial vector of wages.
#' @param theta Float - Commuting elasticity.
#' @param tau NxN matrix - Commuting cost matrix across all locations.
#' @param L_i Nx1 matrix - Number of residents in each location.
#' @param L_j Nx1 matrix - Number of workers in each location.
#' @param nu_init Float - Convergence parameter to update wages.
#'     Default nu=0.01.
#' @param tol Float - Maximum tolerable error for estimating total labor.
#'     Default tol=10^-10.
#' @param maxiter Integer - Maximum number of iterations for convergence.
#'     Default maxiter=10000.
#'
#' @return A list with equilibrium wages and probability of workers in each
#'     location working in every other location.
wages_inversion = function(N,
                           w_init,
                           theta,
                           tau,
                           L_i,
                           L_j,
                           nu_init=0.05,
                           tol=10^-10,
                           maxiter=10000){
  
  # Settings
  outerdiff = Inf
  w = w_init
  iter = 0
  nu = nu_init
  
  cat("Inverting wages...\n")
  while(outerdiff>tol & iter<maxiter){
    # 1) Labor supply
    # Indirect utility
    w_tr = aperm(array(w, dim=c(N,1)), c(2,1));
    rep_w_tr = kronecker(w_tr^theta, array(1, dim=c(N, 1)));
    # Constructing emp` loyment shares
    w_tr_tau = array_operator(w_tr^theta, tau^(-theta), '*');
    lambda_ij_i = array_operator(w_tr_tau, sumDims2(w_tr_tau,2), '/');
    W_i = (sumDims2(w_tr_tau,2))^(1/theta);
    
    # Labor is equal to probabilities * total number of residents * proportion of workers in each sector.
    L_ij = array_operator(L_i, lambda_ij_i, '*')
    L_j_tr = sumDims2(L_ij, 1)
    #    L_j_model = aperm(L_j_tr, c(2, 1));
    Ratio_supply = array_operator(L_j_tr, w, "/");
    w_prime = array_operator(L_j, Ratio_supply, "/");
    
    z_L = array_operator(w, w_prime, '-');
    w = array_operator(w*(1-nu), w_prime*nu, '+');
    w_mean = exp(mean(log(w)))
    w = w/w_mean;
    outerdiff = max(abs(z_L))
    
    iter = iter+1;
    
    if(iter %% 10 == 0){
      cat(paste0("Iteration: ", iter, ", error: ", round(outerdiff, 10), ".\n"))
    }
  }
  if(outerdiff<=tol){
    cat(paste0("Converged after ", iter, " iterations. Error=", round(outerdiff, 10), ".\n"))
  } else{
    cat(paste0("Reached maximum number of iterations (", iter, "). Error=", round(outerdiff, 10), ".\n"))
  }
  
  return(list(w=w, w_tr=w_tr, W_i=W_i, lambda_ij_i=lambda_ij_i))
}