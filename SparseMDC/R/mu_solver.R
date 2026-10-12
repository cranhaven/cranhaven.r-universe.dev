#' Mu Solver
#'
#' Calculates the regularized center values for a cluster. This function
#'  runs inside \code{sparse_mdc}.
#'
#' @param d - The current condition.
#' @param mu - A matrix of regularized mean values.
#' @param v - Relationship matrix, describing relationship between d-1 and d.
#' @param EQ - Equality matrix specifying the number of following terms to
#' which mu_d is equal.
#' @param dim - Total dimensions.
#' @param x - Matrix of mean values.
#' @param nk - Vector with the number of samples in each condition for this
#' cluster.
#' @param p1 - Penalties on mean size.
#' @param p2 - Penalties on mean difference.
#' @param delta Small term to ensure existance of solution.
#'
#' @return A matrix containing the regularized cluster means for each
#' dimension.
#' @export
mu_solver <- function(d, mu, v, EQ,  dim, x, nk, p1, p2, delta){
  if ( d == 1){ # Calculate mu_1

    mu[,1] <- mu_calc(d = 1, v = v, EQ = EQ, dim = dim, x = x, nk = nk,
                      p1 = p1, p2 = p2,  delta = delta)[[1]] # Use mu_calc

  }
  else if (1 < d){
    ## Calculate mu_{d-1}
    v_1 <- v_2 <- v # Create temporary relationship matrices
    v_1[,d-1] <- -1 # mu_{d-1} > mu_d
    v_2[,d-1] <- 1 # mu_{d-1} < mu_d
    EQ_b <- EQ # Create temporary equality matrix
    EQ_b[,d-1] <- EQ[,d] + 1 # Add in new equality
    # Calculate mu_{d-1}^{1}
    fit_1 <- mu_solver(d = d-1, mu = mu, v = v_1, EQ = EQ, dim = dim, x = x,
                       nk = nk, p1 = p1, p2 = p2, delta = delta)
    # Calculate mu_{d-1}^{2}
    fit_2 <- mu_solver(d = d-1, mu = mu, v = v_2, EQ = EQ, dim = dim, x = x,
                       nk = nk, p1 = p1, p2 = p2, delta = delta)
    # Calculate mu_{d-1}^{3}
    fit_3 <- mu_solver(d = d - 1, mu = mu, v = v, EQ = EQ_b, dim = dim, x = x,
                       nk = nk, p1 = p1, p2 = p2, delta = delta)

    ## Calculate mu_d
    t_vals <- mu_calc(d = d, v = v, EQ = EQ, dim = dim, x = x, nk = nk,
                      p1 = p1, p2 = p2, delta = delta)

    ## Test Relationships between mu_{d-1} & mu_d and assign results
    ind_1 <- fit_1[,d-1] > t_vals[[1]] # mu_{d-1}^{1} > mu_d^{1}
    mu[ind_1,1:(d-1)] <- fit_1[ind_1,1:(d-1)]
    mu[ind_1,d] <- t_vals[[1]][ind_1]

    ind_2 <- fit_2[,d-1] < t_vals[[2]] # mu_{d-1}^{2} < mu_d^{2}
    mu[ind_2,1:(d-1)] <- fit_2[ind_2,1:(d-1)]
    mu[ind_2,d] <- t_vals[[2]][ind_2]

    ## Calcualte Equal vals
    ind_3 <- !ind_1 & !ind_2 # Store indices of equals
    if (sum(ind_3) > 0){ # If any terms are equals
      mu[ind_3,1:(d-1)] <- fit_3[ind_3,1:(d-1)] # Assign results
      mu[ind_3,d] <- mu[ind_3, d-1]
    }
  }
  return(mu)
}
