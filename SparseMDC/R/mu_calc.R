#' mu Calculator
#'
#' This function handles the calculations of the Mu Solver. This function
#'  runs inside \code{sparse_mdc}.
#'
#' @param d - The current condition.
#' @param v - Relationship matrix, describing relationship between d-1 and d.
#' @param EQ - Equality matrix specifying the number of following terms to
#' which mu_d is equal.
#' @param dim Total number of conditions, D.
#' @param x - Matrix of mean values.
#' @param nk - Vector with the number of samples in each dimension for this
#' cluster.
#' @param p1 - Penalties on mean size.
#' @param p2 - Penalties on mean difference.
#' @param delta Small term to ensure existance of solution.
#'
#' @return A list containing two vectors containing the calculated values of
#' mu_{d} | mu_{d} < mu_{d-1} and mu_{d} | mu_{d} > mu_{d-1} respecitively.
#' @export
mu_calc <- function(d, v, EQ, dim, x, nk, p1, p2, delta){
  # Create storage for results
  m_1 <- m_2 <- rep(NA, nrow(x))
  # Calcualte the necessary equal dimension for each gene
  dims <- d + EQ[,d]
  # Select the unique values of equalities
  calc_dims <- unique(dims)
  for ( i in calc_dims){ # For  each equality
    s_1 <- d # Get starting term
    f_1 <- i # Get final term
    n_kt <- max(sum(nk[s_1:f_1]),1)
    if (d == 1 & i < dim){ # If d == 1 and equality does not reach dim
      # Calculate x term
      t_1 <- rowSums(t( t(x[ dims == i, s_1:f_1, drop = FALSE])  *
                          (nk[s_1:f_1])) / n_kt)  +
        (v[dims == i,f_1] * p2[f_1])/n_kt
      # Calculate a term

      p_1 <- (sum(p1[s_1:f_1])/n_kt)
      # Apply soft thresholding function
      m_1[dims == i] <-  S_func(t_1 , p_1)
    } else if (d == 1 & i == dim){
      # Calculate x term
      t_1 <- rowSums(t( t(x[ dims == i, s_1:f_1, drop = FALSE])  *
                          (nk[s_1:f_1])) / n_kt)
      # Calculate a term
      p_1 <- (sum(p1[s_1:f_1])/n_kt)
      # Apply soft thresholding function
      m_1[dims == i] <-  S_func(t_1 , p_1)
    } else if (d > 1 & i < dim){
      # Calculate x terms
      t_1 <- rowSums(t( t(x[dims == i, s_1:f_1, drop = FALSE])  *
                          (nk[s_1:f_1])) /n_kt ) +
        (p2[s_1-1])/n_kt +
        (v[dims == i,f_1] * p2[f_1])/n_kt
      t_2 <- rowSums(t( t(x[ dims == i, s_1:f_1, drop = FALSE])  *
                          (nk[s_1:f_1])) / n_kt ) -
        (p2[s_1-1])/n_kt +
        (v[dims == i,f_1] * p2[f_1])/n_kt
      # Calculate a term
      p_1 <- (sum(p1[s_1:f_1])/n_kt)
      # Apply soft thresholding function
      m_1[dims == i] <-  S_func(t_1 , p_1)
      m_2[dims == i] <-  S_func(t_2 , p_1)
    } else if (d > 1 & i == dim){
      # Calculate x terms
      t_1 <- rowSums(t( t(x[ dims == i, s_1:f_1, drop = FALSE])  *
                          (nk[s_1:f_1] )) / n_kt ) +
        (p2[s_1-1])/n_kt
      t_2 <- rowSums(t( t(x[ dims == i, s_1:f_1, drop = FALSE])  *
                          (nk[s_1:f_1] )) / n_kt ) -
        (p2[s_1-1])/n_kt
      # Calculate a term
      p_1 <- (sum(p1[s_1:f_1])/n_kt)
      # Apply soft thresholding function
      m_1[dims == i] <-  S_func(t_1 , p_1)
      m_2[dims == i] <-  S_func(t_2 , p_1)
    }
  }
  return(list(m_1 = m_1, m_2 = m_2))
}
