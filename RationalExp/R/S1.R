#' Core part of the Statistic T
#'
#' This function implements the core part of the Cramer-von-Mises test statistic T, denoted by S in AS.
#' @param m_bar  the sample vector of moments for a specified vector $(h_{a,r},y)$
#' @param sigma_bar the sample covariance matrix of m_bar
#' @param M1 number of inequality moments
#' @param N_k index of the $ h_{a,r}$ function considered
#' @param p parameter p in the statistic
#'
#' @return  a real number with the statistic evaluated
#' @export
#'
#' @examples
S1 <- function( m_bar,sigma_bar,  M1, N_k, p){

  if(N_k ==1){
    S1_temp <- m_bar/sqrt(sigma_bar)
  }else{
    S1_temp <- m_bar/sqrt(diag(sigma_bar))
  }

  if (M1 != 0) {
    S1_temp[1:M1] = S1_temp[1:M1] * (S1_temp[1:M1] <= 0 )
  }
  return( (1-p)*sum(S1_temp[1:M1]^2) +  p*sum(S1_temp[M1+1]^2))
}
