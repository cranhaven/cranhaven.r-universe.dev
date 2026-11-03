
#' Computation of the test statistic
#'
#'
#' This function implements the Computation of the test statistic T given in section 3. "Statistical tests" of "Rationalizing Rational Expectations? Tests and Deviations".
#' @param m_bar the moments m_bar for the different instrumental functions h considered
#' @param Sigma_bar the matrix of all the variances of the moments m_bar for the different instrumental functions h considered
#' @param prob_weight vector of weigths for the test statistic
#' @param N_g number of instrumental functions h considered
#' @param N_k number of moments
#' @param p the parameter p in the Statistic.
#'
#' @return a real T which is the test statistic
#' @export
#'
#' @examples
T_stat <- function(m_bar, Sigma_bar, prob_weight,N_g, N_k,p ){

  T_vec = matrix(0,N_g,1)
  for (i in 1:N_g){
    m_temp = m_bar[(N_k*(i-1)+1):(N_k*i),]
    sigma_temp = Sigma_bar[(N_k*(i-1)+1):(N_k*i),(N_k*(i-1)+1):(N_k*i)]

    T_vec[i,1] = S1(m_temp,sigma_temp,1, N_k,p)
  }
  T_vec[is.na(T_vec),1]<- 0
  statistic = prob_weight %*% T_vec #// CvM statistic

  return(statistic)
}
