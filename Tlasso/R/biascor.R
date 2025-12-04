#' Bias Correction of Sample Covariance of Residuals
#'
#' Generate a matrix of bias-corrected sample covariance of residuals (excludes diagnoal) described in \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)}.
#'
#' @param rho matrix of sample covariance of residuals (includes diagnoal), e.g., output of \code{\link{covres}}.    
#' @param Omega.list list of precision matrices of tensor, i.e., \code{Omega.list[[k]]} is the precision matrix 
#' for the kth tensor mode, \eqn{k \in \{1 , \ldots, K\}}{ 1 <= k <= K }. For example, output of \code{link{Tlasso.fit}}.  
#' @param k index of interested mode, default is 1. 
#' 
#' @details This function computes bias-corrected sample covariance of residuals (excludes diagnoal, diagnoal is zero vector). 
#' Note that output matrix excludes diagnoal while sample covariance of residuals includes diagnoal, see \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)} for details. 
#' Elements in \code{Omega.list} are true precision matrices or estimation of the true ones, the latter can be output of \code{\link{Tlasso.fit}}.
#' 
#' @return A matrix whose (i,j) entry (excludes diagnoal; diagnoal is zero vector) is bias-corrected sample covariance of the ith and jth residuals in the kth mode. See \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)} for details.  
#'
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{varcor}}, \code{\link{covres}}
#'
#' @examples
#' 
#' m.vec = c(5,5,5)  # dimensionality of a tensor 
#' n = 5   # sample size 
#' k=1 # index of interested mode
#' lambda.thm = 20*c( sqrt(log(m.vec[1])/(n*prod(m.vec))), 
#'                    sqrt(log(m.vec[2])/(n*prod(m.vec))), 
#'                    sqrt(log(m.vec[3])/(n*prod(m.vec))))
#' DATA=Trnorm(n,m.vec,type='Chain') 
#' # obersavations from tensor normal distribution
#' out.tlasso = Tlasso.fit(DATA,T=1,lambda.vec = lambda.thm)   
#' # output is a list of estimation of precision matrices
#' 
#' rho=covres(DATA, out.tlasso, k = k) 
#' # sample covariance of residuals, including diagnoal 
#' bias_rho=biascor(rho,out.tlasso,k=k)
#' bias_rho # bias-corrected sample covariance of residuals
#' # diagnoal is zero vector
#' 
#' @export
#'
biascor=function(rho, Omega.list, k=1){

  if (!is.list(Omega.list)){
    stop('argument Omega.list should be a list')
  } else if (any(!sapply(Omega.list,is.matrix))) {
    stop('argument Omega.list should be a list of precision matrices')
  }
  
  # dimensionality 
  K=length(Omega.list)
  m.vec = sapply(Omega.list,dim)[1,] # m1 , m2 , ... , mK , n
  
  if (!( (k==round(k)) & ((k>1)|(k==1) ) & ((k<K))|(k==K)) ) {
    stop('argument k should an integer between 1 and the length of argument Omega.list')
  } else if ( !is.matrix(rho)) {
    stop('argument rho should be a matrix')
  } else if (   any(!(dim(rho)==dim(Omega.list[[k]]))) ) {
    stop('argument rho should match the dimension of kth elements in Omega.list') 
  }
   
  bias_rho=matrix(0,m.vec[k],m.vec[k]) # excludes diagnoal
  # bias correction 
  
  
  for( i in 1:(m.vec[k]-1)) {
    for ( j in (i+1):m.vec[k]){   # excludes diagnoal
      # the bias term of residual covariance
      bias=rho[i,i]*signal(Omega.list,i=j, k =k)[i]+rho[j,j]*signal(Omega.list,i=i, k =k)[j-1]
      # correction
      bias_rho[j,i]=bias_rho[i,j]=rho[i,j]+bias
    }
  }
  
  return(bias_rho)
}