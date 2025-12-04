#' Separable Tensor Normal Distribution
#'
#' Generate observations from separable tensor normal distribution.
#'
#' @param n number of generated observations.
#' @param m.vec vector of tensor mode dimensions, e.g., \code{m.vec=c(m1, m2, m3)} for a 3-mode tensor normal distribution.  
#' @param mu array of mean for tensor normal distribution with dimension \code{m.vec}. Default is zero mean. 
#' @param Sigma.list list of covariance matrices in mode sequence. Default is \code{NULL}.
#' @param sd seed of random number generation, default is 1.
#' @param type type of precision matrix, default is 'Chain'. Optional values are 'Chain' for 
#' triangle graph and 'Neighbor' for nearest-neighbor graph. Useless if \code{Sigma.list} is not \code{NULL}.
#' @param knn sparsity of precision matrix, i.e., matrix is generated from a \code{knn} nearest-neighbor graph. 
#' Default is 4. Useless if \code{type='Chain'} or \code{Sigma.list} is not \code{NULL}.
#' @param norm.type normalization method of precision matrix, i.e., \eqn{\Omega_{11} = 1}{\Omega_{11}=1} 
#' if norm.type = 1 and \eqn{\|\Omega\|_{F}=1}{||\Omega||_F =1 } if norm.type = 2. Default value is 2.  
#'
#'
#'
#'
#' @details This function generates obeservations from separable tensor normal distribution and returns a \code{m1 * ... * mK * n} array. 
#' If \code{Sigma.list} is not given, default distribution is from either triangle graph or nearest-neighbor graph (depends on \code{type}).
#'
#' 
#' @return An array with dimension m_1 * ... * m_K * n.
#'
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{ChainOmega}}, \code{\link{NeighborOmega}}
#'
#' @examples
#'  
#' m.vec = c(5,5,5)  # dimensionality of a tensor 
#' n = 5   # sample size 
#' DATA=Trnorm(n,m.vec,type='Chain') 
#' # a 5*5*5*10 array of oberservation from 5*5*5 separable tensor 
#' #     normal distribtuion with mean zero and 
#' #         precision matrices from triangle graph
#' 
#' @export
#' 
#' @importFrom stats rnorm
#'

Trnorm = function(n, m.vec, mu=array(0,m.vec), Sigma.list=NULL, type='Chain', sd = 1, knn=4, norm.type=2){
  
  if (!is.array(mu)){
    stop('argument mu should be an array') 
  } else if (any(dim(mu)!=m.vec)){
    stop('dimension of argument mu does not match argument m.vec')
  } else if (!(norm.type==1 | norm.type==2)){
    stop('argument norm.type should be 1 or 2')
  } else if ( !((knn==round(knn))&((knn>1)|(knn==1) )) ){
    stop('argument knn should be a positive integer')
  } else if ( !((n==round(n))&((n>1)|(n==1) )) ) {
    stop('argument n should be a positive integer')
  } else if (!is.vector(m.vec)) {
    stop('argument m.vec should be a vector')
  } else if ( !is.null(Sigma.list) ) {
    if ( !is.list(Sigma.list) ) {
      stop('argument Sigma.list should be a list or NULL.')
    } else if ( any(!sapply(Sigma.list,is.matrix)) ) {
      stop('argument Sigma.list should be a list of covariance matrices')
    } else if (  length(Sigma.list)!=length(m.vec) ) {
      stop('argument Sigma.list should have the same length as m.vec')
    } else if ( any(!(sapply(Sigma.list,dim)[1,]==m.vec)) ) {
      stop('dimension of elements in argument Omega.list should match argument data')
    } 
    
  }
    
  
  K = length(m.vec)
  
  if (is.null(Sigma.list)) {
    Omega.list=list();Sigma.list=list();
    
    for ( k in 1:K) {
  
      if (type=='Chain') {
        Omega.list[[k]] = ChainOmega(m.vec[k], sd = k*100,norm.type=norm.type)
      } else if (type == 'Neighbor'){
        Omega.list[[k]] = NeighborOmega(m.vec[k], sd = k*100, knn=knn, norm.type=norm.type)
      } else {
        stop('Please input a correct type')
      }
      Sigma.list[[k]] = solve(Omega.list[[k]])
    }
  } 
  
  
  kcov=1

  for (k in K:1){
    kcov=kronecker(kcov, Sigma.list[[k]])
  }

  Data = array(0,c(m.vec,n))

  set.seed(sd)
  
  ncols = ncol(kcov)
  #mu = rep(mu, each = n) ## not obliged to use a matrix (recycling)
  #mu + matrix(rnorm(n * ncols), ncol = ncols) %*% chol(sigma)
  vecdata=matrix(rnorm(n * ncols), ncol = ncols) %*% chol(kcov)
  #vecdata = mvrnorm(n,Sigma = kcov, mu = rep(0,prod(m.vec)))
  # need a fast method to generate multi-vari data
  Data = array(rep(mu,n),c(m.vec,n))+array(t(vecdata), c(m.vec,n))
  
  return(Data)

}