#' Precision Matrix of Triangle Graph
#'
#' Generate precision matrix of triangle graph (chain like network) following the set-up in \href{https://arxiv.org/abs/0908.2053}{Fan et al. (2009)}.
#'
#' @param p dimension of generated precision matrix.
#' @param sd seed for random number generation, default is 1.
#' @param norm.type normalization methods of generated precision matrix, i.e., \eqn{\Omega_{11} = 1}{\Omega_{11}=1} 
#' if norm.type = 1 and \eqn{\|\Omega\|_{F}=1}{||\Omega||_F =1 } if norm.type = 2. Default value is 2.
#'
#' @details This function first construct a covariance matrix \eqn{\Sigma}{\Sigma} that its (i,j) entry is 
#' \eqn{\exp (- | h_i - h_j |/2)}{exp (- | h_i - h_j | / 2)} with \eqn{h_1 < h_2 < \ldots < h_p}{h_1 < h_2 < \ldots < h_p}.
#' The difference \eqn{h_i - h_{i+1}}{h_i - h_{i+1}} is generated i.i.d. from Unif(0.5,1). See \href{https://arxiv.org/abs/0908.2053}{Fan et al. (2009)} 
#' for more details.
#' 
#' @return A precision matrix generated from triangle graph.
#'
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{NeighborOmega}}
#'
#' @examples
#' 
#' m.vec = c(5,5,5)  # dimensionality of a tensor 
#' n = 5   # sample size 
#' 
#' Omega.true.list = list()
#' 
#' for ( k in 1:length(m.vec)){
#'  Omega.true.list[[k]] = ChainOmega(m.vec[k],sd=k*100,norm.type=2)
#' }
#' Omega.true.list  # a list of length 3 contains precision matrices from triangle graph
#'
#' @export
#' 
#' @importFrom stats runif
#' 

ChainOmega = function(p, sd = 1, norm.type = 2){

  if (!(norm.type==1 | norm.type==2)){
    stop('argument norm.type should be 1 or 2')
  } else if ( !((p==round(p))&((p>1)|(p==1) )) ){
    stop('argument p should be a positive integer')
  } 
  
  Sigma = matrix(NA,p,p)
  set.seed(sd)
  s = rep(0, p)
  a = runif((p - 1),0.5,1)
  for(i in 1:(p - 1)){
    s[i + 1] = s[i] + a[i]
  }

  for(i in 1:p){
    for(j in 1:p){
      Sigma[i,j] = exp(-abs(s[i] - s[j]) / 2)
    }
  }

  Omega = solve(Sigma)
  Omega[which(abs(Omega)<1e-5)] = 0

  if(norm.type == 2){
    Omega = Omega/norm(Omega,type="F")
  }else if(norm.type == 1){
    Omega = Omega/Omega[1,1]
  }
  
  return(Omega)
}

