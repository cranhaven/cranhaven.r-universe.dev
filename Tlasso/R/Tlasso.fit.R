#' Non-Convex Optimization for Sparse Tensor Graphical Models
#'
#' An alternating optimization algorithm for estimation of precision matrices of sparse tensor graphical models. See \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)} for details.
#' 
#' @param data tensor object stored in a m1 * m2 * ... * mK * n array, where n is sample size
#' and mk is dimension of the kth tensor mode.
#' @param T number of maximal iteration, default is 1. Each iteration involves update on all modes. 
#' If output change less than \code{thres} after certain iteration, in terms of summation on Frobenius norm, this function will be terminated (before Tth iteration).
#' @param lambda.vec vector of tuning parameters (\eqn{\lambda_1}{\lambda_1},...,\eqn{\lambda_K}{\lambda_K}). Defalut is NULL, s.t. it is tuned via \code{HUGE} package directly.
#' @param norm.type normalization method of precision matrix, i.e., \eqn{\Omega_{11} = 1}{\Omega_{11}=1} 
#' if norm.type = 1 and \eqn{\|\Omega\|_{F}=1}{||\Omega||_F =1 } if norm.type = 2. Default value is 2.  
#' @param thres thresholding value that terminates algorithm before Tth iteration if output change less than \code{thres} after certain iteration, in terms of summation over Frobenius norm. 
#' If \code{thres} is negative or zero, this algorithm will iterate T times.
#' 
#' @details This function conducts an alternating optimization algorithm to sparse tensor graphical model. The output is optimal consistent even when \code{T=1}, see \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)} for details.
#' There are two ternimation criteria, \code{T} and \code{thres}. Algorithm will be terminated if output in certain iteration change less than \code{thres}. Otherwise, T iterations will be fully operated.
#' 
#' @return A length-K list of estimation of precision matrices.  
#'
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{varcor}}, \code{\link{biascor}}, \code{\link{huge}}
#'
#' @examples
#' 
#' m.vec = c(5,5,5)  # dimensionality of a tensor 
#' n = 5   # sample size 
#' lambda.thm = 20*c( sqrt(log(m.vec[1])/(n*prod(m.vec))), 
#'                   sqrt(log(m.vec[2])/(n*prod(m.vec))), 
#'                   sqrt(log(m.vec[3])/(n*prod(m.vec))))
#' DATA=Trnorm(n,m.vec,type='Chain') 
#' # obersavations from tensor normal distribution
#' out.tlasso = Tlasso.fit(DATA,T=10,lambda.vec = lambda.thm,thres=10)   
#' # terminate by thres
#' out.tlasso = Tlasso.fit(DATA,T=3,lambda.vec = lambda.thm,thres=0)   
#' # thres=0, iterate 10 times
#' 
#'
#' @export 
#'
#' @import rTensor 
#'          huge
#'          expm
#'
#'


Tlasso.fit <- function(data, T = 1, lambda.vec = NULL, norm.type = 2, thres=1e-5)  {
  
  if (!is.array(data)){
    stop('argument data should be an array')
  } 
  
  dim.vec = dim(data) # m1 , m2 , ... , mK , n
  K = length(dim.vec) - 1
  
  if (  length(dim.vec)<2   ) {
    stop('argument data should be at least a 2-dimension array')
  }
  
  n = as.integer(dim.vec[K+1])
  m.vec = dim.vec[1:K]
  
  if (!(norm.type==1 | norm.type==2)){
    stop('argument norm.type should be 1 or 2')
  } else if (!(is.vector(lambda.vec)&(length(lambda.vec)==length(m.vec)))) {
    stop('argument lambda.vec should be a vector that matches the dimension of argument data')
  } else if (!( (T==round(T)) & ((T>1)|(T==1) )) ) {
    stop('argument T should be a positive integer')
  } 
  
  # initial value of precision matrices
  Omega.list=list()
  for (k in 1:K) {
    Omega.list[[k]] = diag(m.vec[k])
  }
  # the square root of precision matrix
  # in initialization, Omega.list.sqrt and Omega.list are the same
  Omega.list.sqrt=Omega.list

  # iterate T times
  for (iter in 1:T) {
  # update from 1st to Kth mode
    Omega.list.old=Omega.list # store an old list for termination after current iteration
    for(k in 1:K){


      # compute the estimation of covariance matrix to construct likelihood

      S.array = array(0,c(m.vec[k],m.vec[k],n))
      for(i in 1:n){
        # set k-th matrix into identity
        Omega.list.sqrt[[k]]=diag(m.vec[k])
        # the ith observation
        
        d=0
        eval(parse(text=paste('d=data[',paste(rep(',',K),collapse=''),'i]')))
        
        Vi = k_unfold( as.tensor(ttl( as.tensor(d) , Omega.list.sqrt , ms=1:K)@data) ,m=k)@data
        S.array[,,i] = Vi %*% t(Vi)
      }

      S.mat = apply(S.array,c(1,2),mean) * m.vec[k] / prod(m.vec)

      # optimaze the penalized likelihood with rest K-1 precision fixed fixed
      if(is.null(lambda.vec) == FALSE){
        Out1 = huge(S.mat, lambda = lambda.vec[k], method = "glasso",verbose = FALSE)
      }else{
        Out1 = huge(S.mat, method = "glasso",verbose = FALSE)
      }

      # normalize matrix and stored into Omega.list
      if(norm.type == 2){
        Omega.list[[k]] = as.matrix(Out1$icov[[1]]) / norm(as.matrix(Out1$icov[[1]]),type='F')
      }else if(norm.type == 1){
        Omega.list[[k]] = as.matrix(Out1$icov[[1]]) / as.matrix(Out1$icov[[1]])[1,1]
      }
      
      # store the square root of kth precision matrix for likelihood in next update
      Omega.list.sqrt[[k]]=sqrtm(Omega.list[[k]])

    }
    
    diff=0
    for (i in 1:K){
      diff=diff+norm(Omega.list.old[[i]]-Omega.list[[i]],type='F')
    }
    if ( diff < thres ) {
      print('Output changes less than arguement thres after certain iteration. Terminate the algorithm.')
      return(Omega.list)
    }
  }  
  return(Omega.list)
}
