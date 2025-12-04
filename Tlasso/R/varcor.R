#' Variance Correction of Sample Covariance of Residuals
#'
#' Generate variance correction term of sample covariance of residuals described in \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)}.
#'
#' @param data tensor object stored in a m1 * m2 * ... * mK * n array, where n is sample size
#' and mk is dimension of the kth tensor mode.
#' @param Omega.list list of precision matrices of tensor, i.e., \code{Omega.list[[k]]} is precision matrix 
#' for the kth tensor mode, \eqn{k \in \{1 , \ldots, K\}}{ 1 <= k <= K }. 
#' Elements in \code{Omega.list} are true precision matrices or estimation of the 
#' true ones, the latter can be output of \code{Tlasso.fit}.
#' @param k index of interested mode, default is 1.
#' 
#' @details This function computes variance correction term of sample covariance of residuals and
#'  is utilized to normalize test statistic into standord normal, see \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)}.  
#' 
#' @return A scalar of variance correction for the kth mode.  
#'
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{varcor}}, \code{\link{biascor}}, \code{\link{covres}}
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
#' varpi2=varcor(DATA, out.tlasso, k = k)
#' # variance correction term for kth mode's sample covariance of residuals
#' 
#' @export
#'
#' @import expm 
#'         rTensor
#'
#'
varcor=function(data, Omega.list, k = 1){
  
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
  
  if (!is.list(Omega.list) ){
    stop('argument Omega.list should be a list')
  } else if (any(!sapply(Omega.list,is.matrix))) {
    stop('argument Omega.list should be a list of precision matrices')
  } else if (  length(Omega.list)!=length(m.vec) ) {
    stop('length of argument Omega.list should be the same as number of modes')
  } else if ( any(!(sapply(Omega.list,dim)[1,]==m.vec)) ) {
    stop('dimension of elements in argument Omega.list should match argument data')
  } else if (!( (k==round(k)) & ((k>1)|(k==1) ) & ((k<K))|(k==K)) ) {
    stop('argument k should an integer between 1 and the length of argument Omega.list')
  }

  # the square root of precision matrices
  Omega.list.sqrt=list()
  for (kk in 1:K) {
    Omega.list.sqrt[[kk]]=sqrtm(Omega.list[[kk]])
  }

  # compute the list of the estimation of covariance matrices
  S.list=list()
  for(kk in 1:K){
    # the kth covariance matrix is no required
    if (kk==k) {
      S.list[[kk]]=1
      next
    }

    S.array = array(0,c(m.vec[kk],m.vec[kk],n))
    Omega.list.sqrtk=Omega.list.sqrt
    Omega.list.sqrtk[[kk]]=diag(m.vec[kk])
    # construction of estimation
    for(i in 1:n){
      d=0
      eval(parse(text=paste('d=data[',paste(rep(',',K),collapse=''),'i]')))
      Vi = k_unfold( as.tensor(ttl( as.tensor(d) , Omega.list.sqrtk , ms=1:K)@data) ,m=kk)@data
      S.array[,,i] = Vi %*% t(Vi)
    }
    #average over sample size
    S.mat = apply(S.array,c(1,2),mean) * m.vec[k] / prod(m.vec)
    S.list[[kk]]=S.mat
  }

  # compute the variance correction term based on covariance estimations
  varpi2=1
  for (kk in 1:K){
    if (kk==k) next
    varpi2 = varpi2 * norm(S.list[[kk]],type='F')^2 / sum(diag(S.list[[kk]]))^2
  }
  varpi2=varpi2*prod(m.vec[-k])

  return(varpi2)
}

