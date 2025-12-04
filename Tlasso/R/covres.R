#' Sample Covariance Matrix of Residuals
#'
#' Generate sample covariance matrix of residuals (includes diagnoal) described in \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)}.
#'
#' @param data tensor object stored in a m1 * m2 * ... * mK * n array, where n is sample size
#' and mk is dimension of the kth tensor mode.
#' @param Omega.list list of precision matrices of tensor, i.e., \code{Omega.list[[k]]} is precision matrix 
#' for the kth tensor mode, \eqn{k \in \{1 , \ldots, K\}}{ 1 <= k <= K }.   
#' @param k index of interested mode, default is 1.
#' 
#'
#' @details This function computes sample covariance of residuals and is the basis for support recovery procedure in \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)}. Note that output matrix includes 
#' diagnoal while bias corrected matrix (output of \code{\link{biascor}}) for inference is off-diagnoal, see \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)} for details.
#'  Elements in Omega.list are true precision matrices or estimation of the true ones, the latter can be output of \code{\link{Tlasso.fit}}.
#' 
#' @return A matrix whose (i,j) entry (includes diagnoal) is sample covariance of the ith and jth residuals in the kth mode. See \href{https://arxiv.org/abs/1609.04522}{Lyu et al. (2019)} for details.  
#'
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{varcor}}, \code{\link{biascor}}
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
#' rho=covres(DATA, out.tlasso, k = k) # sample covariance of residuals, including diagnoal 
#' rho
#' 
#' @export
#'
#' @import rTensor
#'
#'
covres = function(data, Omega.list, k = 1) {
  
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
    stop('the length of arguments Omega.list should be the same as the number of modes')
  } else if ( any(!(sapply(Omega.list,dim)[1,]==m.vec)) ) {
    stop('dimension of elements in argument Omega.list should match argument data')
  } else if (!( (k==round(k)) & ((k>1)|(k==1) ) & ((k<K))|(k==K)) ) {
    stop('argument k should an integer between 1 and the length of argument Omega.list')
  }

  # compute the mean of n observation
  ten_bar=apply(data,1:K,mean)

  rho=matrix(0,m.vec[k],m.vec[k])
  for ( i in 1:m.vec[k]) {
    for ( j in i:m.vec[k]){ #includes the diagnoal
      for( l in 1:n) {
        diff=0
        eval(parse(text=paste('diff=data[',paste(rep(',',K),collapse=''),'l]')))
        diff = diff-ten_bar # difference of Tl - Tbar
        
        diff_ki=0;diff_kj=0;diff_kirest=0;diff_kjrest=0
        eval(parse(text=paste('diff_ki=diff[',paste(rep(',',k-1),collapse=''),'i',
                              paste(rep(',',K-k),collapse=''),']')))
        eval(parse(text=paste('diff_kj=diff[',paste(rep(',',k-1),collapse=''),'j',
                              paste(rep(',',K-k),collapse=''),']')))
        eval(parse(text=paste('diff_kirest=diff[',paste(rep(',',k-1),collapse=''),'-i',
                              paste(rep(',',K-k),collapse=''),']')))
        eval(parse(text=paste('diff_kjrest=diff[',paste(rep(',',k-1),collapse=''),'-j',
                              paste(rep(',',K-k),collapse=''),']')))
        xsignali=ttm(as.tensor(diff_kirest),t(as.matrix(signal(Omega.list,i=i, k =k))),m=k)@data
        dim(xsignali)=dim(xsignali)[-k]
        # compute the residual defined in Sun et al. 2016
        xi_li=diff_ki - xsignali # the ith residual of kth mode in lth observation

        xsignalj=ttm(as.tensor(diff_kjrest),t(as.matrix(signal(Omega.list,i=j, k =k))),m=k)@data
        dim(xsignalj)=dim(xsignalj)[-k]
        # compute the residual defined in Sun et al. 2016
        xi_lj=diff_kj - xsignalj # the jth residual of kth mode in lth observation

        #summation over n observations and rest K-1 mode
        rho[i,j]=rho[i,j]+sum(xi_li * xi_lj)
      }
      rho[j,i]=rho[i,j]
    }
  }

  # normalization
  rho=rho/(prod(m.vec[-k])*(n-1))

  return(rho)
}

