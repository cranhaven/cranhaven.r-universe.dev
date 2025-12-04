#' Inference Performance Measures
#'
#' False positive, false negative, discoveries, and non-discoveries of inference for sparse tensor graphical models.
#'
#' @param mat.list list of matrices. (i,j) entry in its kth element is test statistic 
#' value for (i,j) entry of kth true precision matrix.
#' @param Omega.true.list list of true precision matrices of tensor, i.e., \code{Omega.true.list[[k]]} is true precision matrix 
#' for the kth tensor mode, \eqn{k \in \{1 , \ldots, K\}}{ 1 <= k <= K }.
#' @param critical critical level of rejecting null hypothesis. If \code{critical} is not positive, all null hypothesis will not be rejected.
#' @param offdiag logical; indicate if excludes diagnoal when computing performance measures. 
#' If \code{offdiag = TRUE}, diagnoal in each matrix is ingored 
#' when comparing two matrices. Default is \code{TRUE}.
#' 
#' @details This function computes performance measures of inference for sparse tensor graphical models. 
#' False positive, false negative, discovery (number of rejected null hypothesis), non-discovery (number of non-rejected null hypothesis), 
#' and total non-zero entries of each true precision matrix is listed in output.
#' 
#' @return A list, named \code{Out}, of following performance measures:
#' \tabular{ll}{
#'  \code{Out$fp}  \tab  vector; number of false positive of each mode \cr
#'  \code{Out$fn}  \tab  vector; number of false negative of each mode \cr
#'  \code{Out$d}  \tab   vector; number of all discovery of each mode \cr
#'  \code{Out$nd}  \tab  vector; number of all non-discovery of each mode \cr 
#'  \code{Out$t}  \tab   vector; number of all true non-zero entries in true precision matrix of each mode \cr
#' }
#' 
#' 
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{Tlasso.fit}}, \code{\link{est.analysis}}, \code{\link{ChainOmega}}
#'
#' @examples
#' 
#' m.vec = c(5,5,5)  # dimensionality of a tensor 
#' n = 5   # sample size 
#' Omega.true.list = list()
#' Omega.true.list[[1]] = ChainOmega(m.vec[1], sd = 1)
#' Omega.true.list[[2]] = ChainOmega(m.vec[2], sd = 2)
#' Omega.true.list[[3]] = ChainOmega(m.vec[3], sd = 3)
#' lambda.thm = 20*c( sqrt(log(m.vec[1])/(n*prod(m.vec))), 
#'                    sqrt(log(m.vec[2])/(n*prod(m.vec))), 
#'                    sqrt(log(m.vec[3])/(n*prod(m.vec))))
#' DATA=Trnorm(n,m.vec,type='Chain') 
#' # obersavations from tensor normal distribution
#' out.tlasso = Tlasso.fit(DATA,T=1,lambda.vec = lambda.thm)   
#' # output is a list of estimation of precision matrices
#' mat.list=list()
#' for ( k in 1:3) {
#'   rho=covres(DATA, out.tlasso, k = k) 
#'   # sample covariance of residuals, including diagnoal 
#'   varpi2=varcor(DATA, out.tlasso, k = k)
#'   # variance correction term for kth mode's sample covariance of residuals
#'   bias_rho=biascor(rho,out.tlasso,k=k)
#'   # bias corrected 
#'   
#'   tautest=matrix(0,m.vec[k],m.vec[k])
#'   for( i in 1:(m.vec[k]-1)) {
#'     for ( j in (i+1):m.vec[k]){
#'       tautest[j,i]=tautest[i,j]=sqrt((n-1)*prod(m.vec[-k]))*
#'         bias_rho[i,j]/sqrt(varpi2*rho[i,i]*rho[j,j])
#'     }
#'   }
#'   # list of matrices of test statistic values (off-diagnoal). See Sun et al. 2016
#'   mat.list[[k]]=tautest
#' }
#' 
#' infer.analysis(mat.list, qnorm(0.975), Omega.true.list, offdiag=TRUE)
#' # inference measures (off-diagnoal) 
#' 
#' @export
#'
infer.analysis=function(mat.list, critical, Omega.true.list, offdiag=TRUE)  {
  
  if ( !is.list(mat.list)){
    stop('argument mat.list should be a list')
  } else if (!is.list(Omega.true.list)) {
    stop('argument Omega.true.list should be a list')
  } else if (any(!sapply(mat.list,is.matrix))) {
    stop('argument mat.list should be a list of matrices')
  } else if (any(!sapply(Omega.true.list,is.matrix))) {
    stop('argument Omega.true.list should be a list of matrices')
  } else if (  length(mat.list)!=length(Omega.true.list) ) {
    stop('arguments mat.list and Omega.true.list should share the same length')
  } else if ( any(!(sapply(mat.list,dim)[1,]==sapply(Omega.true.list,dim)[1,])))  {
    stop('dimension of elements in argument mat.list should match argument Omega.true.list') 
  } else if ( !is.logical(offdiag)) {
    stop('argument offdiag should be a logical TRUE or FALSE ')
  } 
  
  K=length(mat.list)
  fp=c();fn=c();d=c();nd=c();t=c()
  
  for (i in 1:K) {
    mat.list[[i]]=sign(abs(mat.list[[i]])>critical)
    
    if (offdiag==TRUE) {
      diag(mat.list[[i]])=NA
      diag(Omega.true.list[[i]])=NA
    }
    
    fp[i]=length(intersect(which(mat.list[[i]] !=0 ), which(Omega.true.list[[i]] ==0)))
    fn[i] =length(intersect(which(mat.list[[i]] ==0 ), which(Omega.true.list[[i]] !=0)))
    d[i] =length(which(mat.list[[i]] !=0 ))
    nd[i] =length(which(mat.list[[i]] ==0 ))
    t[i] = length(which(Omega.true.list[[i]] !=0 ))  
  }
  
  Out = list()
  Out$fp=fp
  Out$fn=fn
  Out$d=d
  Out$nd=nd
  Out$t=t
  
  return(Out)
}