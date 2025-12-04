#' Estimation Errors and TPR/TNR 
#'
#' Compute estimation errors and TPR/TNR of optimization for sparse tensor graphical models
#'
#' @param Omega.hat.list list of estimation of precision matrices of tensor, i.e., \code{Omega.hat.list[[k]]} is estimation of precision matrix 
#' for the kth tensor mode, \eqn{k \in \{1 , \ldots, K\}}{ 1 <= k <= K }. For example, output of \code{\link{Tlasso.fit}}.
#' @param Omega.true.list list of true precision matrices of tensor, i.e., \code{Omega.true.list[[k]]} is true precision matrix 
#' for the kth tensor mode, \eqn{k \in \{1 , \ldots, K\}}{ 1 <= k <= K }.
#' @param offdiag logical; indicate if excludes diagnoal when computing performance measures. 
#' If \code{offdiag = TRUE}, diagnoal in each matrix is ingored 
#' when comparing two matrices. Default is \code{TRUE}.
#' 
#' @details This function computes performance measures of optimazation for sparse tensor graphical models. 
#' Errors are measured in Frobenius norm and Max norm. Model selection measures are TPR and TNR. All these measures are computed in each 
#' mode, average across all modes, and kronecker production of precision matrices.
#' 
#' @return A list, named \code{Out}, of following performance measures:
#' \tabular{ll}{
#' \code{Out$error.kro}  \tab  error in Frobenius norm of kronecker product \cr
#' \code{Out$tpr.kro}  \tab   TPR of kronecker product \cr
#' \code{Out$tnr.kro}  \tab     TNR of kronecker product \cr
#' \code{Out$av.error.f}  \tab  averaged Frobenius norm error across all modes \cr
#' \code{Out$av.error.max}  \tab  averaged Max norm error across all modes \cr 
#' \code{Out$av.tpr}  \tab  averaged TPR across all modes \cr
#' \code{Out$av.tnr}  \tab  averaged TNR across all modes \cr
#' \code{Out$error.f}  \tab  vector; error in Frobenius norm of each mode  \cr
#' \code{Out$error.max}  \tab  vector; error in Max norm of each mode  \cr 
#' \code{Out$tpr}  \tab  vector; TPR of each mode \cr
#' \code{Out$tnr}  \tab   vector; TNR of each mode   \cr
#' }
#' 
#' 
#' @author Xiang Lyu, Will Wei Sun, Zhaoran Wang, Han Liu, Jian Yang, Guang Cheng. 
#' @seealso \code{\link{Tlasso.fit}}, \code{\link{NeighborOmega}}, \code{\link{ChainOmega}}
#'
#' @examples
#' 
#' m.vec = c(5,5,5)  # dimensionality of a tensor 
#' n = 5   # sample size 
#' k=1 # index of interested mode
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
#' est.analysis(out.tlasso, Omega.true.list, offdiag=TRUE)
#' # generate a list of performance measures
#' 
#' @export
#'
est.analysis = function(Omega.hat.list, Omega.true.list, offdiag=TRUE){

  if ( !is.list(Omega.hat.list)){
    stop('argument Omega.hat.list should be a list')
  } else if (!is.list(Omega.true.list)) {
    stop('argument Omega.true.list should be a list')
  } else if (any(!sapply(Omega.hat.list,is.matrix))) {
    stop('argument Omega.hat.list should be a list of precision matrices')
  } else if (any(!sapply(Omega.true.list,is.matrix))) {
    stop('argument Omega.true.list should be a list of precision matrices')
  } else if (  length(Omega.hat.list)!=length(Omega.true.list) ) {
    stop('arguments Omega.hat.list and Omega.true.list should share the same length')
  } else if ( any(!(sapply(Omega.hat.list,dim)[1,]==sapply(Omega.true.list,dim)[1,])))  {
    stop('dimension of elements in argument Omega.hat.list should match argument Omega.true.list') 
  } else if ( !is.logical(offdiag)) {
    stop('argument offdiag should be a logical TRUE or FALSE ')
  }

  K = dim(as.array(Omega.hat.list))
  error.f = rep(0,K)
  error.max = rep(0,K)
  tpr = rep(0,K)
  tnr = rep(0,K)
  
  if (offdiag==FALSE) {
    
    for(i in 1:K){
      error.f[i] = norm(Omega.hat.list[[i]] - Omega.true.list[[i]], type="F")
      error.max[i] = norm(Omega.hat.list[[i]] - Omega.true.list[[i]], type="M")
      # tpr and tnr include diagnoal
      tpr[i] = length(intersect(which(Omega.hat.list[[i]] !=0 ), which(Omega.true.list[[i]] !=0))) / length(which(Omega.true.list[[i]] !=0))
      tnr[i] = length(intersect(which(Omega.hat.list[[i]] ==0 ), which(Omega.true.list[[i]] ==0))) / length(which(Omega.true.list[[i]] ==0))
    }
    
    KOmega.true=1;KOmega.hat=1
    for (k in 1:K){
      KOmega.true=kronecker(KOmega.true, Omega.true.list[[k]])
      KOmega.hat=kronecker(KOmega.hat, Omega.hat.list[[k]])
    }
    
    error.kro = norm(KOmega.hat - KOmega.true,type="F")
    tpr.kro = length(intersect(which(KOmega.hat !=0 ), which(KOmega.true !=0))) / length(which(KOmega.true !=0))
    tnr.kro = length(intersect(which(KOmega.hat ==0 ), which(KOmega.true ==0))) / length(which(KOmega.true ==0))

  } else {
    Omega.hat.list.off=Omega.hat.list
    Omega.true.list.off=Omega.true.list
    
    for(i in 1:K){
      diag(Omega.hat.list.off[[i]])=0
      diag(Omega.true.list.off[[i]])=0
      error.f[i] = norm(Omega.hat.list.off[[i]] - Omega.true.list.off[[i]], type="F")
      error.max[i] = norm(Omega.hat.list.off[[i]] - Omega.true.list.off[[i]], type="M")
      # tpr and tnr include diagnoal
      diag(Omega.hat.list.off[[i]])=NA
      diag(Omega.true.list.off[[i]])=NA
      tpr[i] = length(intersect(which(Omega.hat.list.off[[i]] !=0 ), which(Omega.true.list.off[[i]] !=0))) / length(which(Omega.true.list.off[[i]] !=0))
      tnr[i] = length(intersect(which(Omega.hat.list.off[[i]] ==0 ), which(Omega.true.list.off[[i]] ==0))) / length(which(Omega.true.list.off[[i]] ==0))
    }
    
    KOmega.true=1;KOmega.hat=1
    for (k in 1:K){
      KOmega.true=kronecker(KOmega.true, Omega.true.list[[k]])
      KOmega.hat=kronecker(KOmega.hat, Omega.hat.list[[k]])
    }
    
    diag(KOmega.hat)=0
    diag(KOmega.true)=0
    
    error.kro = norm(KOmega.hat - KOmega.true,type="F")
    
    diag(KOmega.hat)=NA
    diag(KOmega.true)=NA
    tpr.kro = length(intersect(which(KOmega.hat !=0 ), which(KOmega.true !=0))) / length(which(KOmega.true !=0))
    tnr.kro = length(intersect(which(KOmega.hat ==0 ), which(KOmega.true ==0))) / length(which(KOmega.true ==0))
  }
  
  Out = list()
  Out$error.kro =  error.kro # Error in Frobenius norm
  Out$tpr.kro = tpr.kro  # TPR of kronecker product 
  Out$tnr.kro = tnr.kro  # TNR of kronecker product
  Out$av.error.f = mean(error.f)  # average Frobenius norm error across each mode
  Out$av.error.max = mean(error.max)   # # average Max norm error across each mode
  Out$av.tpr = mean(tpr) # average TPR across each mode
  Out$av.tnr = mean(tnr) # average TNR across each mode
  Out$error.f =  error.f # Frobenius norm error of each mode 
  Out$error.max =  error.max # Max norm error of each mode 
  Out$tpr = tpr # TPR of each mode
  Out$tnr = tnr # TNR of each mode
  
  return(Out)
}
