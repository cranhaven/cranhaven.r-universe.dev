#' Structural transfer learning of non-Gaussian DAG.
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>, Xin He, and Junhui Wang
#' @references Ren, M., He X., and Wang J. (2023). Structural transfer learning of non-Gaussian DAG.
#' @usage trans.local.DAG(t.data, A.data, hardth=0.5, hardth.A=hardth, criti.val=0.01,
#'                        precision.method="glasso", precision.method.A = "CLIME",
#'                        cov.method="opt", cn.lam2=seq(1,2.5,length.out=10),
#'                        precision.refit=TRUE, ini.prec=TRUE, cut.off=TRUE,
#'                        preselect.aux=0, sel.type="L2")
#'
#' @description Structural transfer learning of non-Gaussian DAG.
#' @param t.data The target data, a n * p matrix, where n is the sample size and p is data dimension.
#' @param A.data The auxiliary data in K auxiliary domains, a list with K elements, each of which is a nk * p matrix, where nk is the sample size of the k-th auxiliary domain.
#' @param hardth The hard threshold of regression in the target domain.
#' @param hardth.A The hard threshold of regression in the auxiliary domains.
#' @param criti.val The critical value of independence test based on distance covariance, and the default setting is 0.01.
#' @param precision.method The initial method of estimating the target precision matrix, which can be selected as "CLIME" or "glasso".
#' @param precision.method.A The initial method of estimating the auxiliary precision matrices, which can be selected as "CLIME" or "glasso".
#' @param cov.method The method of aggregating K auxiliary covariance matrices, which can be selected as "size" (the sum weighted by the sample sizes), "weight" (the sum weighted by the differences), or "opt" (select the optimal one).
#' @param cn.lam2 A vector or a float value: the coefficients set in tuning parameters used to solve the target precision matrix, default is cn.lam2*sqrt( log(p) / n ).
#' @param precision.refit Whether to perform regression for re-fitting the coefficients in the precision matrix to improve estimation accuracy, after determining the non-zero elements of the precision matrix. The default is True.
#' @param ini.prec Whether to store the initial estimation of the precision matrix, and the default is True.
#' @param cut.off Whether to truncate the finally estimated coefficients in the structural equation models at threshold "hardth", and the default is True.
#' @param preselect.aux Whether to pre-select informative auxiliary domains based on the distance between initially estimated auxiliary and target parameters. The default is 0, which means that pre-selection will not be performed. If "preselect.aux" is specified as a real number greater than zero, then the threshold value is forpreselect.aux*s*sqrt( log(p) / n ).
#' @param sel.type If pre-selection should be performed, "sel.type" is the type of distance. The default is L2 norm, and can be specified as "L1" to use L1 norm.
#'
#'
#' @return A result list including:
#' \describe{
#' \item{A}{The information of layer.}
#' \item{B}{The coefficients in structural equation models.}
#' \item{prec.res0}{The results about estimating the prscision matrix via transfer learning.}
#' \item{prec.res0$Theta.hat}{The estimated prscision matrix via transfer learning.}
#' \item{prec.res0$Theta.hat0}{The estimated prscision matrix based on the target domain only.}
#' }
#' @export
#'
#' @import dcov huge clime
#'
#'
#'

trans.local.DAG = function(t.data, A.data, hardth=0.5, hardth.A=hardth, criti.val=0.01,
                          precision.method="glasso", precision.method.A = "CLIME",
                          cov.method="opt", cn.lam2=seq(1,2.5,length.out=10),
                          precision.refit=TRUE, ini.prec=TRUE, cut.off=TRUE,
                          preselect.aux=0, sel.type="L2"){

  check = FALSE
  n = nrow(t.data)
  p = ncol(t.data)
  N = nrow(A.data[[1]])
  K = length(A.data)

  A = matrix(0,p,2) # store the information of layer
  A[,1] = 1:p
  colnames(A) = c("Node","Layer")
  B = matrix(0,p,p)
  S = 1:p
  B.int = matrix(0,p,p)

  # Identify A_0 layer
  #### detect the node-level structure-informative auxiliary DAG
  # target dcov
  prec.res = Compute.precision.trans(t.data, A.data, comp.hardth=hardth,
                                     precision.method=precision.method,
                                     cov.method=cov.method, cn.lam2=cn.lam2,
                                     precision.refit = precision.refit,
                                     ini.prec=ini.prec, preselect.aux=preselect.aux,
                                     sel.type=sel.type)
  prec.res0 = prec.res
  N = prec.res0$N
  stdPreciMat.trans = prec.res$stdPreciMat.trans
  stdPreciMat.int = prec.res$stdPreciMat.int
  dcov_max.t = sapply(1:p,function(l) Compute.dcor.est.trans(l,stdPreciMat.int[,l],t.data))

  # auxiliary dcov
  dcov_max.A = matrix(0, length(S), K)
  stdPreciMat.A.list = list()
  for (k in 1:K) {
    stdPreciMat.k = Compute.precision(A.data[[k]], comp.hardth=hardth.A, precision.refit = precision.refit, precision.method=precision.method)
    dcov_max.A[,k] = sapply(1:p,function(l) Compute.dcor.est.trans(l,stdPreciMat.k[,l],A.data[[k]]))
    stdPreciMat.A.list[[k]] = stdPreciMat.k
  }
  num.aux.jk = apply(abs(dcov_max.A - dcov_max.t), 1, which.min)

  #### calculate P-value for each node based on the detected node-level structure-informative auxiliary DAG
  dcov_pvalue = sapply(1:p,function(l) Compute.dcor.test.trans(l, stdPreciMat.A.list[[num.aux.jk[l]]][,l], A.data[[num.aux.jk[l]]]))


  node_S_select=S[dcov_pvalue>criti.val]
  if (length(node_S_select)==0){
    check=TRUE
    return(list(A=A,B=B,check=check))
  }
  S=S[-node_S_select]
  B[S,node_S_select] = stdPreciMat.trans[S,node_S_select]
  t=1

  while(length(S)>1){
    len_S = length(S)
    t.data_S = t.data[,S]
    A.data_S = list()
    for(k in 1:K){
      A.data_S[[k]] = A.data[[k]][,S]
    }

    #### detect the node-level structure-informative auxiliary DAG
    # target dcov
    prec.res = Compute.precision.trans(t.data_S, A.data_S, comp.hardth=hardth,
                                       precision.method=precision.method,
                                       cov.method=cov.method, cn.lam2=cn.lam2,
                                       precision.refit = precision.refit,
                                       ini.prec=ini.prec, preselect.aux=preselect.aux,
                                       sel.type=sel.type)
    stdPreciMat.trans = prec.res$stdPreciMat.trans
    stdPreciMat.int = prec.res$stdPreciMat.int
    dcov_max.t = sapply(1:len_S,function(l) Compute.dcor.est.trans(l,stdPreciMat.int[,l],t.data_S))

    # auxiliary dcov
    dcov_max.A = matrix(0, length(S), K)
    stdPreciMat.A.list = list()
    for (k in 1:K) {
      stdPreciMat.k = Compute.precision(A.data_S[[k]], comp.hardth=hardth.A, precision.refit = precision.refit, precision.method=precision.method.A)
      dcov_max.A[,k] = sapply(1:len_S,function(l) Compute.dcor.est.trans(l,stdPreciMat.k[,l],A.data_S[[k]]))
      stdPreciMat.A.list[[k]] = stdPreciMat.k
    }
    num.aux.jk = apply(abs(dcov_max.A - dcov_max.t), 1, which.min)

    #### calculate P-value for each node based on the detected node-level structure-informative auxiliary DAG
    dcov_pvalue = sapply(1:len_S,function(l) Compute.dcor.test.trans(l, stdPreciMat.A.list[[num.aux.jk[l]]][,l], A.data_S[[num.aux.jk[l]]]))

    node_S_select = S[dcov_pvalue>criti.val]
    index_node_S_select = which(dcov_pvalue>criti.val)
    if (length(node_S_select)==0){
      node_S_select=S
    }
    A[node_S_select,2] = t
    S = setdiff(S,node_S_select)
    index_S = setdiff(c(1:len_S),index_node_S_select)
    B[S,node_S_select] = stdPreciMat.trans[index_S,index_node_S_select]

    t=t+1
  }
  if (length(S)==1){
    A[S,2]=t
  }

  k.check = prec.res0$k.check
  B = Parent.set.refit(A.data[[k.check]],B)
  if(cut.off){
    B[abs(B)<hardth]=0
  }
  return(list(A=A,B=B,check=check,prec.res0=prec.res0))
}


