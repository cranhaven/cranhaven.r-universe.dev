#' Transfer learning for tensor graphical models.
#'
#' @author Mingyang Ren, Yaoming Zhen, Junhui Wang. Maintainer: Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M., Zhen Y., and Wang J. (2022). Transfer learning for tensor graphical models.
#' @usage tensor.GGM.trans(t.data, A.data, A.lambda, A.orac = NULL, c=0.6,
#'                         t.lambda.int.trans=NULL, t.lambda.int.aggr=NULL,
#'                         theta.algm="cd", cov.select="inverse",
#'                         cov.select.agg.size = "inverse",
#'                         cov.select.agg.diff = "tensor.prod",
#'                         symmetric = TRUE, init.method="Tlasso",
#'                         init.method.aux="Tlasso", mode.set = NULL,
#'                         init.iter.Tlasso=2, cn.lam2=seq(0.1,2,length.out =10),
#'                         c.lam.Tlasso=20, c.lam.sepa=20, adjust.BIC=FALSE,
#'                         normalize = TRUE, inti.the=TRUE, sel.ind="fit")
#'
#' @description The main function for Transfer learning for tensor graphical models.
#' @param t.data The tensor data in the target domain, a p1 * p2 * ... * pM * n array, where n is the sample size and pm is dimension of the m-th tensor mode. M should be larger than 2.
#' @param A.data The tensor data in auxiliary domains, a list with K elements, each of which is a p1 * p2 * ... * pM * nk array, where nk is the sample size of the k-th auxiliary domain.
#' @param A.lambda The tuning parameters used for initialization in auxiliary domains, a list with K elements, each of which is a M-dimensional vector corresponding to M modes.
#' @param A.orac The set of informative auxiliary domains, and the default setting is NULL, which means that no set is specified.
#' @param c The c of subjects in the target domain are used for initialization of the transfer learning, and the remaining 1-c of subjects are used for the model selection step. The default setting is 0.8.
#' @param t.lambda.int.trans The tuning parameters used for initialization in the target domain (based on c subjects used for transfer learning), that is, the tuning lambda for Tlasso (PAMI, 2020) & Separable method (JCGS, 2022)
#' @param t.lambda.int.aggr The tuning parameters used for initialization in the target domain (based on 1-c subjects used for the model selection step).
#' @param theta.algm The optimization algorithm used to solve \eqn{\widehat{\Omega}}{\widehat{\Omega}} in step 2(b), which can be selected as "admm" (ADMM algorithm) or "cd" (coordinate descent).
#' @param cov.select Methods used to calculate covariance matrices for initialization in both target and auxiliary domains, which can be selected as "tensor.prod" (tensor product based on tensor subject and the initial estimate of the precision matrix, TPAMI, 2020) and "inverse" (direct inversion of the initial estimate of the precision matrix)
#' @param cov.select.agg.size Methods used to calculate covariance matrices for model selection step in the target domain.
#' @param cov.select.agg.diff Methods used to calculate covariance matrices for model selection step in the target domain.
#' @param symmetric Whether to symmetrize the final estimated precision matrices, and the default is True.
#' @param init.method The initialization method for tensor precision matrices in the target domain, which can be selected as "Tlasso" (PAMI, 2020) & "sepa" (Separable method, JCGS, 2022). Note that the "sepa" method has not been included in the current version of this R package to circumvent code ownership issues.
#' @param init.method.aux The initialization method for tensor precision matrices in auxiliary domains.
#' @param mode.set Whether to estimate only the specified mode, and the default setting is NULL, which means estimating all mode.
#' @param init.iter.Tlasso The number of maximal iteration when using Tlasso for initialization, default is 2.
#' @param cn.lam2 The coefficient set in tuning parameters used to solve \eqn{\widehat{\Omega}}{\widehat{\Omega}} in step 2(b), default is seq(0.1,1,length.out =10).
#' @param c.lam.Tlasso The coefficient in tuning parameters for initialization (when using Tlasso): \eqn{c.lam.Tlasso * \sqrt( pm * \log(pm)/( n*p1*...*pM ))}{c.lam.sepa * \sqrt( pm * \log(pm)/( n*p1*...*pM ))}, default is 20 suggested in (PAMI, 2020).
#' @param c.lam.sepa The coefficient in tuning parameters for initialization (when using sepa): \eqn{c.lam.sepa * \sqrt( pm * \log(pm)/( n*p1*...*pM ))}{c.lam.sepa * \sqrt( pm * \log(pm)/( n*p1*...*pM ))}.
#' @param adjust.BIC Whether to use the adjusted BIC to select lambda2, the default setting is F.
#' @param normalize The normalization method of precision matrix. When using Tlasso, \eqn{\Omega_{11} = 1}{\Omega_{11}=1} if normalize = F and \eqn{\| \Omega_{11} \|_{F} = 1}{\| \Omega_{11} \|_F = 1} if normalize = T. Default value is T.
#' @param inti.the T: the initial values in Step 2(b) is Omega0.
#' @param sel.ind The approach to model selection, which can be selected from c("fit", "predict").
#'
#' @return A result list including:
#' Omega.list: the final estimation result of the target precision matrices after the model selection of transfer learning-based estimation and initial estimation (in which the initial covariance matrices of auxiliary domains is weighted by sample sizes);
#' Omega.sym.list: the symmetrized final estimation result in Omega.list;
#' Omega.list.diff: the final estimation result of the target precision matrices after the model selection of transfer learning-based estimation and initial estimation (in which the initial covariance matrices of auxiliary domains is weighted by the differences with the target domain);
#' Omega.sym.list.diff: the symmetrized final estimation result in Omega.list.diff;
#' res.trans.list: transfer learning-based estimation results.
#' @export
#'
#' @import rTensor Tlasso glasso doParallel expm
#' @importFrom stats cov
#'
#' @examples
#' \donttest{
#' library(TransTGGM)
#' library(Tlasso)
#' data(example.data)
#' t.data = example.data$t.data
#' A.data = example.data$A.data
#' t.Omega.true.list = example.data$t.Omega.true.list
#' normalize = TRUE
#'
#' K = length(A.data)
#' p.vec = dim(t.data)
#' M = length(p.vec) - 1
#' n = p.vec[M+1]
#' p.vec = p.vec[1:M]
#' tla.lambda = 20*sqrt( p.vec*log(p.vec) / ( n * prod(p.vec) ))
#' A.lambda = list()
#' for (k in 1:K) {
#'   A.lambda[[k]] = 20*sqrt( log(p.vec) / ( dim(A.data[[k]])[M+1] * prod(p.vec) ))
#' }
#'
#' res.final = tensor.GGM.trans(t.data, A.data, A.lambda, normalize = normalize)
#' Tlasso.Omega.list = Tlasso.fit(t.data, lambda.vec = tla.lambda,
#'                     norm.type = 1+as.numeric(normalize))
#'
#' i.Omega = as.data.frame(t(unlist(est.analysis(res.final$Omega.list, t.Omega.true.list))))
#' i.Omega.diff = t(unlist(est.analysis(res.final$Omega.list.diff, t.Omega.true.list)))
#' i.Omega.diff = as.data.frame(i.Omega.diff)
#' i.Tlasso = as.data.frame(t(unlist(est.analysis(Tlasso.Omega.list, t.Omega.true.list))))
#' i.Omega.diff     # proposed.v
#' i.Omega          # proposed
#' i.Tlasso         # Tlasso
#' }
#'
#'
#'
#'
tensor.GGM.trans = function(t.data, A.data, A.lambda, A.orac = NULL, c=0.6,
                            t.lambda.int.trans=NULL, t.lambda.int.aggr=NULL,
                            theta.algm="cd", cov.select="inverse",
                            cov.select.agg.size = "inverse",
                            cov.select.agg.diff = "tensor.prod",
                            symmetric = TRUE, init.method="Tlasso",
                            init.method.aux="Tlasso", mode.set = NULL,
                            init.iter.Tlasso=2, cn.lam2=seq(0.1,2,length.out =10),
                            c.lam.Tlasso=20, c.lam.sepa=20, adjust.BIC=FALSE,
                            normalize = TRUE, inti.the=TRUE, sel.ind="fit"){

  p.vec = dim(t.data)
  M = length(p.vec) - 1
  if(M < 2){
    warning("Error: M is less than 2!")
  }
  n = p.vec[M+1]
  p.vec = p.vec[1:M]

  if(is.null(mode.set)){
    mode.set = 1:M
  } else {
    mode.set = mode.set
  }

  # split the target data
  nc = n*c
  t.data.tran=0
  eval(parse(text=paste('t.data.tran=t.data[',paste(rep(',',M),collapse=''),'1:floor(nc)]')))
  t.data.aggr=0
  eval(parse(text=paste('t.data.aggr=t.data[',paste(rep(',',M),collapse=''),'setdiff(1:n,1:floor(nc))]')))


  res.trans = trans.estimate(t.data.tran, A.data, A.lambda, A.orac = A.orac,
                             t.lambda.int=t.lambda.int.trans, adjust.BIC=adjust.BIC,
                             mode.set = mode.set, init.iter=init.iter.Tlasso,
                             init.method=init.method, init.method.aux=init.method.aux,
                             theta.algm=theta.algm, cov.select=cov.select,
                             c.lam.sepa=c.lam.sepa, c.lam.Tlasso=c.lam.Tlasso,
                             cn.lam2=cn.lam2, normalize = normalize,
                             inti.the=inti.the)
  # initial precision matrices of all modes in the target domain
  t.Omega.hat.list = res.trans$t.Omega.hat.list

  # transfer learning-based precision matrices of all modes
  # using the weight (in Sigma.A.m) determined by the sample sizes
  Theta.hat.list.trans = res.trans$Theta.hat.list
  # using the weight (in Sigma.A.m) determined by the differences
  Theta.hat.list.trans.diff = res.trans$Theta.hat.diff.list

  if(sel.ind == "predict"){
    n.a = dim(t.data.aggr)[M+1]
    X = matrix(0, nrow = n.a, ncol = prod(p.vec))
    for (i in 1:n.a) {
      t.data.aggr.i=0
      eval(parse(text=paste('t.data.aggr.i=t.data.aggr[',paste(rep(',',M),collapse=''),'i]')))
      t.data.i.vec = as.vector(t.data.aggr.i)
      X[i,] = t.data.i.vec
    }
    Sigma.X = cov(X)

    KOmega.hat=1; KOmega.hat.diff=1; KOmega.hat0 = 1;
    for (m in M:1){
      KOmega.hat0 = kronecker(KOmega.hat0, t.Omega.hat.list[[m]])
      # Theta.hat using the weight (in Sigma.A.m) determined by the sample sizes
      KOmega.hat = kronecker(KOmega.hat, Theta.hat.list.trans[[m]])
      # Theta.hat using the weight (in Sigma.A.m) determined by the differences
      KOmega.hat.diff = kronecker(KOmega.hat.diff, Theta.hat.list.trans.diff[[m]])
    }

    pre0 = sum(diag(Sigma.X %*% KOmega.hat0)) - log(det(KOmega.hat0))
    pre1 = sum(diag(Sigma.X %*% KOmega.hat)) - log(det(KOmega.hat))
    pre1.diff = sum(diag(Sigma.X %*% KOmega.hat.diff)) - log(det(KOmega.hat.diff))


    # Theta.hat using the weight (in Sigma.A.m) determined by the sample sizes
    W.list = which.min(c(pre0, pre1))
    if(W.list == 2){
      Omega.hat.final.list = Theta.hat.list.trans
    } else {
      Omega.hat.final.list = t.Omega.hat.list
    }
    Omega.hat.final.sym.list = Omega.hat.final.list
    for (m in 1:M) {
      Omega.hat.final.sym.list[[m]] = Omega.hat.final.list[[m]]
    }

    # Theta.hat using the weight (in Sigma.A.m) determined by the differences
    W.list.diff = which.min(c(pre0, pre1.diff))
    if(W.list == 2){
      Omega.hat.final.list.diff = Theta.hat.list.trans.diff
    } else {
      Omega.hat.final.list.diff = t.Omega.hat.list
    }
    Omega.hat.final.sym.list.diff = Omega.hat.final.list.diff
    for (m in 1:M) {
      Omega.hat.final.sym.list.diff[[m]] = Omega.hat.final.list.diff[[m]]
    }

  }

  if(sel.ind == "fit"){
    # covariance matrices for aggregation & Theta.hat using the weight (in Sigma.A.m) determined by the sample sizes
    init.aggr = Initial.aggr(t.data.aggr, t.lambda.int=t.lambda.int.aggr,
                             method = init.method, cov.select=cov.select.agg.size,
                             c.lam.sepa=c.lam.sepa, c.lam.Tlasso=c.lam.Tlasso,
                             normalize = normalize)
    t.sigma.tilde.list = init.aggr$t.S.hat.list

    Omega.res.list = select.1(t.sigma.tilde.list, t.Omega.hat.list, Theta.hat.list.trans, mode.set)
    Omega.hat.final.list = Omega.res.list$Omega.hat.final.list
    Omega.hat.final.sym.list = Omega.res.list$Omega.hat.final.sym.list
    W.list = Omega.res.list$W.list

    # covariance matrices for aggregation & Theta.hat using the weight (in Sigma.A.m) determined by the differences
    init.aggr = Initial.aggr(t.data.aggr, t.lambda.int=t.lambda.int.aggr,
                             method = init.method, cov.select=cov.select.agg.diff,
                             c.lam.sepa=c.lam.sepa, c.lam.Tlasso=c.lam.Tlasso,
                             normalize = normalize)
    t.sigma.tilde.list = init.aggr$t.S.hat.list

    Omega.res.list.diff = select.1(t.sigma.tilde.list, t.Omega.hat.list, Theta.hat.list.trans.diff, mode.set)
    Omega.hat.final.list.diff = Omega.res.list.diff$Omega.hat.final.list
    Omega.hat.final.sym.list.diff = Omega.res.list.diff$Omega.hat.final.sym.list
    W.list.diff = Omega.res.list.diff$W.list
  }

  tensor.GGM.trans.res = list(Omega.list = Omega.hat.final.list,
                              Omega.sym.list = Omega.hat.final.sym.list,
                              Omega.list.diff = Omega.hat.final.list.diff,
                              Omega.sym.list.diff = Omega.hat.final.sym.list.diff,
                              res.trans.list = res.trans,
                              W.list=W.list, W.list.diff=W.list.diff)
  return(tensor.GGM.trans.res)
}


