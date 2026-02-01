#' Transfer learning of high-dimensional Gaussian graphical mixture models.
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M. and Wang J. (2023). Local transfer learning of Gaussian graphical mixture models.
#' @usage trans_GGMM(t.data, lambda.t, M, A.data, lambda.A.list, M.A.vec,
#'                   pseudo.cov="soft", cov.method="opt", cn.lam2=0.5, clambda.m=1,
#'                   theta.algm="cd", initial.selection="K-means", preselect.aux=0,
#'                   sel.type="L2", trace=FALSE )
#'
#' @description Transfer learning of high-dimensional Gaussian graphical mixture models.
#' @param t.data The target data, a n * p matrix, where n is the sample size and p is data dimension.
#' @param lambda.t A list, the sequences of the tuning parameters (lambda1, lambda2, and lambda3) used in the initialization of the target domain.
#' @param M Int, a selected upper bound of the true numbers of subgroups in the target domain.
#' @param A.data The auxiliary data in K auxiliary domains, a list with K elements, each of which is a nk * p matrix, where nk is the sample size of the k-th auxiliary domain.
#' @param lambda.A.list A list consisting of K lists, the k-th list is the sequences of the tuning parameters (lambda1, lambda2, and lambda3) used in the initialization of the k-th auxiliary domain.
#' @param M.A.vec A vector composed of K integers, the k-th element is a selected upper bound of the true numbers of subgroups in the k-th auxiliary domain.
#' @param pseudo.cov The method for calculating pseudo covariance matricex in auxiliary domains, which can be selected from "soft"(default, subgroups based on samples of soft clustering via posterior probability ) and "hard" (subgroups based on samples of hard clustering).
#' @param cov.method The method of aggregating K auxiliary covariance matrices, which can be selected as "size" (the sum weighted by the sample sizes), "weight" (the sum weighted by the differences) or "opt" (select the optimal one).
#' @param cn.lam2 A vector or a float value: the coefficients set in tuning parameters used to solve the target precision matrix, default is cn.lam2*sqrt( log(p) / n ).
#' @param clambda.m The coefficients set in tuning parameters used in transfer learning for mean eatimation, and the default setting is clambda.m * sqrt( log(p) / n ).
#' @param theta.algm The optimization algorithm used to solve the precision, which can be selected as "admm" (ADMM algorithm) or "cd" (coordinate descent).
#' @param initial.selection The different initial values from two clustering methods, which can be selected from c("K-means","dbscan").
#' @param preselect.aux Whether to pre-select informative auxiliary domains based on the distance between initially estimated auxiliary and target parameters. The default is 0, which means that pre-selection will not be performed. If "preselect.aux" is specified as a real number greater than zero, then the threshold value is forpreselect.aux*s*sqrt( log(p) / n ).
#' @param sel.type If pre-selection should be performed, "sel.type" is the type of distance. The default is L2 norm, and can be specified as "L1" to use L1 norm.
#' @param trace The logical variable, whether or not to output the number of identified subgroups during the search for parameters in the initialization.
#'
#' @return A result list including:
#' \describe{
#' \item{res.target}{A list including transfer learning results of the target domain.}
#'               \item{res.target$opt_Mu_hat}{The final estimation of means in all detected subgroups via transfer learning.}
#'               \item{res.target$opt_Theta_hat}{The final estimation of precision matrices in all detected subgroups via transfer learning.}
#' \item{res.target0}{A list including initial results of the target domain.}
#'               \item{res.target0$opt_Mu_hat}{The initial estimation of means in all detected subgroups.}
#'               \item{res.target0$opt_Theta_hat}{ The initial estimation of precision matrices in all detected subgroups.}
#' \item{t.res}{A list including results of the transfer precision matrix for each subgroup.}
#' }
#' @export
#'
#' @import HeteroGGM
#'
#'
#' @examples
#' "Will be supplemented in the next version."
#'
#'
trans_GGMM = function(t.data, lambda.t, M, A.data, lambda.A.list, M.A.vec,
                      pseudo.cov="soft", cov.method="opt", cn.lam2=0.5, clambda.m=1,
                      theta.algm="cd", initial.selection="K-means", preselect.aux=0,
                      sel.type="L2", trace=FALSE ){

  init_GGMM = Init_trans_GGMM(t.data, lambda.t, M, A.data, lambda.A.list, M.A.vec, initial.selection, trace )
  t.n.vec = init_GGMM$t.n.vec
  M0.hat = init_GGMM$M0.hat
  t.Theta_hat.array0 = init_GGMM$t.Theta_hat.array0
  t.mu_hat.mat0 = init_GGMM$t.mean
  res.target0 = init_GGMM$res.target
  if(pseudo.cov=="soft"){A.cov = init_GGMM$A.cov.soft; nA.vec=init_GGMM$nA.vec.soft}
  if(pseudo.cov=="hard"){A.cov = init_GGMM$A.cov.hard; nA.vec=init_GGMM$nA.vec.hard}
  A.mean = init_GGMM$A.mean

  t.res = list()
  t.Theta_hat.array = t.Theta_hat.array0
  t.mu_hat.mat = t.mu_hat.mat0
  res.target = res.target0
  for (m in 1:M0.hat) {
    ## tranfer precision
    t.res[[m]] = trans_precision(cov.method=cov.method, cn.lam2=cn.lam2, theta.algm=theta.algm,
                              input.A.cov=T, A.cov=A.cov, nA.vec=nA.vec,
                              preselect.aux=preselect.aux, sel.type=sel.type,
                              t.Theta.hat0=t.Theta_hat.array0[,,m], t.n=t.n.vec[m])
    t.Theta_hat.array[,,m] = t.res[[m]]$Theta.hat

    ## tranfer mean
    t.mu_hat.mat[m,] = trans_mean(t.mu_hat.mat0[m,], A.mean, t.n.vec[m], clambda.m)

  }
  res.target$opt_Theta_hat = t.Theta_hat.array
  res.target$opt_Mu_hat = t.mu_hat.mat

  res.final = list(res.target=res.target, res.target0=res.target0, t.res=t.res)
  return(res.final)

}

