#' Transfer learning for vector-valued precision matrix (graphical model).
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M., Zhen Y., and Wang J. (2022). Transfer learning for tensor graphical models.
#'             Ren, M., He X., and Wang J. (2023). Structural transfer learning of non-Gaussian DAG.
#' @usage trans_precision(t.data=NULL, A.data=NULL, precision.method="CLIME",
#'                        cov.method="opt", cn.lam2=seq(1,2.5,length.out=10),
#'                        theta.algm="cd", adjust.BIC=FALSE, symmetry=TRUE,
#'                        preselect.aux=0, sel.type="L2", input.A.cov=FALSE,
#'                        A.cov=NULL, nA.vec=NULL, t.Theta.hat0=NULL,
#'                        t.n=NULL, correlation=FALSE)
#'
#' @description The transfer learning for vector-valued precision matrix via D-trace loss method.
#' @param t.data The target data, a n * p matrix, where n is the sample size and p is data dimension.
#' @param A.data The auxiliary data in K auxiliary domains, a list with K elements, each of which is a nk * p matrix, where nk is the sample size of the k-th auxiliary domain.
#' @param precision.method The initial method of estimating the target precision matrix, which can be selected as "CLIME" or "glasso".
#' @param cov.method The method of aggregating K auxiliary covariance matrices, which can be selected as "size" (the sum weighted by the sample sizes), "weight" (the sum weighted by the differences) or "opt" (select the optimal one).
#' @param cn.lam2 A vector or a float value: the coefficients set in tuning parameters used to solve the target precision matrix, default is cn.lam2*sqrt( log(p) / n ).
#' @param theta.algm The optimization algorithm used to solve the precision, which can be selected as "admm" (ADMM algorithm) or "cd" (coordinate descent).
#' @param adjust.BIC Whether to use the adjusted BIC to select lambda2, the default setting is FALSE.
#' @param symmetry Whether to symmetrize the final estimated precision matrices, and the default is True.
#' @param preselect.aux Whether to pre-select informative auxiliary domains based on the distance between initially estimated auxiliary and target parameters. The default is 0, which means that pre-selection will not be performed. If "preselect.aux" is specified as a real number greater than zero, then the threshold value is preselect.aux*s*sqrt( log(p) / n ).
#' @param sel.type If pre-selection should be performed, "sel.type" is the type of distance. The default is L2 norm, and can be specified as "L1" to use L1 norm.
#' @param input.A.cov Whether to input the covariance matrices of the auxiliary domains. The default setting is FALSE, which means that the raw data of the auxiliary domain is input, and the covariance will be calculated within this function. If input.A.cov=T, then the calculated covariance matrices must be input through parameter "A.cov", and parameter "A.data" can be defaulted at this time. This setting is suitable for situations where raw data cannot be obtained but the covariance matrix can be obtained.
#' @param A.cov If input.A.cov=T, the "A.cov" must be auxiliary covariance matrices in K auxiliary domains, a list with K elements, each of which is a p * p matrix.
#' @param nA.vec If input.A.cov=T, the "nA.vec" must be a vector consisting of sample sizes of K auxiliary domains.
#' @param t.Theta.hat0 Whether to input the estimated target precision matrix based on the target domain only, and the default setting is NULL. If "t.Theta.hat0" is specified as an estimated precision matrix, it will not be recalculated in the initialization phase. This parameter mainly plays a role in transfer learning of GGMMs.
#' @param t.n Whether to input the target sample size, and the default setting is NULL. This parameter mainly plays a role in transfer learning of GGMMs.
#' @param correlation Whether to use correlation matrix for initial parameters in both target and auxiliary domains. The default setting is F.
#'
#'
#' @return A result list including:
#' \describe{
#' \item{Theta.hat}{The target precision matrix via transfer learning.}
#' \item{Theta.hat0}{The initial target precision matrix.}
#' \item{k.check}{The number of the optimal auxiliary domain.}
#' \item{N}{The minimum sample size for auxiliary domain.}
#' }
#' @export
#'
#' @import glasso huge clime
#' @importFrom stats cov cor cov2cor
#' @importFrom utils capture.output
#'
#'
#'

trans_precision = function(t.data=NULL, A.data=NULL, precision.method="CLIME",
                           cov.method="opt", cn.lam2=seq(1,2.5,length.out=10),
                           theta.algm="cd", adjust.BIC=FALSE, symmetry=TRUE,
                           preselect.aux=0, sel.type="L2", input.A.cov=FALSE,
                           A.cov=NULL, nA.vec=NULL, t.Theta.hat0=NULL,
                           t.n=NULL, correlation=FALSE){
  ##### Initialization ####
    int.value = Initial_GGM(t.data, A.data, precision.method=precision.method,
                            input.A.cov=input.A.cov, A.cov=A.cov, nA.vec=nA.vec,
                            t.Theta.hat0=t.Theta.hat0, t.n=t.n,
                            correlation=(correlation & is.null(t.Theta.hat0)),
                            preselect.aux=preselect.aux, sel.type=sel.type)
    Omega.hat0 = int.value$Theta.hat0
    noninfor   = int.value$noninfor


    if(noninfor){
      res = list(Theta.hat=Omega.hat0, Theta.hat0=Omega.hat0, k.check = NULL,
                 N=NULL, A.data.select=NULL, A.cov.select=NULL,
                 delta.vec=int.value$delta.vec, lam.preaux=int.value$lam.preaux,
                 infor.num=int.value$infor.num)
    } else{
      if(cov.method=="size"){
        S.hat.A = int.value$S.hat.A.size
      }
      if(cov.method=="weight"){
        S.hat.A = int.value$S.hat.A.weight
      }
      if(cov.method=="opt"){
        S.hat.A = int.value$S.hat.A.opt
      }
      k.check = int.value$k.check
      n.da = int.value$n
      p = int.value$p
      N = int.value$N
      A.data.select = int.value$A.data.select
      A.cov.select = int.value$A.cov.select

      #### Transfer learning algorithm ####
      lam1 = 2*max(apply(abs(Omega.hat0), 2, sum))*sqrt(log(p) / n.da)
      delta.hat = delta.est(S.hat.A, Omega.hat0, lam1=lam1)

      lambda2 = cn.lam2*sqrt( log(p) / n.da )
      Theta.tuning.res = Theta.tuning(lambda2, S.hat.A, delta.hat, Omega.hat0,
                                      n.A=n.da, theta.algm=theta.algm, adjust.BIC=adjust.BIC)
      Theta.hat = Theta.tuning.res$Theta.hat.m
      if(symmetry){
        Theta.hat = ( Theta.hat + t(Theta.hat) ) / 2
        Omega.hat0 = ( Omega.hat0 + t(Omega.hat0) ) / 2
      }

      res = list(Theta.hat=Theta.hat, Theta.hat0=Omega.hat0, k.check = k.check,
                 N=N, A.data.select=A.data.select, A.cov.select=A.cov.select,
                 delta.vec=int.value$delta.vec, lam.preaux=int.value$lam.preaux,
                 infor.num=int.value$infor.num)
    }

  return(res)

}

