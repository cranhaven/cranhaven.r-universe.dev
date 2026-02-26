#' @title Ancillary arguments for controlling the outputs of coxph_mpl_dc
#'
#' @description This is used to set various numeric parameters controlling a Cox model fit using coxph_mpl_dc.
#' Typically it would only be used in a call to coxph_mpl_dc.
#' @usage coxph_mpl_dc.control(ordSp,
#'                        binCount, tie,
#'                        tau, copula,
#'                        pent, smpart, penc, smparc,
#'                        maxit2, maxit,
#'                        mid, asy, ac, cv,
#'                        ac.theta, ac.gamma, ac.Utheta, ac.Ugamma,
#'                        min.theta, min.gamma,
#'                        min.ht, min.hc, min.St, min.Sc, min.C, min.dC,
#'                        eps, tol.thga, tol.bph, cat.smpar, tol.smpar
#'                        )
#'
#' @param ordSp the order of spline for the basis function for baseline hazard for both T and C,
#' can be 'piecewise constant' if \code{ordSp=1}, cubic 'm-spline' if \code{ordSp=4}, etc. Default is \code{ordSp=1}.
#' @param binCount the number of subjects in each discretized bin, can be selected either by trial and error or AIC method
#' Default is \code{binCount=1}.
#' @param tie tie='No' if tied observations are not existed, otherwise tied observations existed. Default is \code{tie='No'}.
#' @param tau the kendall’s correlation coefficient between T and C. Default is \code{tau=0}.
#' @param copula Archimedean copula type, i.e. 'independent', 'clayton', 'gumbel' and 'frank'. Default is \code{copula='independent'}.
#' @param pent penalty function type for T, i.e. mat1 (first order difference) or mat2 (second order difference) for piecewise constant basis, penalty_mspl for m-spline basis
#' Default is \code{pent='mat1'}.
#' @param smpart value of smoothing parameter for T, can be selected by either trial and error or cross validation method.
#' Note that smpart can be also estimated by restricted maximum likelihood (i.e. \code{smpart='REML'}). Default is \code{smpart=0}.
#' @param penc penalty function type for C, i.e. mat1 (first order difference) or mat2 (second order difference) for piecewise constant basis, penalty_mspl for m-spline basis
#' Default is \code{pent='mat1'}.
#' @param smparc value of smoothing parameter for C, can be selected by either trial and error or cross validation method.
#' Note that smparc can be also estimated by restricted maximum likelihood (i.e. \code{smparc='REML'}). Default is \code{smparc=0}.
#' @param maxit2 maximum number of iterations for smpart and smparc. Defualt is \code{maxit2=50}.
#' @param maxit maximum number of iteration for updating beta, phi, theta and gamma given fixed smpart and smparc.
#' Default is \code{maxit=5000}.
#' @param mid the middle matrix selection for the sandwich formula that used to computed the asymptotic covariance matrix,
#' i.e. \code{mid=1} (negative of the hessian matrix with zeros smoothing parameters, i.e. \code{smpart=smparc=0},
#' or negative of the matrix with second derivatives of the MPL estimates with respect to the log-likelihood),
#' 2 (the matrix created by the vector of first derivative of the penalized log-likelihood with respect to the MPL estimates times its transpose) and
#' otherwise (negative of the hessian matrix or negative of  the matrix with second derivatives of the MPL estimates with respect to the penalized log-likelihood). Default is \code{mid=1}.
#' @param asy \code{asy=1} if asymptotic standard deviation of the MPL estimates are computed and 0 if not computed. Default is \code{asy=1}.
#' @param ac  \code{ac=1} if aic value is calculated 0 if not. Default is \code{ac=0}.
#' @param cv  \code{cv=0} if cv value is calculated 0 if not. Default is \code{cv=0}.
#' @param ac.theta the minimum value of theta for active contraints. Default is \code{ac.theta=1e-5}.
#' @param ac.gamma the minimum value of gamma for active contraints. Default is \code{ac.gamma=1e-5}.
#' @param ac.Utheta the minimum value of Utheta (the first derivative of the penalized log-likelihood with respect to theta) for active contraints. Default is \code{ac.Utheta=1e-2}.
#' @param ac.Ugamma  the minimum value of Ugamma (the first derivative of the penalized log-likelihood with respect to gamma) for active contraints. Default is \code{ac.Ugamma=1e-2}.
#' @param min.theta a value indicating the minimal baseline hazard parameter value theta updated at each iteration.
#' Baseline hazard parameter theta estimates at each iteration lower than min.theta will be considered as min.theta. Default is \code{min.theta=1e-7}.
#' @param min.gamma  a value indicating the minimal baseline hazard parameter value gamma updated at each iteration.
#' Baseline hazard parameter gamma estimates at each iteration lower than min.gamma will be considered as min.gamma. Default is \code{min.gamma=1e-7}.
#' @param min.ht a value indicating the minimal baseline hazard of T updated at each iteration. Baseline hazard estimates of T at each iteration lower than min.ht will be considered as min.ht.
#' Default is \code{min.ht=1e-7}.
#' @param min.hc a value indicating the minimal baseline hazard of C updated at each iteration. Baseline hazard estimates of C at each iteration lower than min.hc will be considered as min.hc.
#' Default is \code{min.hc=1e-7}.
#' @param min.St a value indicating the minimal baseline survival of T updated at each iteration. Baseline survival estimates of T at each iteration lower than min.St will be considered as min.St.
#' Default is \code{min.St=1e-7}.
#' @param min.Sc a value indicating the minimal baseline survival of C updated at each iteration. Baseline survival estimates of C at each iteration lower than min.Sc will be considered as min.Sc.
#' Default is \code{min.Sc=1e-7}.
#' @param min.C a value indicating the minimal copula \eqn{K(u,v)} at each iteration, lower than min.C will be considered as min.C.
#' Default is \code{min.C=1e-7}.
#' @param min.dC a value indicating the minimal first i.e. \eqn{dK(u,v)/du} and \eqn{dK(u,v)/dv} and second i.e. \eqn{d^2K(u,v)/dudv} derivatives of copula \eqn{K(u,v)} at each iteration,
#' lower than min.dC will be considered as min.dC. Default is \code{min.dC=1e-7}.
#' @param eps a small positive value added to the diagonal of a square matrix. Default value is \code{eps=1e-5}.
#' @param tol.thga the convergence tolerence value for both theta and gamma.
#' Convergence is achieved when the maximum absolute difference between the parameter estimates at iteration k and iteration k-1 is smaller than tol.thga.
#' Default is \code{tol.thga=1e-5}.
#' @param tol.bph the convergence tolerence value for both beta and phi.
#' Convergence is achieved when the maximum absolute difference between the parameter estimates at iteration k and iteration k-1 is smaller than tol.bph.
#' Default is \code{tol.bph=1e-5}.
#' @param tol.smpar the convergence tolerence value for both smpart and smparc.
#' Convergence is achieved when the maximum absolute difference between the parameter estimates at iteration k and iteration k-1 is smaller than tol.smpar.
#' Default is \code{tol.smpar=1e-2}.
#' @param cat.smpar cat.smpar='Yes' to display the smoothing parameters estimation process, otherwise not to display.
#' Default is \code{cat.smpar='Yes'}.
#'
#' @return A list containing the values of each of the above arguments for most of the inputs of Coxph_mpl_dc.
#'
#' @author Jing Xu, Jun Ma, Thomas Fung
#'
#' @references Ma, J. and Heritier, S. and Lo, S. (2014). \emph{"On the Maximum Penalised Likelihood Approach forProportional Hazard Models with Right Censored Survival Data"}.
#' Computational Statistics and Data Analysis 74, 142-156.
#'
#' Xu J, Ma J, Connors MH, Brodaty H. (2018). \emph{"Proportional hazard model estimation under dependent censoring using copulas and penalized likelihood"}.
#' Statistics in Medicine 37, 2238–2251.
#'
#' @seealso \code{\link{plot.coxph_mpl_dc}}, \code{\link{coxph_mpl_dc}}, \code{\link{coef.coxph_mpl_dc}}
#'
#' @examples
#' control <- coxph_mpl_dc.control(ordSp=4,
#'                              binCount=40,
#'                              tau=0.8, copula='frank',
#'                              pent='penalty_mspl', smpart='REML', penc='penalty_mspl', smparc='REML',
#'                              cat.smpar='No'
#'                              )
#'
#' @export


coxph_mpl_dc.control <- function(ordSp,
                              binCount, tie,
                              tau, copula,
                              pent, smpart, penc, smparc,
                              maxit2, maxit,
                              mid, asy, ac, cv,
                              ac.theta, ac.gamma, ac.Utheta, ac.Ugamma,
                              min.theta, min.gamma, min.ht, min.hc, min.St, min.Sc, min.C, min.dC, eps,
                              tol.thga, tol.bph, cat.smpar, tol.smpar
                              )
{
  if(missing(ordSp)){ordSp <- 1}
  if(missing(binCount)){binCount <- 1}
  if(missing(tie)){tie <- 'No'}
  if(missing(tau)){tau <- 0}
  if(missing(copula)){copula <- 'independent'}
  if(missing(pent)){pent <- 'mat1'}
  if(missing(penc)){penc <- 'mat1'}
  if(missing(smpart)){smpart <- 0}
  if(missing(smparc)){smparc <- 0}
  if(missing(maxit2)){maxit2 <- 50}
  if(missing(maxit)){maxit <- 5000}
  #if(missing(inc)){inc=1}
  if(missing(mid)){mid <- 1}
  if(missing(asy)){asy <- 1}
  if(missing(ac)){ac <- 0}
  if(missing(cv)){cv <- 0}

  if(missing(ac.theta)){ac.theta <- 1e-5}
  if(missing(ac.gamma)){ac.gamma <- 1e-5}
  if(missing(ac.Utheta)){ac.Utheta <- -1e-2}
  if(missing(ac.Ugamma)){ac.Ugamma <- -1e-2}
  if(missing(min.theta)){min.theta <- 1e-7}
  if(missing(min.gamma)){min.gamma <- 1e-7}
  if(missing(min.ht)){min.ht <- 1e-7}
  if(missing(min.hc)){min.hc <- 1e-7}
  if(missing(min.St)){min.St <- 1e-7}
  if(missing(min.Sc)){min.Sc <- 1e-7}
  if(missing(min.C)){min.C <- 1e-7}
  if(missing(min.dC)){min.dC <- 1e-7}
  if(missing(eps)){eps <- 1e-5}
  if(missing(tol.thga)){tol.thga <- 1e-5}
  if(missing(tol.bph)){tol.bph <- 1e-5}
  if(missing(cat.smpar)){cat.smpar <- 'Yes'}
  if(missing(tol.smpar)){tol.smpar <- 1e-2}

  out=list(
    ordSp=ordSp,
    binCount=binCount,
    tie=tie,
    tau=tau,
    copula=copula,
    pent=pent,
    smpart=smpart,
    penc=penc,
    smparc=smparc,
    maxit2=maxit2,
    maxit=maxit,
    mid=mid,
    asy=asy,
    ac=ac,
    cv=cv,
    ac.theta=ac.theta,
    ac.gamma=ac.gamma,
    ac.Utheta=ac.Utheta,
    ac.Ugamma=ac.Ugamma,
    min.theta=min.theta,
    min.gamma=min.gamma,
    min.ht=min.ht,
    min.hc=min.hc,
    min.St=min.St,
    min.Sc=min.Sc,
    min.C=min.C,
    min.dC=min.dC,
    eps=eps,
    tol.thga=tol.thga,
    tol.bph=tol.bph,
    cat.smpar=cat.smpar,
    tol.smpar=tol.smpar
  )

  class(out) = "coxph_mpl_dc.control"
  out
}





































