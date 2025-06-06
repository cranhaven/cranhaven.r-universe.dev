#' 
#' @name modreg.control
#' 
#' @title 
#' Setting fitting values for \code{\link[dirttee]{modreg}}.
#' 
#' @description 
#' This is an internal function of package \code{dirttee} which allows control of the numerical options 
#' for fitting mode regression. Typically, users will want to modify the defaults if model fitting 
#' is slow or fails to converge.
#' 
#' @param StartInterval Starting values are based on an estimate for the mean and an interval around it. The interval is \eqn{+-\code{StartInterval} * \sigma}. Default is  \eqn{\sqrt{3}}.
#' @param nStart Number of starting values, considered in the first iteration. Default is 11.
#' @param nInterim Probably has little impact on speed and result. After \code{itInterim} weighted least squares iterations, the number of estimates is reduced from \code{nStart} to \code{nInterim} estimates. Default is 5.
#' @param maxit Maximum number of iterations for the weighted least squares algorithm. Default is 100.
#' @param itInterim Probably has little impact on speed and result. After \code{itInterim} weighted least squares iterations, the number of estimates is reduced from \code{nStart} to \code{nInterim} estimates. Default is 10.
#' @param tol Convergence criterion for the weighted least squares algorithm. Default is 10^-4.
#' @param tol_bw_plugin Convergence criterion for bandwidth selection in the \code{"Plugin"} method. Default is 10^-3.
#' @param maxit_bw_plugin Maximum number of iterations for bandwidth selection in the \code{"Plugin"} method. Default is 10.
#' @param maxit_penalty_plugin Maximum number of iterations for penalty selection in the \code{"Plugin"} method. Default is 10.
#' @param tol_penalty_plugin Convergence criterion for penalty selection in the \code{"Plugin"} method. Default is 10^-3.
#' @param tol_regopt Weighted least squares are recalculated for hyperparameter optimization. This is the convergence criterion within this optimization. Default is \code{tol} * 100.
#' @param tol_opt Convergence criterion for the first hyperparameter optimizion. Can be increased to reduce compuation time. Default is 10^-3.
#' @param maxit_opt Maximum number of iterations for the first hyperparameter optimizion. Can be lowered to reduce compuation time. Default is 200.
#' @param tol_opt2 Convergence criterion for the second hyperparameter optimizion. Default is 10^-3.
#' @param maxit_opt2 Maximum number of iterations for the second hyperparameter optimizion. Default is 200.
#' 
#' @returns
#' A list with the arguments as components
#' 
#' @details 
#' The algorithm is described in Seipp et al. (2022). To increase the speed of the algorithm, adapting \code{tol} and \code{maxit_opt}/\code{maxit_opt2} and other penalty / hyperparameter optimization parameters are a good starting point. 
#' 
#' @references 
#' Seipp, A., Uslar, V., Weyhe, D., Timmer, A., & Otto-Sobotka, F. (2022). Flexible Semiparametric Mode Regression for Time-to-Event Data. Manuscript submitted for publication. \cr
#' Yao, W., & Li, L. (2014). A new regression model: modal linear regression. Scandinavian Journal of Statistics, 41(3), 656-671.
#' 
#' @export


modreg.control <- function(
  StartInterval = sqrt(3),
  nStart = 11,
  nInterim = NULL,
  maxit = 100,
  itInterim = 10,
  tol = 10^-4,
  tol_bw_plugin = 10^-3,
  maxit_bw_plugin = 10,
  maxit_penalty_plugin = 10,
  tol_penalty_plugin = 10^-3,
  tol_regopt = tol * 100,
  tol_opt = 10^-3,
  maxit_opt = 200,
  tol_opt2 = 10^-3,
  maxit_opt2 = 200
){
  obj <- list(  StartInterval = StartInterval,
                nStart = nStart,
                nInterim = nInterim,
                maxit = maxit,
                itInterim = itInterim,
                tol = tol,
                tol_bw_plugin = tol_bw_plugin,
                maxit_bw_plugin = maxit_bw_plugin,
                maxit_penalty_plugin = maxit_penalty_plugin,
                tol_penalty_plugin = tol_penalty_plugin,
                tol_regopt = tol_regopt,
                tol_opt = tol_opt,
                maxit_opt = maxit_opt,
                tol_opt2 = tol_opt2,
                maxit_opt2 = maxit_opt2)
  class(obj) <- "modreg_control"
  return(obj)
}
