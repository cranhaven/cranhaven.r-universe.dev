#' Estimate the covariance of estimated parameters using the explicit formula
#'
#' \code{covjmcm} is a combination of \code{covjmcm_mcd}, \code{covjmcm_acd}, and \code{covjmcm_hpc}.
#' It identifies the corresponding type of the model, i.e. MCD, ACD, or HPC, and calculates the estimation
#' of the covariance of estimated parameters using explicit formula,
#' which is the inverse of the estimated Fisher's information matrix.
#'
#' @param object a fitted joint mean-covariance model of class "jmcmMod", returned by the function \code{jmcm}.
#' @return an estimated covariance matrix of the estimated parameters.
#' @references [1] Pourahmadi, M., "Maximum likelihood estimation of generalised linear models for multivariate normal covariance matrix," Biometrika
#' 87(2), 425–435 (2000).
#' @references [2] M. Maadooliat, M. Pourahmadi and J. Z. Huang, "Robust estimation of the correlation
#' matrix of longitudinal data", Statistics and Computing 23, 17-28, (2013).
#' @references [3]  W. Zhang, C. Leng, and C. Y. Tang(2015), "A joint modelling approach for longitudinal studies,"
#' Journal of the Royal Statistical Society. Series B. 77, 219-238.
#' @examples
#' ## balanced data
#' cattleA <- cattle[cattle$group=='A', ]
#' fit.mcd <- jmcm(weight|id|I(ceiling(day/14+1))~1|1,
#'                data = cattleA, cov.method = "mcd",
#'                triple = c(8,3,4))
#' cov.mcd <- covjmcm(fit.mcd)  ##same as covjmcm_mcd(fit.mcd)
#' ## unbalanced data
#' ## This may take about 1.25 min.
#' \donttest{
#' fit.hpc <- jmcm(I(sqrt(cd4)) | id | time ~ 1 | 1,
#'                 data = aids, triple = c(8,1,1),
#'                 cov.method = "hpc")
#' cov.hpc <- covjmcm(fit.hpc)  ##same as covjmcm_hpc(fit.hpc)}
#' @seealso \code{\link{covjmcm_mcd}}, \code{\link{covjmcm_acd}}, and \code{\link{covjmcm_hpc}}
#' @export
covjmcm <- function(object){
  ##object is a fitted jmcm model
  if (missing(object)) stop("missing object.")
  if (class(object)[1]!="jmcmMod") stop("Object must be of class 'jmcmMod'.")

  method <- object@call$cov.method

  if(method=="mcd") cov <- covjmcm_mcd(object)
  else if(method=="acd") cov <- covjmcm_acd(object)
  else if(method=="hpc") cov <- covjmcm_hpc(object)
  else stop("Method must be one of 'mcd','acd, or 'hpc'.")

  cov
}
