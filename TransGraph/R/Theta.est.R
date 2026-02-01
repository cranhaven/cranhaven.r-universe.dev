#' Sparse precision matrix estimation.
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M., Zhen Y., and Wang J. (2022). Transfer learning for tensor graphical models.
#'             Liu, W. and Luo X. (2015). Fast and adaptive sparse precision matrix estimation in high dimensions, Journal of Multivariate Analysis.
#' @usage Theta.est(S.hat.A, delta.hat, lam2=0.1, Omega.hat0=NULL,
#'                  n=100, max_iter=10, eps=1e-3, method = "cd")
#'
#' @description The fast sparse precision matrix estimation in step 2(b).
#' @param S.hat.A The sample covariance matrix.
#' @param delta.hat The divergence matrix estimated in step 2(a). If the precision matrix is estimated in the common case (Liu and Luo, 2015, JMVA), it can be set to zero matrix.
#' @param lam2 A float value, a tuning parameter.
#' @param Omega.hat0 The initial values of the precision matrix, which can be unspecified.
#' @param n The sample size.
#' @param max_iter Int, maximum number of cycles of the algorithm.
#' @param eps A float value, algorithm termination threshold.
#' @param method The optimization algorithm, which can be selected as "admm" (ADMM algorithm) or "cd" (coordinate descent).
#'
#' @return A result list including:
#' \describe{
#' \item{Theta.hat.m}{The optimal precision matrix.}
#' \item{BIC.summary}{The summary of BICs.}
#' \item{Theta.hat.list.m}{The precision matrices corresponding to a sequence of tuning parameters.}
#' }
#' @export
#'
#' @examples
#' p = 20
#' n = 200
#' omega = diag(rep(1,p))
#' for (i in 1:p) {
#'   for (j in 1:p) {
#'     omega[i,j] = 0.3^(abs(i-j))*(abs(i-j) < 2)
#'   }
#' }
#' Sigma = solve(omega)
#' X = MASS::mvrnorm(n, rep(0,p), Sigma)
#' S.hat.A = cov(X)
#' delta.hat = diag(rep(1,p)) - diag(rep(1,p))
#' omega.hat = Theta.est(S.hat.A, delta.hat, lam2=0.2)
#'
#'
#'
Theta.est = function(S.hat.A, delta.hat, lam2=0.1, Omega.hat0=NULL,
                     n=100, max_iter=10, eps=1e-3, method = "cd"){
  # Theta.est: the function estimating transfer learning-based estimator of
  #            precision matrix of the mode corresponding to S.hat.A, via
  #            two algorithms:
  #            ADMM (method = "admm") & Coordinate descent (method = "cd").
  pm = dim(S.hat.A)[1]
  deltaI = delta.hat + diag(pm)

  if(is.null(Omega.hat0)){
    if(abs(det(S.hat.A)) < 1e-3){
      Omega.hat0 = solve(S.hat.A + diag(pm)/n) %*% deltaI
    } else {
      Omega.hat0 = solve(S.hat.A) %*% deltaI
    }
  }

  if(method == "cd"){
    Theta.hat = Thetaest.cd(S.hat.A, deltaI, lam2, Omega.hat0, max_iter, eps)
  }
  if(method == "admm"){
    Theta.hat = Thetaest.admm(S.hat.A, deltaI, lam2, Omega.hat0, max_iter, eps)
  }

  return(Theta.hat)
}
