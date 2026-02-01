#' Sparse precision matrix estimation with tuning parameters.
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M., Zhen Y., and Wang J. (2022). Transfer learning for tensor graphical models.
#'             Liu, W. and Luo X. (2015). Fast and adaptive sparse precision matrix estimation in high dimensions, Journal of Multivariate Analysis.
#' @usage Theta.tuning(lambda2, S.hat.A, delta.hat, Omega.hat0, n.A,
#'                     theta.algm="cd", adjust.BIC=FALSE)
#'
#' @description The fast sparse precision matrix estimation in step 2(b).
#' @param lambda2 A vector, a sequence of tuning parameters.
#' @param S.hat.A The sample covariance matrix.
#' @param delta.hat The divergence matrix estimated in step 2(a). If the precision matrix is estimated in the common case (Liu and Luo, 2015, JMVA), it can be set to zero matrix.
#' @param Omega.hat0 The initial values of the precision matrix.
#' @param n.A The sample size.
#' @param theta.algm The optimization algorithm used to solve \eqn{\widehat{\Omega}}{\widehat{\Omega}} in step 2(b), which can be selected as "admm" (ADMM algorithm) or "cd" (coordinate descent).
#' @param adjust.BIC Whether to use the adjusted BIC to select lambda2, the default setting is F.
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
#' lambda2 = seq(0.1,0.5,length.out =10)
#' res = Theta.tuning(lambda2, S.hat.A, delta.hat, n.A=n)
#' omega.hat = res$Theta.hat.m
#'
#'
#'

Theta.tuning = function(lambda2, S.hat.A, delta.hat, Omega.hat0=NULL, n.A,
                        theta.algm="cd", adjust.BIC=FALSE){

  L                  = length(lambda2)
  aBIC               = rep(0,L)
  Theta.list         = list()
  BIC.summary        = as.data.frame(matrix(0,ncol = 5, nrow = L))
  names(BIC.summary) = c("lambda", "BIC", "fitness", "BIC.penalty", "degree")

  for (l in 1:L) {
    lam2 = lambda2[l]
    Theta.hat.m = Theta.est(S.hat.A, delta.hat, lam2=lam2,
                            Omega.hat0=Omega.hat0, method = theta.algm)
    BIC.list = BIC_value(S.hat.A, delta.hat, Theta.hat.m, n.A, adjust.BIC)
    BIC.summary[l,] = c(lam2, unlist(BIC.list))
    Theta.list[[l]] = Theta.hat.m
  }
  Theta.hat.m.opt = Theta.list[[which.min(BIC.summary$BIC)]]
  Theta.tuning.res = list(Theta.hat.m=Theta.hat.m.opt, BIC.summary=BIC.summary,
                          Theta.hat.list.m=Theta.list)
  return(Theta.tuning.res)
}
