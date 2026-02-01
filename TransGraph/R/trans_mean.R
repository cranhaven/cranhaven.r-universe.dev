#' Transfer learning for mean estimation.
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M. and Wang J. (2023). Local transfer learning of Gaussian graphical mixture models.
#' @usage trans_mean(t.mean.m, A.mean, n, clambda=1)
#'
#' @description Transfer learning for mean estimation.
#' @param t.mean.m The estimated target p-dimensional mean vector, where p is mean dimension.
#' @param A.mean A K*p matrix with the k-th row being the estimated p-dimensional mean vector of the k-th auxiliary domain.
#' @param n The target sample size.
#' @param clambda The coefficients set in tuning parameters used in transfer learning for mean eatimation, and the default setting is clambda.m * sqrt( log(p) / n ).
#'
#'
#' @return t.mean.m.hat: The transfer learning estimation of the target p-dimensional mean vector.
#'
#'
#'
#' @export
#'
#'
#'

trans_mean = function(t.mean.m, A.mean, n, clambda=1){
  p = dim(A.mean)[2]
  lam.m = clambda * sqrt( log(p) / n )
  A.mean.m = A.mean[which.min(apply(t(A.mean) - t.mean.m, 2, function(x) sum(x^2))),]
  t.mean.m.hat = A.mean.m + S_soft.vec(t.mean.m - A.mean.m, lam.m)
  return(t.mean.m.hat)
}

