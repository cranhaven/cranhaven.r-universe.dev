#' The Objective function and its gradient
#'
#' Calculates the objective function and its gradient for estimating the \eqn{M}-envelope of span(\eqn{U}), where \eqn{M} is positive definite and \eqn{U} is positive semi-definite.
#'
#' @param M The \eqn{p}-by-\eqn{p} positive definite matrix \eqn{M} in the envelope objective function.
#' @param U The \eqn{p}-by-\eqn{p} positive semi-definite matrix \eqn{U} in the envelope objective function.
#' @param Gamma \eqn{\Gamma} matrix in the envelope objective function. A \eqn{p}-by-\eqn{u} matrix.
#'
#' @details
#' The generic objective function \eqn{F(\Gamma)} and its gradient \eqn{G(\Gamma)} are listed below for estimating \eqn{M}-envelope of span(\eqn{U}). For the detailed description, see Cook, R. D., & Zhang, X. (2016).
#'
#' \deqn{F(\Gamma)=\log|\Gamma^T M \Gamma|+\log| \Gamma^T(M+U)^{-1}\Gamma|}
#' \deqn{G(\Gamma) = dF/d \Gamma = 2 M \Gamma (\Gamma^T M \Gamma)^{-1} + 2 (M + U)^{-1} \Gamma (\Gamma^T (M + U)^{-1} \Gamma)^{-1}}
#'
#' @return
#' \item{F}{The value of the objective function at \code{Gamma}.}
#' \item{G}{The value of the gradient function at \code{Gamma}.}
#'
#' @references Cook, R.D. and Zhang, X., 2016. Algorithms for envelope estimation. Journal of Computational and Graphical Statistics, 25(1), pp.284-300.
#'
#' @export
FGfun <- function (Gamma, M, U)
{
  f <- log(det(t(Gamma) %*% M %*% Gamma)) + log(det(t(Gamma) %*% chol2inv(chol(M+U)) %*% Gamma))
  a <- M %*% Gamma %*% chol2inv(chol(t(Gamma) %*% M %*% Gamma))
  b <- chol2inv(chol(M+U)) %*% Gamma %*% chol2inv(chol((t(Gamma) %*% chol2inv(chol(M+U)) %*% Gamma)))
  df <- 2*(a + b)
  return(list(F = f, G = df))
}
