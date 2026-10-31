#' Envelope dimension selection based on 1D-BIC
#'
#' This function selects envelope subspace dimension using 1D-BIC proposed by Zhang, X., & Mai, Q. (2018). The constrained optimization in the 1D algorithm is based on the line search algorithm for optimization on manifold. The algorithm is developed by Wen and Yin (2013) and the Matlab version is in the Matlab package \pkg{OptM}.
#'
#' @param M The \eqn{p}-by-\eqn{p} positive definite matrix \eqn{M} in the envelope objective function.
#' @param U The \eqn{p}-by-\eqn{p} positive semi-definite matrix \eqn{U} in the envelope objective function.
#' @param n The sample size.
#' @param C The constant defined in 1D-BIC criterion, the default value is 1.
#' @param maxdim The maximum dimension to consider, \code{maxdim} is smaller than \eqn{p}, the default value is 10.
#' @param ... Additional user-defined arguments for the line search algorithm:
#' \itemize{
#'  \item \code{maxiter}: The maximal number of iterations.
#'  \item \code{xtol}: The convergence tolerance for the relative changes of the consecutive iterates \eqn{w}, e.g., \eqn{||w^{(k)} - w^{(k-1)}||_F/\sqrt{p}}
#'  \item \code{gtol}: The convergence tolerance for the gradient of Lagrangian, e.g., \eqn{||G^{(k)} - w^{(k)} (G^{(t)})^T w^{(t)}||_F}
#'  \item \code{ftol}: The convergence tolerance for relative changes of the consecutive objective function values \eqn{F}, e.g., \eqn{|F^{(k)} - F^{(k-1)}|/(1+|F^{(k-1)}|)}. Usually, \code{max{xtol, gtol} > ftol}
#' }
#' The default values are: \code{maxiter=500; xtol=1e-08; gtol=1e-08; ftol=1e-12.}
#'
#' @return
#' \item{bicval}{The BIC values for different envelope dimensions.}
#' \item{u}{The dimension selected which corresponds to the smallest BIC values.}
#' \item{Gamma}{The estimation of envelope subspace basis.}
#'
#' @details The objective function \eqn{F(w)} and its gradient \eqn{G(w)} in line search algorithm are:
#' \deqn{F(w)=\log|w^T M_k w|+\log|w^T(M_k+U_k)^{-1}w|}
#' \deqn{G(w) = dF/dw = 2 (w^T M_k w)^{-1} M_k w + 2 (w^T (M_k + U_k)^{-1} w)^{-1}(M_k + U_k)^{-1} w}
#' See Cook, R. D., & Zhang, X. (2016) for more details of the 1D algorithm.
#'
#' The 1D-BIC criterion is defined as
#' \deqn{I(k) = \sum_{j=1}^k \phi_j(\hat{w}_j) + Ck\log(n)/n, \quad k = 0,1, \ldots, p,}
#' where \eqn{C > 0} is a constant, \eqn{\hat{w}} is the 1D solver, the function \eqn{\phi_j} is the individual objective function solved by 1D algorithm, \eqn{n} is the sample size. Then the selected dimension \eqn{u} is the one yielding the smallest 1D-BIC \eqn{I(k)}. See Zhang, X., & Mai, Q. (2018) for more details.
#'
#' As suggested by Zhang, X., & Mai, Q. (2018), the number \eqn{C} should be set to its default value \eqn{C = 1} when there is no additional model assumption or prior information. However, if additional model assumption or prior information are known, C should be set such that \eqn{Ck} best matches the degree-of-freedom or total number of free parameters of the model or estimation procedure. For example, in TRR model where the predictor design matrix is of dimension \eqn{p \times n}, \eqn{C} should be set as \eqn{p}. See Zhang, X., & Mai, Q. (2018) for more details.
#'
#' @references Zhang, X. and Mai, Q., 2018. Model-free envelope dimension selection. Electronic Journal of Statistics, 12(2), pp.2193-2216.
#'
#' Wen, Z. and Yin, W., 2013. A feasible method for optimization with orthogonality constraints. Mathematical Programming, 142(1-2), pp.397-434.
#'
#' @seealso \code{\link{OptM1D}, \link{MenvU_sim}}
#'
#' @examples
#' ##simulate two matrices M and U with an envelope structure
#' data <- MenvU_sim(p = 20, u = 5, wishart = TRUE, n = 200)
#' M <- data$M
#' U <- data$U
#' bic <- oneD_bic(M, U, n = 200)
#' ## visualization
#' plot(1:10, bic$bicval, type="o", xlab="Envelope Dimension", ylab="BIC values",
#' main="Envelope Dimension Selection")
#'
#' @export
oneD_bic <- function(M, U, n, C=1, maxdim=10, ...) {
  p <- dim(M)[2]
  Mnew <- M
  Unew <- U
  G <- matrix(0, p, maxdim)
  G0 <- diag(p)
  phi <- rep(0, p)
  for (k in 1:maxdim){
    if (k == p) break
    gk <- ballGBB1D(Mnew, Unew, ...)$X
    phi[k] <- n*(log(t(gk) %*% Mnew %*% gk)+ log(t(gk) %*% chol2inv(chol(Mnew+Unew)) %*% gk))+
      log(n)*C
    G[, k] <- G0 %*% gk
    G0 <- qr.Q(qr(G[, 1:k]), complete=TRUE)[, (k+1):p]
    Mnew <- t(G0) %*% M %*% G0
    Unew <- t(G0) %*% U %*% G0
  }
  bicval <- rep(0, maxdim)
  for (k in 1:maxdim) {
    bicval[k] <- sum(phi[1:k])
  }
  u <- which.min(bicval)
  Gamma <- G[,1:u]
  return(list(u = u, bicval = bicval, Gamma = Gamma))
}
