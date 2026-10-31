##################################################
#  1D optimization solve for envelope basis      #
##################################################
#' Estimate the envelope subspace (\pkg{OptM} 1D)
#'
#' The 1D algorithm to estimate the envelope subspace based on the line search algorithm for optimization on manifold. The line search algorithm is developed by Wen and Yin (2013) and the Matlab version is implemented in the Matlab package \pkg{OptM}.
#'
#' @param M The \eqn{p}-by-\eqn{p} positive definite matrix \eqn{M} in the envelope objective function.
#' @param U The \eqn{p}-by-\eqn{p} positive semi-definite matrix \eqn{U} in the envelope objective function.
#' @param u An integer between 0 and \eqn{n} representing the envelope dimension.
#' @param ... Additional user-defined arguments for the line search algorithm:
#' \itemize{
#'  \item \code{maxiter}: The maximal number of iterations.
#'  \item \code{xtol}: The convergence tolerance for the relative changes of the consecutive iterates \eqn{w}, e.g., \eqn{||w^{(k)} - w^{(k-1)}||_F/\sqrt{p}}
#'  \item \code{gtol}: The convergence tolerance for the gradient of Lagrangian, e.g., \eqn{||G^{(k)} - w^{(k)} (G^{(t)})^T w^{(t)}||_F}
#'  \item \code{ftol}: The convergence tolerance for relative changes of the consecutive objective function values \eqn{F}, e.g., \eqn{|F^{(k)} - F^{(k-1)}|/(1+|F^{(k-1)}|)}. Usually, \code{max{xtol, gtol} > ftol}
#' }
#' The default values are: \code{maxiter=500; xtol=1e-08; gtol=1e-08; ftol=1e-12.}
#'
#' @details The objective function \eqn{F(w)} and its gradient \eqn{G(w)} in line search algorithm are:
#' \deqn{F(w)=\log|w^T M_k w|+\log|w^T(M_k+U_k)^{-1}w|}
#' \deqn{G(w) = dF/dw = 2 (w^T M_k w)^{-1} M_k w + 2 (w^T (M_k + U_k)^{-1} w)^{-1}(M_k + U_k)^{-1} w}
#' See Cook, R. D., & Zhang, X. (2016) for more details of the 1D algorithm.
#'
#' @return Return the estimated orthogonal basis of the envelope subspace.
#'
#' @examples
#' ## Simulate two matrices M and U with an envelope structure
#' data <- MenvU_sim(p = 20, u = 5, wishart = TRUE, n = 200)
#' M <- data$M
#' U <- data$U
#' G <- data$Gamma
#' Gamma_1D <- OptM1D(M, U, u = 5)
#' subspace(Gamma_1D, G)
#'
#' @references Cook, R.D. and Zhang, X., 2016. Algorithms for envelope estimation. Journal of Computational and Graphical Statistics, 25(1), pp.284-300.
#'
#' Wen, Z. and Yin, W., 2013. A feasible method for optimization with orthogonality constraints. Mathematical Programming, 142(1-2), pp.397-434.
#'
#' @export
OptM1D <- function(M, U, u, ...) {
  if(dim(U)[1]!=dim(U)[2]){
    U <- tcrossprod(U)
  }
  p <-  dim(U)[2]
  if(u < p){
    Mnew <- M
    Unew <- U
    G <- matrix(0, p, u)
    G0 <- diag(p)
    for(k in 1:u){
      gk <- ballGBB1D(Mnew, Unew, ...)$X
      G[, k] <- G0 %*% gk
      G0 <- qr.Q(qr(G[, 1:k]),complete=T)[,(k+1):p]
      Mnew <- t(G0) %*% M %*% G0
      Unew <- t(G0) %*% U %*% G0
    }
    Gamma <- G
  }else{
    Gamma <- diag(p)
  }
  Gamma
}
