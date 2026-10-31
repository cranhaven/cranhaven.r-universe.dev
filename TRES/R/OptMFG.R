#' Estimate the envelope subspace (\pkg{OptM} FG)
#'
#' The FG algorithm to estimate the envelope subspace based on the curvilinear search algorithm for optimization on Stiefel manifold. The curvilinear algorithm is developed by Wen and Yin (2013) and the Matlab version is implemented in the Matlab package \pkg{OptM}.
#'
#' @param M The \eqn{p}-by-\eqn{p} positive definite matrix \eqn{M} in the envelope objective function.
#' @param U The \eqn{p}-by-\eqn{p} positive semi-definite matrix \eqn{U} in the envelope objective function.
#' @param u An integer between 0 and \eqn{n} representing the envelope dimension. Ignored if \code{Gamma_init} is provided.
#' @param Gamma_init Initial envelope subspace basis. The default value is the estimator from \code{OptM1D(M, U, u)}.
#' @param ... Additional user-defined arguments for the curvilinear search algorithm:
#' \itemize{
#'  \item \code{maxiter}: The maximal number of iterations.
#'  \item \code{xtol}: The convergence tolerance for \eqn{\Gamma}, e.g., \eqn{||\Gamma^{(k)} - \Gamma^{(k-1)}||_F/\sqrt{p}}
#'  \item \code{gtol}: The convergence tolerance for the projected gradient, e.g., \eqn{||G^{(k)} - \Gamma^{(k)} (G^{(t)})^T \Gamma^{(t)}||_F}
#'  \item \code{ftol}: The convergence tolerance for objective function \eqn{F}, e.g., \eqn{|F^{(k)} - F^{(k-1)}|/(1+|F^{(k-1)}|)}. Usually, \code{max{xtol, gtol} > ftol}
#' }
#' The default values are: \code{maxiter=500; xtol=1e-08; gtol=1e-08; ftol=1e-12.}
#'
#' @details If \code{Gamma_init} is provided, then the envelope dimension \code{u = ncol(Gamma_init)}.
#'
#' The function \code{OptMFG} calls the function \code{\link{OptStiefelGBB}} internally which implements the curvilinear search algorithm.
#'
#' The objective function \eqn{F(\Gamma)} and its gradient \eqn{G(\Gamma)} in curvilinear search algorithm are:
#' \deqn{F(\Gamma)=\log|\Gamma^T M \Gamma|+\log| \Gamma^T(M+U)^{-1}\Gamma|}
#' \deqn{G(\Gamma) = dF/d \Gamma = 2 M \Gamma (\Gamma^T M \Gamma)^{-1} + 2 (M + U)^{-1} \Gamma (\Gamma^T (M + U)^{-1} \Gamma)^{-1}}
#'
#' @return Return the estimated orthogonal basis of the envelope subspace.
#'
#' @references Wen, Z. and Yin, W., 2013. A feasible method for optimization with orthogonality constraints. Mathematical Programming, 142(1-2), pp.397-434.
#'
#' @seealso \code{\link{OptStiefelGBB}}
#'
#' @examples
#' ##simulate two matrices M and U with an envelope structure
#' data <- MenvU_sim(p=20, u=5, wishart = TRUE, n = 200)
#' M <- data$M
#' U <- data$U
#' G <- data$Gamma
#' Gamma_FG <- OptMFG(M, U, u=5)
#' subspace(Gamma_FG, G)

#' @export
OptMFG <- function(M, U, u, Gamma_init = NULL, ...)
{
  if(is.null(Gamma_init)){
    if(missing(u)) stop("u is required since Gamma_init is NULL.")
    X <- OptM1D(M, U, u)
  }else{
    X <- Gamma_init
    rm(Gamma_init)
  }
  if(is.vector(X)){X <- as.matrix(X)}
  p <- dim(X)[1]
  u <- dim(X)[2]
  opts <- list(...)

  Gamma <- OptStiefelGBB(X, FGfun, opts, M, U)$X
  Gamma
}
