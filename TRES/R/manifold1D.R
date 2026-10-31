##################################################
# Manifold   function 1D                         #
##################################################
#' @import ManifoldOptim
mod <- Module("ManifoldOptim_module", PACKAGE = "ManifoldOptim")
mani.params <- get.manifold.params(IsCheckParams = TRUE)

fun1D_mfd <- function(M, U) {
  n <- dim(M)[2]
  mw <- function(w) { matrix(w, n, 1) }
  f <- function(w) { W <- mw(w); log(t(W) %*% M %*% W) + log(t(W) %*% chol2inv(chol(M+U)) %*% W)  }
  g <- function(w) { W <- mw(w); 2*(M %*% W/(as.numeric(t(W) %*% M %*% W))+
                                      chol2inv(chol(M+U)) %*% W/(as.numeric(t(W) %*% chol2inv(chol(M+U)) %*% W))) }

  prob <- methods::new(mod$RProblem, f, g)
  mani.defn <- ManifoldOptim::get.stiefel.defn(n, 1)

  return(list(prob=prob, mani.defn=mani.defn))
}

##################################################
# Manifold1D optimization solve for gamma        #
##################################################

first1D <- function(M, U, ...) {

  opts <- list(...)
  if(is.null(opts$maxiter) || opts$maxiter < 0 || opts$maxiter > 2^20) opts$maxiter=500
  if(is.null(opts$tol) || opts$tol < 0 || opts$tol > 1) opts$tol=1e-08
  if(is.null(opts$method)) opts$method="RBFGS"
  if(is.null(opts$check)) opts$check= FALSE

  res  <- fun1D_mfd(M, U)
  prob <- res$prob
  mani.defn <- res$mani.defn
  mani.params <- get.manifold.params(IsCheckParams = opts$check)
  solver.params <- get.solver.params(Max_Iteration = opts$maxiter, Tolerance=opts$tol, IsCheckParams = opts$check)

  W0 <- get_ini1D(M, U)
  gamma <- ManifoldOptim::manifold.optim(prob, mani.defn, method = opts$method,
                          mani.params = mani.params, solver.params = solver.params, x0 = W0)
  n <- dim(M)[2]
  return(matrix(gamma$xopt,n,1))
}


##################################################
#  1D optimization solve for envelope basis      #
##################################################
#' Estimate the envelope subspace (\pkg{ManifoldOptim} 1D)
#'
#' The 1D algorithm (Cook and Zhang 2016) implemented with Riemannian manifold optimization from R package \pkg{ManifoldOptim}.
#'
#' @param M The \eqn{p}-by-\eqn{p} positive definite matrix \eqn{M} in the envelope objective function.
#' @param U The \eqn{p}-by-\eqn{p} positive semi-definite matrix \eqn{U} in the envelope objective function.
#' @param u An integer between 0 and \eqn{n} representing the envelope dimension.
#' @param ... Additional user-defined arguments:
#' \itemize{
#'   \item{\code{maxiter}: The maximal number of iterations.}
#'   \item{\code{tol}: The tolerance used to assess convergence. See Huang et al. (2018) for details on how this is used.}
#'   \item{\code{method}: The name of optimization method supported by R package \pkg{ManifoldOptim}.
#'     \itemize{
#'       \item{\code{"LRBFGS"}}: Limited-memory RBFGS
#'       \item{\code{"LRTRSR1"}}: Limited-memory RTRSR1
#'       \item{\code{"RBFGS"}}: Riemannian BFGS
#'       \item{\code{"RBroydenFamily"}}: Riemannian Broyden family
#'       \item{\code{"RCG"}}: Riemannian conjugate gradients
#'       \item{\code{"RNewton"}}: Riemannian line-search Newton
#'       \item{\code{"RSD"}}: Riemannian steepest descent
#'       \item{\code{"RTRNewton"}}: Riemannian trust-region Newton
#'       \item{\code{"RTRSD"}}: Riemannian trust-region steepest descent
#'       \item{\code{"RTRSR1"}}: Riemannian trust-region symmetric rank-one update
#'       \item{\code{"RWRBFGS"}}: Riemannian BFGS
#'       }
#'    }
#'  \item{\code{check}: Logical value. Should internal manifold object check inputs and print summary message before optimization.}
#'  }
#'  The default values are: \code{maxiter = 500; tol = 1e-08; method = "RCG"; check = FALSE}.
#'
#' @details Estimate \code{M}-envelope of \code{span(U)}. The dimension of the envelope is \code{u}.
#'
#' @return Return the estimated orthogonal basis of the envelope subspace.
#'
#' @references
#' Cook, R.D. and Zhang, X., 2016. Algorithms for envelope estimation. Journal of Computational and Graphical Statistics, 25(1), pp.284-300.
#'
#' Huang, W., Absil, P.A., Gallivan, K.A. and Hand, P., 2018. ROPTLIB: an object-oriented C++ library for optimization on Riemannian manifolds. ACM Transactions on Mathematical Software (TOMS), 44(4), pp.1-21.
#'
#' @seealso \code{\link{MenvU_sim}, \link{subspace}}
#'
#' @examples
#' ## Simulate two matrices M and U with an envelope structure
#' data <- MenvU_sim(p = 20, u = 5, wishart = TRUE, n = 200)
#' M <- data$M
#' U <- data$U
#' G <- data$Gamma
#' Gamma_1D <- manifold1D(M, U, u = 5)
#' subspace(Gamma_1D, G)
#'
#' @export
manifold1D <- function(M, U, u, ...){

  if(dim(U)[1]!=dim(U)[2]) {
    U = tcrossprod(U)
  }
  p = dim(U)[2]
  if(u < p) {
    Mnew <- M
    Unew <- U
    G <- matrix(0, p, u)
    G0 <- diag(p)
    for(k in 1:u) {
      gk <- first1D(Mnew, Unew, ...)
      G[, k] <- G0 %*% gk
      G0 <- qr.Q(qr(G[, 1:k]),complete=TRUE)[,(k+1):p]
      Mnew <- t(G0) %*% M %*% G0
      Unew <- t(G0) %*% U %*% G0
    }
    Gamma <- G
  } else { Gamma <- diag(p) }
  return(Gamma)
}

