#' @title Pure Newton-Raphson Optimization
#'
#' @description
#' Performs unconstrained minimization using the Newton-Raphson algorithm. 
#' This implementation utilizes the second-order derivative (Hessian) information 
#' directly to determine the search direction, supported by a Strong Wolfe line search.
#'
#' @section Comparison with Existing Functions:
#' This function adds three features for rigorous convergence control. First, it 
#' applies an \bold{AND rule}: all selected convergence criteria must be satisfied 
#' simultaneously. Second, users can choose among \bold{eight distinct criteria} 
#' (e.g., changes in $f$, $x$, gradient, or predicted decrease) instead of relying 
#' on fixed defaults. Third, it provides an \bold{optional verification} using the 
#' Hessian computed from derivatives (analytically when provided, or via numerical 
#' differentiation). Checking the positive definiteness of this Hessian at the final 
#' solution reduces the risk of declaring convergence at non-minimizing stationary 
#' points, such as saddle points.
#'
#' @references
#' Nocedal, J., & Wright, S. J. (2006). \emph{Numerical Optimization}. Springer.
#'
#' Akcelik, V. (2011). Newton-Raphson Method. \emph{Encyclopedia of Parallel Computing}.
#'
#' @param start Numeric vector. Initial values for the parameters.
#' @param objective Function. The scalar objective function to be minimized.
#' @param gradient Function (optional). Returns the gradient vector. If \code{NULL}, 
#' numerical derivatives are used.
#' @param hessian Function (optional). Returns the Hessian matrix. If \code{NULL}, 
#' a numerical Hessian is computed for both direction finding and verification.
#' @param ... Additional arguments passed to the objective, gradient, and hessian functions.
#'
#' @param use_abs_f Logical. Criterion 1: \eqn{|f_{new} - f_{old}| <} \code{tol_abs_f}.
#' @param use_rel_f Logical. Criterion 2: \eqn{|(f_{new} - f_{old}) / f_{old}| <} \code{tol_rel_f}.
#' @param use_abs_x Logical. Criterion 3: \eqn{\max |x_{new} - x_{old}| <} \code{tol_abs_x}.
#' @param use_rel_x Logical. Criterion 4: \eqn{\max |(x_{new} - x_{old}) / x_{old}| <} \code{tol_rel_x}.
#' @param use_grad Logical. Criterion 5: \eqn{\|g\|_\infty <} \code{tol_grad}.
#' @param use_posdef Logical. Criterion 6: Positive definiteness of the Hessian computed from derivatives.
#' @param use_pred_f Logical. Criterion 7: Predicted decrease based on the inverse-Hessian 
#' approximation, \eqn{0.5 g^T H^{-1} g <} \code{tol_pred_f}.
#' @param use_pred_f_avg Logical. Criterion 8: Predicted decrease per parameter, 
#' \eqn{(0.5 g^T H^{-1} g)/n <} \code{tol_pred_f_avg}.
#'
#' @param control A list of control parameters:
#' \itemize{
#'   \item \code{max_iter}: Maximum number of iterations (default: 10000).
#'   \item \code{tol_abs_f}, \code{tol_rel_f}: Tolerances for function-based criteria.
#'   \item \code{tol_abs_x}, \code{tol_rel_x}: Tolerances for parameter-based criteria.
#'   \item \code{tol_grad}: Tolerance for the gradient norm (default: 1e-4).
#'   \item \code{tol_pred_f}, \code{tol_pred_f_avg}: Tolerances for predicted decrease.
#'   \item \code{wolfe_c1}, \code{wolfe_c2}: Strong Wolfe line search parameters.
#'   \item \code{ls_max_steps}: Maximum line search steps (default: 50).
#'   \item \code{fallback_to_steepest}: Logical; if Hessian is not PD, use steepest descent (default: TRUE).
#'   \item \code{hessian_regularization}: Constant added to Hessian diagonal for stability.
#' }
#'
#' @return A list containing:
#' \item{par}{Optimized parameter vector.}
#' \item{objective}{Final objective function value.}
#' \item{converged}{Logical; TRUE if all selected criteria were met (AND rule).}
#' \item{status}{Termination status message.}
#' \item{iter}{Number of iterations performed.}
#' \item{time}{Execution time in seconds.}
#' \item{max_grad}{Maximum absolute element of the final gradient.}
#' \item{Hess_is_pd}{Logical; TRUE if the computed Hessian was positive definite.}
#' \item{Hess_min_eig}{Minimum eigenvalue of the computed Hessian.}
#' \item{hessian}{The Hessian matrix at the solution.}
#'
#' @examples
#' # 1. Define the Rosenbrock function and its gradient
#' rosenbrock <- function(x) {
#'   100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
#' }
#' 
#' rosenbrock_grad <- function(x) {
#'   c(-400 * x[1] * (x[2] - x[1]^2) - 2 * (1 - x[1]),
#'      200 * (x[2] - x[1]^2))
#' }
#' 
#' # 2. Run optimization with strict AND convergence criteria
#' res <- newton_raphson(
#'   start = c(-1.2, 1),
#'   objective = rosenbrock,
#'   gradient = rosenbrock_grad,
#'   use_rel_x = TRUE,
#'   use_grad = TRUE,
#'   use_posdef = TRUE # Verify Hessian at the solution
#' )
#' 
#' # 3. Check results
#' print(res$par)
#' print(res$converged)
#' 
#' @export
newton_raphson <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,
    use_abs_f      = FALSE,
    use_rel_f      = FALSE,
    use_abs_x      = FALSE,
    use_rel_x      = TRUE,
    use_grad       = TRUE,
    use_posdef     = TRUE,
    use_pred_f     = FALSE,
    use_pred_f_avg = FALSE,
    control        = list(),
    ...
) {
  
  ctrl0 <- list(
    max_iter        = 10000L,
    tol_abs_f       = 1e-6,
    tol_rel_f       = 1e-6,
    tol_abs_x       = 1e-6,
    tol_rel_x       = 1e-6,
    tol_grad        = 1e-4,
    tol_pred_f      = 1e-4,
    tol_pred_f_avg  = 1e-4,
    
    wolfe_c1        = 1e-4,
    wolfe_c2        = 0.9,
    ls_alpha0       = 1.0,
    ls_expand       = 2.0,
    ls_max_steps    = 50L,
    use_num_grad_if_missing = TRUE,
    use_num_hess_if_missing = TRUE,
    verbose         = FALSE,
    keep_history    = FALSE,
    fallback_to_steepest    = TRUE,
    hessian_regularization  = 1e-8
  )
  ctrl <- utils::modifyList(ctrl0, control)
  
  # ---------- Helpers ----------
  max_abs <- function(x) {
    x <- as.numeric(x)
    if (length(x) == 0L) return(NA_real_)
    max(abs(x), na.rm = TRUE)
  }
  
  rel <- function(num, den) {
    if (!is.finite(num) || !is.finite(den)) return(NA_real_)
    num / max(1, abs(den))
  }
  
  is_pd_mat <- function(M) {
    if (is.null(M)) return(list(is_pd = NA, min_eig = NA_real_))
    Ms <- 0.5 * (M + t(M))
    ev <- tryCatch(eigen(Ms, symmetric = TRUE, only.values = TRUE)$values,
                   error = function(e) NA_real_)
    min_eig <- suppressWarnings(min(ev))
    list(is_pd = is.finite(min_eig) && (min_eig > 0), min_eig = min_eig)
  }
  
  safe_obj <- function(x) {
    fx <- tryCatch(objective(x, ...), error = function(e) NA_real_)
    if (!is.finite(fx)) Inf else as.numeric(fx)
  }
  
  safe_grad <- function(x) {
    if (!is.null(gradient)) {
      g <- tryCatch(gradient(x, ...), error = function(e) NULL)
      if (!is.null(g) && all(is.finite(g))) return(as.numeric(g))
    }
    if (!requireNamespace("numDeriv", quietly = TRUE)) return(rep(NA_real_, length(x)))
    numDeriv::grad(objective, x, ...)
  }
  
  safe_hess <- function(x) {
    if (!is.null(hessian)) {
      H <- tryCatch(hessian(x, ...), error = function(e) NULL)
      if (is.matrix(H)) return(0.5 * (H + t(H)))
    }
    if (!requireNamespace("numDeriv", quietly = TRUE)) return(NULL)
    H <- tryCatch(numDeriv::hessian(objective, x, ...), error = function(e) NULL)
    if (is.matrix(H)) 0.5 * (H + t(H)) else NULL
  }
  
  # ---------- Unified Convergence Checker (AND logic) ----------
  check_convergence <- function(g_inf, g, p_step, f_old, f_new, x_old, x_new, n) {
    res <- TRUE
    
    if (use_grad) res <- res && (g_inf <= ctrl$tol_grad)
    if (use_abs_f && !is.na(f_old)) res <- res && (abs(f_new - f_old) <= ctrl$tol_abs_f)
    if (use_rel_f && !is.na(f_old)) res <- res && (abs(rel(f_new - f_old, f_old)) <= ctrl$tol_rel_f)
    if (use_abs_x && !is.null(x_old)) res <- res && (max_abs(x_new - x_old) <= ctrl$tol_abs_x)
    if (use_rel_x && !is.null(x_old)) {
      rel_x <- max(abs(x_new - x_old) / pmax(1, abs(x_old)))
      res <- res && (rel_x <= ctrl$tol_rel_x)
    }
    
    if (use_pred_f || use_pred_f_avg) {
      if (all(is.finite(p_step))) {
        pred_dec <- as.numeric(0.5 * t(g) %*% (-p_step))
        if (use_pred_f) res <- res && (pred_dec <= ctrl$tol_pred_f)
        if (use_pred_f_avg) res <- res && ((pred_dec / n) <= ctrl$tol_pred_f_avg)
      } else {
        res <- FALSE
      }
    }
    
    if (use_posdef) {
      if (!isTRUE(Hess_checked)) {
        Hc <- safe_hess(x_new)
        hpd <- is_pd_mat(Hc)
        Hess_is_pd_cache <<- hpd$is_pd
        Hess_min_eig_cache <<- hpd$min_eig
        Hess_checked <<- TRUE
      }
      res <- res && isTRUE(Hess_is_pd_cache)
    }
    
    res
  }
  
  # ---------- Line Search (Strong Wolfe) ----------
  wolfe_ls <- function(x, f0, g0, p) {
    phi <- function(a) safe_obj(x + a * p)
    gphi <- function(a) safe_grad(x + a * p)
    dphi0 <- as.numeric(crossprod(g0, p))
    
    if (!is.finite(dphi0) || dphi0 >= 0) {
      return(list(ok = FALSE, alpha = 0, x.new = x, f.new = f0, g.new = g0, why = "not_descent"))
    }
    
    zoom <- function(alo, flo, ahi, fhi) {
      for (j in seq_len(ctrl$ls_max_steps)) {
        aj <- 0.5 * (alo + ahi)
        xj <- x + aj * p
        fj <- safe_obj(xj)
        if (!is.finite(fj) || fj > f0 + ctrl$wolfe_c1 * aj * dphi0 || fj >= flo) {
          ahi <- aj; fhi <- fj
        } else {
          gj <- gphi(aj)
          dj <- as.numeric(crossprod(gj, p))
          if (abs(dj) <= -ctrl$wolfe_c2 * dphi0) {
            return(list(ok = TRUE, alpha = aj, x.new = xj, f.new = fj, g.new = gj, why = "wolfe"))
          }
          if (dj * (ahi - alo) >= 0) { ahi <- alo; fhi <- flo }
          alo <- aj; flo <- fj
        }
      }
      list(ok = FALSE, alpha = alo, x.new = x + alo*p, f.new = flo,
           g.new = safe_grad(x + alo*p), why = "zoom_limit")
    }
    
    a0 <- 0
    a1 <- min(ctrl$ls_alpha0, 1.0)
    f_prev <- f0
    
    for (i in seq_len(ctrl$ls_max_steps)) {
      xi <- x + a1 * p
      fi <- phi(a1)
      if (!is.finite(fi) || fi > f0 + ctrl$wolfe_c1 * a1 * dphi0 || (i > 1 && fi >= f_prev)) {
        return(zoom(a0, f_prev, a1, fi))
      }
      gi <- gphi(a1)
      dpi <- as.numeric(crossprod(gi, p))
      if (abs(dpi) <= -ctrl$wolfe_c2 * dphi0) {
        return(list(ok = TRUE, alpha = a1, x.new = xi, f.new = fi, g.new = gi, why = "wolfe"))
      }
      if (dpi >= 0) return(zoom(a1, fi, a0, f_prev))
      a0 <- a1; f_prev <- fi; a1 <- a1 * ctrl$ls_expand
    }
    
    list(ok = FALSE, alpha = a1, x.new = x + a1*p, f.new = phi(a1), g.new = gphi(a1), why = "ls_limit")
  }
  
  # ---------- Optimization Initialization ----------
  x <- as.numeric(start)
  n <- length(x)
  f <- safe_obj(x)
  g <- safe_grad(x)
  
  it <- 0L
  x_old <- NULL
  f_old <- NA_real_
  converged <- FALSE
  status <- NULL
  
  Hess_checked <- FALSE
  Hess_is_pd_cache <- NA
  Hess_min_eig_cache <- NA_real_
  
  p <- rep(NA_real_, n)
  
  # >>> BUG FIX: define H_final up front so return is always valid
  H_final <- NULL
  
  # ---------- Main Loop ----------
  tinfo <- system.time({
    repeat {
      it <- it + 1L
      if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      
      g_inf <- max_abs(g)
      
      if (check_convergence(g_inf, g, p, f_old, f, x_old, x, n)) {
        status <- "converged"
        converged <- TRUE
        break
      }
      
      H <- safe_hess(x)
      if (is.null(H)) { status <- "hessian_failed"; break }
      
      # >>> BUG FIX: keep the most recent Hessian for output
      H_final <- H
      
      h_info <- is_pd_mat(H)
      
      if (!isTRUE(h_info$is_pd)) {
        if (isTRUE(ctrl$fallback_to_steepest)) {
          p <- -g
        } else {
          status <- "hessian_not_positive_definite"
          break
        }
      } else {
        p <- tryCatch(solve(H + diag(ctrl$hessian_regularization, n), -g),
                      error = function(e) -g)
      }
      
      ls <- wolfe_ls(x, f, g, p)
      
      if (!ls$ok) {
        # Even if LS fails, ensure H_final matches the last x we return (current x)
        if (check_convergence(max_abs(ls$g.new), ls$g.new, p, f_old, ls$f.new, x_old, ls$x.new, n)) {
          status <- "converged"
          converged <- TRUE
          x <- ls$x.new; f <- ls$f.new; g <- ls$g.new
          # >>> BUG FIX: recompute H_final at the returned x (best effort)
          H_final <- safe_hess(x)
        } else {
          status <- paste0("line_search_failed:", ls$why)
        }
        break
      }
      
      x_old <- x
      f_old <- f
      x <- ls$x.new
      f <- ls$f.new
      g <- ls$g.new
      Hess_checked <- FALSE
    }
  })
  
  # If converged without ever computing Hessian inside loop (possible if start already converged)
  if (isTRUE(converged) && is.null(H_final)) {
    H_final <- safe_hess(x)
  }
  
  out <- list(
    par          = x,
    objective    = f,
    converged    = converged,
    status       = status,
    iter         = it,
    time         = as.numeric(tinfo["user.self"] + tinfo["sys.self"]),
    max_grad     = max_abs(g),
    Hess_is_pd   = Hess_is_pd_cache,
    Hess_min_eig = Hess_min_eig_cache,
    hessian      = H_final
  )
  
  if (isTRUE(ctrl$keep_history)) out$history <- list()
  out
}
