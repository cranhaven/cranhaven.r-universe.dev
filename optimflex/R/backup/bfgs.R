#' @title BFGS Optimization with Eight Convergence Criteria
#'
#' @description
#' Performs unconstrained minimization using the Broyden-Fletcher-Goldfarb-Shanno (BFGS) algorithm.
#' This implementation is designed for rigorous convergence control, offering multiple 
#' user-selectable stopping rules.
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
#' Broyden, C. G. (1970). The convergence of a class of double-rank minimization
#' algorithms: Part I. \emph{Journal of the Institute of Mathematics and Its Applications},
#' 6(1), 76-90.
#'
#' Fletcher, R. (1970). A new approach to variable metric algorithms.
#' \emph{The Computer Journal}, 13(3), 317-322.
#'
#' Goldfarb, D. (1970). A family of variable-metric methods derived by
#' variational means. \emph{Mathematics of Computation}, 24(111), 23-26.
#'
#' Shanno, D. F. (1970). Conditioning of quasi-Newton methods for function
#' minimization. \emph{Mathematics of Computation}, 24(111), 647-656.
#'
#' @param start Numeric vector. Initial values for the parameters to be optimized.
#' @param objective Function. The scalar objective function to be minimized.
#' @param gradient Function (optional). Returns the gradient vector. If \code{NULL}, 
#' numerical derivatives are used via \code{numDeriv}.
#' @param hessian Function (optional). Returns the Hessian matrix. Used for verifying
#' positive definiteness when \code{use_posdef} is \code{TRUE}. If \code{NULL}, 
#' the Hessian is computed via numerical differentiation.
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
#'   \item \code{tol_abs_f}: Tolerance for Criterion 1 (default: 1e-6).
#'   \item \code{tol_rel_f}: Tolerance for Criterion 2 (default: 1e-6).
#'   \item \code{tol_abs_x}: Tolerance for Criterion 3 (default: 1e-6).
#'   \item \code{tol_rel_x}: Tolerance for Criterion 4 (default: 1e-6).
#'   \item \code{tol_grad}: Tolerance for Criterion 5 (default: 1e-4).
#'   \item \code{tol_pred_f}: Tolerance for Criterion 7 (default: 1e-4).
#'   \item \code{tol_pred_f_avg}: Tolerance for Criterion 8 (default: 1e-4).
#'   \item \code{wolfe_c1}: Parameter for Armijo condition (default: 1e-4).
#'   \item \code{wolfe_c2}: Parameter for curvature condition (default: 0.9).
#'   \item \code{ls_alpha0}: Initial step length for line search (default: 1.0).
#'   \item \code{ls_expand}: Expansion factor for line search (default: 2.0).
#'   \item \code{ls_max_steps}: Maximum line search steps (default: 50).
#'   \item \code{Hinv_init_diag}: Scale for initial inverse Hessian diagonal (default: 1.0).
#'   \item \code{reset_on_curvature}: Reset \eqn{Hinv} if curvature \eqn{s^T y \le 0} (default: TRUE).
#'   \item \code{curvature_eps}: Small constant for curvature check (default: 1e-12).
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
#' \item{Hess_is_pd}{Logical; TRUE if the Hessian was positive definite (if checked).}
#' \item{Hess_min_eig}{Minimum eigenvalue of the Hessian (if checked).}
#' \item{hessian}{The Hessian matrix at the final point, if computed.}
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
#' res <- bfgs(
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
bfgs <- function(
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
    control        = list()
) {
  
  # ---------- Default Controls ----------
  ctrl0 <- list(
    max_iter           = 10000L,
    tol_abs_f          = 1e-6,
    tol_rel_f          = 1e-6,
    tol_abs_x          = 1e-6,
    tol_rel_x          = 1e-6,
    tol_grad           = 1e-4,
    tol_pred_f         = 1e-4,
    tol_pred_f_avg     = 1e-4,
    wolfe_c1           = 1e-4,
    wolfe_c2           = 0.9,
    ls_alpha0          = 1.0,
    ls_expand          = 2.0,
    ls_max_steps       = 50L,
    reset_on_curvature = TRUE,
    curvature_eps      = 1e-12,
    Hinv_init_diag     = 1.0,
    verbose            = FALSE,
    keep_history       = FALSE
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
    if (is.null(M)) return(list(is_pd = FALSE, min_eig = NA_real_))
    Ms <- 0.5 * (M + t(M))
    ev <- tryCatch(eigen(Ms, symmetric = TRUE, only.values = TRUE)$values,
                   error = function(e) NA_real_)
    min_eig <- suppressWarnings(min(ev))
    list(is_pd = is.finite(min_eig) && (min_eig > 0), min_eig = min_eig)
  }
  
  safe_obj <- function(x) {
    fx <- tryCatch(objective(x), error = function(e) NA_real_)
    if (!is.finite(fx)) Inf else as.numeric(fx)
  }
  
  safe_grad <- function(x) {
    if (!is.null(gradient)) {
      g <- tryCatch(gradient(x), error = function(e) NULL)
      if (!is.null(g) && all(is.finite(g))) return(as.numeric(g))
    }
    if (!requireNamespace("numDeriv", quietly = TRUE)) {
      stop("numDeriv is required for numerical gradients.")
    }
    numDeriv::grad(objective, x)
  }
  
  safe_hess <- function(x) {
    if (!is.null(hessian)) {
      H <- tryCatch(hessian(x), error = function(e) NULL)
      if (is.matrix(H)) return(0.5 * (H + t(H)))
    }
    if (!requireNamespace("numDeriv", quietly = TRUE)) return(NULL)
    H <- tryCatch(numDeriv::hessian(objective, x), error = function(e) NULL)
    if (is.matrix(H)) 0.5 * (H + t(H)) else NULL
  }
  
  # ---------- Fast Convergence Checker ----------
  check_convergence_fast <- function(g_inf, g, Hinv, f_old, f_new, x_old, x_new, n,
                                     allow_pred = TRUE) {
    res <- TRUE
    
    if (use_grad) res <- res && (g_inf <= ctrl$tol_grad)
    if (use_abs_f && !is.na(f_old)) res <- res && (abs(f_new - f_old) <= ctrl$tol_abs_f)
    if (use_rel_f && !is.na(f_old)) res <- res && (abs(rel(f_new - f_old, f_old)) <= ctrl$tol_rel_f)
    if (use_abs_x && !is.null(x_old)) res <- res && (max_abs(x_new - x_old) <= ctrl$tol_abs_x)
    if (use_rel_x && !is.null(x_old)) {
      rel_x <- max(abs(x_new - x_old) / pmax(1, abs(x_old)))
      res <- res && (rel_x <= ctrl$tol_rel_x)
    }
    
    if ((use_pred_f || use_pred_f_avg) && allow_pred) {
      pred_dec <- NA_real_
      if (is.matrix(Hinv) && all(is.finite(Hinv)) && all(is.finite(g))) {
        pred_dec <- as.numeric(0.5 * t(g) %*% Hinv %*% g)
      }
      if (!is.finite(pred_dec)) return(FALSE)
      if (use_pred_f) res <- res && (pred_dec <= ctrl$tol_pred_f)
      if (use_pred_f_avg) res <- res && ((pred_dec / n) <= ctrl$tol_pred_f_avg)
    }
    
    res
  }
  
  # ---------- Line Search (Strong Wolfe) ----------
  wolfe_ls <- function(x, f0, g0, p) {
    phi  <- function(a) safe_obj(x + a * p)
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
      
      a0 <- a1
      f_prev <- fi
      a1 <- a1 * ctrl$ls_expand
    }
    
    list(ok = FALSE, alpha = a1, x.new = x + a1*p, f.new = phi(a1), g.new = gphi(a1), why = "ls_limit")
  }
  
  # ---------- Initialization ----------
  x <- as.numeric(start)
  n <- length(x)
  f <- safe_obj(x)
  g <- safe_grad(x)
  Hinv <- diag(ctrl$Hinv_init_diag, n)
  it <- 0L
  x_old <- NULL
  f_old <- NA_real_
  converged <- FALSE
  status <- NULL
  g_inf <- max_abs(g)
  Hess_is_pd_cache <- NA
  Hess_min_eig_cache <- NA_real_
  H_final <- NULL
  
  # ---------- Main Optimization Loop ----------
  tinfo <- system.time({
    repeat {
      it <- it + 1L
      if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      
      g_inf <- max_abs(g)
      
      if (check_convergence_fast(g_inf, g, Hinv, f_old, f, x_old, x, n)) {
        status <- "converged"
        converged <- TRUE
        break
      }
      
      p <- as.numeric(-Hinv %*% g)
      ls <- wolfe_ls(x, f, g, p)
      
      if (!ls$ok) {
        # Check convergence one last time at the final attempted point
        allow_pred <- !(use_pred_f || use_pred_f_avg)
        if (check_convergence_fast(max_abs(ls$g.new), ls$g.new, Hinv, f_old, ls$f.new, x_old, ls$x.new, n,
                                   allow_pred = allow_pred)) {
          status <- "converged"
          converged <- TRUE
          x <- ls$x.new; f <- ls$f.new; g <- ls$g.new; g_inf <- max_abs(g)
        } else {
          status <- paste0("line_search_failed:", ls$why)
        }
        break
      }
      
      x_old <- x; f_old <- f
      x <- ls$x.new; f <- ls$f.new
      g_new <- ls$g.new
      s <- x - x_old; y <- g_new - g
      g <- g_new
      
      # BFGS update
      ys <- as.numeric(crossprod(y, s))
      if (is.finite(ys) && ys > ctrl$curvature_eps) {
        Hy <- Hinv %*% y
        yHy <- as.numeric(crossprod(y, Hy))
        Hinv <- Hinv + ((1 + yHy / ys) * (s %*% t(s)) / ys) - ((s %*% t(Hy) + Hy %*% t(s)) / ys)
      } else if (isTRUE(ctrl$reset_on_curvature)) {
        Hinv <- diag(ctrl$Hinv_init_diag, n)
      }
    }
    
    # ---------- Final Hessian Verification ----------
    if (converged) {
      H_final <- safe_hess(x)
      if (use_posdef) {
        hpd <- is_pd_mat(H_final)
        Hess_is_pd_cache <- hpd$is_pd
        Hess_min_eig_cache <- hpd$min_eig
        if (!isTRUE(Hess_is_pd_cache)) {
          status <- "converged_but_not_positive_definite"
          converged <- FALSE
        }
      }
    }
  })
  
  list(
    par          = x,
    objective    = f,
    converged    = converged,
    status       = status,
    iter         = it,
    time         = as.numeric(tinfo["user.self"] + tinfo["sys.self"]),
    max_grad     = g_inf,
    Hess_is_pd   = Hess_is_pd_cache,
    Hess_min_eig = Hess_min_eig_cache,
    hessian      = H_final
  )
}