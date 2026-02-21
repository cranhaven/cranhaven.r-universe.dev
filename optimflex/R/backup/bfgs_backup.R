#' @title BFGS Optimization with Eight Convergence Criteria
#'
#' @description
#' Performs unconstrained minimization using the Broyden-Fletcher-Goldfarb-Shanno (BFGS) algorithm. 
#' 
#' @section Convergence Criteria and Strict 'AND' Logic:
#' Many standard optimization functions provide a limited set of convergence criteria 
#' and often rely on 'OR' logic. This approach stops the algorithm as soon as any 
#' single condition is met, which may lead to premature convergence as more criteria 
#' are included. Furthermore, these routines do not typically verify whether the 
#' Hessian is positive definite at the point of convergence. Without this 
#' second-order verification, there is an increased risk of the algorithm stopping 
#' at non-optimal critical points, such as saddle points. Additionally, conventional 
#' functions lack the flexibility for users to select from a variety of criteria 
#' to suit specific research needs. In contrast, this function addresses these 
#' issues by offering eight comprehensive convergence criteria and employing 
#' a strict 'AND' logic. By allowing users to select multiple criteria and 
#' requiring every selected condition to be satisfied simultaneously, it 
#' substantially diminishes the likelihood of converging to a non-optimal solution.
#'
#'#' @references
#' Broyden, C. G. (1970). The convergence of a class of double-rank minimization 
#' algorithms: Part I. Journal of the Institute of Mathematics and Its Applications, 
#' 6(1), 76-90.
#' 
#' Fletcher, R. (1970). A new approach to variable metric algorithms. 
#' The Computer Journal, 13(3), 317-322.
#' 
#' Goldfarb, D. (1970). A family of variable-metric methods derived by 
#' variational means. Mathematics of Computation, 24(111), 23-26.
#' 
#' Shanno, D. F. (1970). Conditioning of quasi-Newton methods for function 
#' minimization. Mathematics of Computation, 24(111), 647-656.
#' 
#' @param start Numeric vector. Initial values for the parameters to be optimized.
#' @param objective Function. The scalar objective function to be minimized.
#' @param gradient Function (optional). Returns the gradient vector. If NULL, numerical 
#' derivatives are used via \code{numDeriv}.
#' @param hessian Function (optional). Returns the Hessian matrix. Used specifically for 
#' verifying the positive definiteness if \code{use_posdef} is TRUE.
#'
#' @param use_abs_f Logical. Criteria 1: Absolute change in objective value $|f_{new} - f_{old}| < tol\_abs\_f$.
#' @param use_rel_f Logical. Criteria 2: Relative change in objective value $|(f_{new} - f_{old}) / f_{old}| < tol\_rel\_f$.
#' @param use_abs_x Logical. Criteria 3: Absolute change in parameters $\max|x_{new} - x_{old}| < tol\_abs\_x$.
#' @param use_rel_x Logical. Criteria 4: Relative change in parameters $\max|(x_{new} - x_{old}) / x_{old}| < tol\_rel\_x$.
#' @param use_grad Logical. Criteria 5: Infinity norm of the gradient $\max|g| < tol\_grad$.
#' @param use_posdef Logical. Criteria 6: Verification that the Hessian matrix is positive definite.
#' @param use_pred_f Logical. Criteria 7: Predicted decrease based on quadratic form $0.5 g^T H^{-1} g < tol\_pred\_f$.
#' @param use_pred_f_avg Logical. Criteria 8: Predicted decrease per parameter $(0.5 g^T H^{-1} g) / n < tol\_pred\_f\_avg$.
#'
#' @param control A list of control parameters:
#' \itemize{
#'   \item \code{max_iter}: Maximum number of iterations (default: 10000).
#'   \item \code{tol_grad}: Tolerance for gradient norm (default: 1e-4).
#'   \item \code{tol_rel_x}: Tolerance for relative parameter change (default: 1e-6).
#'   \item \code{wolfe_c1}: Parameter for Armijo condition (default: 1e-4).
#'   \item \code{wolfe_c2}: Parameter for curvature condition (default: 0.9).
#'   \item \code{Hinv_init_diag}: Initial value for the inverse Hessian diagonal (default: 1.0).
#' }
#'
#' @return A list containing:
#' \item{par}{Optimized parameter vector.}
#' \item{objective}{Final objective function value.}
#' \item{converged}{Logical flag indicating if all selected criteria were met.}
#' \item{status}{Character string describing the termination reason.}
#' \item{iter}{Total number of iterations performed.}
#' \item{time}{Execution time in seconds.}
#' \item{max_grad}{Final maximum absolute gradient element.}
#' \item{Hess_is_pd}{Logical; indicates if the final Hessian was positive definite.}
#' \item{Hess_min_eig}{Minimum eigenvalue of the final Hessian.}
#'
#' @export
#'
#' @examples
#' # Minimize the Rosenbrock function
#' rosenbrock <- function(x) {
#'   100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
#' }
#' res <- bfgs(start = c(-1.2, 1), objective = rosenbrock, use_rel_x = TRUE, use_grad = TRUE)
#' print(res$par)

bfgs <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,
    # Convergence selection flags (AND logic) 
    use_abs_f      = FALSE,  # |f_new - f_old| < tol_abs_f
    use_rel_f      = FALSE, # |(f_new - f_old) / f_old| < tol_rel_f 
    use_abs_x      = FALSE, # max|x_new - x_old| < tol_abs_x 
    use_rel_x      = TRUE, # max|(x_new - x_old) / x_old| < tol_rel_x 
    use_grad       = TRUE,  # max|g| < tol_grad 
    use_posdef     = TRUE, # Hessian is Positive Definite 
    use_pred_f     = FALSE, # 0.5 * g^T * Hinv * g < tol_pred_f 
    use_pred_f_avg = FALSE, # (0.5 * g^T * Hinv * g) / n < tol_pred_f_avg 
    control        = list()
) {
  
  # ---------- Default Controls ----------
  ctrl0 <- list(
    max_iter        = 10000L,
    # Tolerances in control list 
    tol_abs_f       = 1e-6,
    tol_rel_f       = 1e-6,
    tol_abs_x       = 1e-6,
    tol_rel_x       = 1e-6,
    tol_grad        = 1e-4,
    tol_pred_f      = 1e-4,
    tol_pred_f_avg  = 1e-4,
    
    # Line search and optimization settings
    wolfe_c1        = 1e-4,
    wolfe_c2        = 0.9,
    ls_alpha0       = 1.0,
    ls_expand       = 2.0,
    ls_max_steps    = 50L,
    reset_on_curvature = TRUE,
    curvature_eps   = 1e-12,
    Hinv_init_diag  = 1.0,
    use_num_grad_if_missing = TRUE,
    use_num_hess_if_missing = TRUE,
    verbose         = FALSE,
    keep_history    = FALSE,
    alpha_max       = 1e6,
    alpha_min       = 1e-16,
    reset_on_nonfinite_Hinv = TRUE,
    fallback_to_steepest    = TRUE
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
    ev <- tryCatch(eigen(Ms, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NA_real_)
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
    if (!requireNamespace("numDeriv", quietly = TRUE)) return(rep(NA_real_, length(x)))
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
  
  # ---------- Unified Convergence Checker (AND logic) ----------
  check_convergence <- function(g_inf, g, Hinv, f_old, f_new, x_old, x_new, n) {
    res <- TRUE
    
    # 1. Gradient norm (using pre-calculated g_inf)
    if (use_grad) res <- res && (g_inf <= ctrl$tol_grad)
    
    # 2. Absolute function value change
    if (use_abs_f && !is.na(f_old)) res <- res && (abs(f_new - f_old) <= ctrl$tol_abs_f)
    
    # 3. Relative function value change
    if (use_rel_f && !is.na(f_old)) res <- res && (abs(rel(f_new - f_old, f_old)) <= ctrl$tol_rel_f)
    
    # 4. Absolute parameter change
    if (use_abs_x && !is.null(x_old)) res <- res && (max_abs(x_new - x_old) <= ctrl$tol_abs_x)
    
    # 5. Relative parameter change
    if (use_rel_x && !is.null(x_old)) {
      rel_x <- max(abs(x_new - x_old) / pmax(1, abs(x_old)))
      res <- res && (rel_x <= ctrl$tol_rel_x)
    }
    
    # 6. Predicted function decrease (0.5 * g^T * Hinv * g)
    pred_dec <- as.numeric(0.5 * t(g) %*% Hinv %*% g)
    if (use_pred_f) res <- res && (pred_dec <= ctrl$tol_pred_f)
    
    # 7. Predicted decrease per parameter
    if (use_pred_f_avg) res <- res && ((pred_dec / n) <= ctrl$tol_pred_f_avg)
    
    # 8. Hessian Positive Definite
    if (use_posdef) {
      if (!isTRUE(Hess_checked)) {
        H <- safe_hess(x_new); hpd <- is_pd_mat(H)
        Hess_is_pd_cache <<- hpd$is_pd; Hess_min_eig_cache <<- hpd$min_eig; Hess_checked <<- TRUE
      }
      res <- res && isTRUE(Hess_is_pd_cache)
    }
    return(res)
  }
  
  # ---------- Line Search (Strong Wolfe) ----------
  wolfe_ls <- function(x, f0, g0, p) {
    phi  <- function(a) safe_obj(x + a * p)
    gphi <- function(a) safe_grad(x + a * p)
    dphi0 <- as.numeric(crossprod(g0, p))
    if (!is.finite(dphi0) || dphi0 >= 0) return(list(ok = FALSE, alpha = 0, x.new = x, f.new = f0, g.new = g0, why = "not_descent"))
    
    zoom <- function(alo, flo, ahi, fhi) {
      for (j in seq_len(ctrl$ls_max_steps)) {
        aj <- 0.5 * (alo + ahi); xj <- x + aj * p; fj <- safe_obj(xj)
        if (!is.finite(fj) || fj > f0 + ctrl$wolfe_c1 * aj * dphi0 || fj >= flo) { ahi <- aj; fhi <- fj } 
        else {
          gj <- gphi(aj); dj <- as.numeric(crossprod(gj, p))
          if (abs(dj) <= -ctrl$wolfe_c2 * dphi0) return(list(ok = TRUE, alpha = aj, x.new = xj, f.new = fj, g.new = gj, why = "wolfe"))
          if (dj * (ahi - alo) >= 0) { ahi <- alo; fhi <- flo }
          alo <- aj; flo <- fj
        }
      }
      list(ok = FALSE, alpha = alo, x.new = x + alo*p, f.new = flo, g.new = safe_grad(x + alo*p), why = "zoom_limit")
    }
    
    a0 <- 0; a1 <- min(ctrl$ls_alpha0, 1.0); f_prev <- f0
    for (i in seq_len(ctrl$ls_max_steps)) {
      xi <- x + a1 * p; fi <- phi(a1)
      if (!is.finite(fi) || fi > f0 + ctrl$wolfe_c1 * a1 * dphi0 || (i > 1 && fi >= f_prev)) return(zoom(a0, f_prev, a1, fi))
      gi <- gphi(a1); dpi <- as.numeric(crossprod(gi, p))
      if (abs(dpi) <= -ctrl$wolfe_c2 * dphi0) return(list(ok = TRUE, alpha = a1, x.new = xi, f.new = fi, g.new = gi, why = "wolfe"))
      if (dpi >= 0) return(zoom(a1, fi, a0, f_prev))
      a0 <- a1; f_prev <- fi; a1 <- a1 * ctrl$ls_expand
    }
    list(ok = FALSE, alpha = a1, x.new = x + a1*p, f.new = phi(a1), g.new = gphi(a1), why = "ls_limit")
  }
  
  # ---------- Initialization ----------
  x <- as.numeric(start); n <- length(x); f <- safe_obj(x); g <- safe_grad(x)
  Hinv <- diag(ctrl$Hinv_init_diag, n)
  it <- 0L; x_old <- NULL; f_old <- NA_real_; converged <- FALSE
  status <- NULL; hist <- if (isTRUE(ctrl$keep_history)) list() else NULL
  Hess_checked <- FALSE; Hess_is_pd_cache <- NA; Hess_min_eig_cache <- NA_real_
  
  # ---------- Main Loop ----------
  tinfo <- system.time({
    repeat {
      it <- it + 1L
      if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      
      # Calculate gradient norm once for this iteration
      g_inf <- max_abs(g)
      
      # Convergence Check
      if (check_convergence(g_inf, g, Hinv, f_old, f, x_old, x, n)) {
        status <- "converged"; converged <- TRUE; break
      }
      
      p <- as.numeric(-Hinv %*% g)
      ls <- wolfe_ls(x, f, g, p)
      
      if (!ls$ok) {
        # Check if current state satisfies convergence before failing
        ls_g_inf <- max_abs(ls$g.new)
        if (check_convergence(ls_g_inf, ls$g.new, Hinv, f_old, ls$f.new, x_old, ls$x.new, n)) {
          status <- "converged"; converged <- TRUE; x <- ls$x.new; f <- ls$f.new; g <- ls$g.new
        } else status <- paste0("line_search_failed:", ls$why)
        break
      }
      
      x_old <- x; f_old <- f; x <- ls$x.new; f <- ls$f.new; g_new <- ls$g.new
      s <- x - x_old; y <- g_new - g; g <- g_new; Hess_checked <- FALSE
      
      ys <- as.numeric(crossprod(y, s))
      if (ys > ctrl$curvature_eps) {
        Hy <- Hinv %*% y; yHy <- as.numeric(crossprod(y, Hy))
        Hinv <- Hinv + ((1 + yHy / ys) * (s %*% t(s)) / ys) - ((s %*% t(Hy) + Hy %*% t(s)) / ys)
      } else if (isTRUE(ctrl$reset_on_curvature)) Hinv <- diag(ctrl$Hinv_init_diag, n)
      
      if (isTRUE(ctrl$keep_history)) hist[[length(hist) + 1L]] <- list(iter = it, f = f, max_grad = max_abs(g))
    }
  })
  
  # ---------- Output ----------
  out <- list(
    par          = x,
    objective    = f,
    converged    = converged,
    status       = status,
    iter         = it,         
    time         = as.numeric(tinfo["user.self"] + tinfo["sys.self"]),
    max_grad     = g_inf,      
    Hess_is_pd   = Hess_is_pd_cache,
    Hess_min_eig = Hess_min_eig_cache
  )
  if (isTRUE(ctrl$keep_history)) out$history <- hist
  return(out)
}