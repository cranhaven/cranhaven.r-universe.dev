# ============================================================
# levenberg_marquardt: Pure LM Optimization
#   - Blends Newton-type and Gradient Descent via damping (lambda)
#   - Performance optimized by checking Hessian PD only once at the final step
#   - Convergence criteria: All selected flags must be met (Strict AND logic)
#   - Consistent with bfgs and gauss_newton input/output
# ============================================================
levenberg_marquardt <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,
    # Convergence selection flags (Strict AND logic)
    use_abs_f      = FALSE, 
    use_rel_f      = FALSE, 
    use_abs_x      = FALSE, 
    use_rel_x      = TRUE,  
    use_grad       = TRUE,  
    use_posdef     = TRUE,  # Final verification for second-order condition
    use_pred_f     = FALSE, 
    use_pred_f_avg = FALSE, 
    control        = list(),
    ...
) {
  
  # ---------- Default Controls ----------
  ctrl0 <- list(
    max_iter        = 10000L,
    tol_abs_f       = 1e-6,
    tol_rel_f       = 1e-6,
    tol_abs_x       = 1e-6,
    tol_rel_x       = 1e-6,
    tol_grad        = 1e-4,
    tol_pred_f      = 1e-4,
    tol_pred_f_avg  = 1e-4,
    lambda_init     = 1e-3,   # Initial damping factor
    lambda_factor   = 10.0,   # Scaling factor for lambda
    use_num_grad_if_missing = TRUE,
    use_num_hess_if_missing = TRUE,
    keep_history    = FALSE
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
    fx <- tryCatch(objective(x, ...), error = function(e) NA_real_)
    if (!is.finite(fx)) Inf else as.numeric(fx) 
  }
  
  safe_grad <- function(x) {
    if (!is.null(gradient)) { 
      g <- tryCatch(gradient(x, ...), error = function(e) NULL)
      if (!is.null(g) && all(is.finite(g))) return(as.numeric(g)) 
    }
    numDeriv::grad(objective, x, ...)
  }
  
  safe_hess <- function(x) {
    if (!is.null(hessian)) { 
      H <- tryCatch(hessian(x, ...), error = function(e) NULL)
      if (is.matrix(H)) return(0.5 * (H + t(H))) 
    }
    if (!ctrl$use_num_hess_if_missing || !requireNamespace("numDeriv", quietly = TRUE)) return(NULL)
    H <- tryCatch(numDeriv::hessian(objective, x, ...), error = function(e) NULL)
    if (is.matrix(H)) 0.5 * (H + t(H)) else NULL
  }
  
  # ---------- Fast Convergence Checker (Excludes PD check) ----------
  check_convergence_fast <- function(g_inf, g, p_step, f_old, f_new, x_old, x_new, n) {
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
      } else res <- FALSE
    }
    return(res)
  }
  
  # ---------- Initialization ----------
  x <- as.numeric(start); n <- length(x)
  f <- safe_obj(x); g <- safe_grad(x)
  lambda <- ctrl$lambda_init
  it <- 0L; x_old <- NULL; f_old <- NA_real_; converged <- FALSE; status <- NULL
  Hess_is_pd_cache <- NA; Hess_min_eig_cache <- NA_real_
  p_step <- rep(0, n)
  
  # ---------- Main Optimization Loop ----------
  tinfo <- system.time({
    repeat {
      it <- it + 1L; if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      g_inf <- max_abs(g)
      
      # Fast check inside the loop to avoid O(n^3) eigenvalue calculations
      if (check_convergence_fast(g_inf, g, p_step, f_old, f, x_old, x, n)) {
        status <- "converged"; converged <- TRUE; break
      }
      
      H <- safe_hess(x)
      if (is.null(H)) { status <- "hessian_failed"; break }
      
      # LM Step: (H + lambda * I) p = -g
      p_step <- tryCatch(solve(H + diag(lambda, n), -g), error = function(e) -g)
      
      # Trial step and Gain Ratio evaluation
      x_try <- x + p_step
      f_try <- safe_obj(x_try)
      actual_red <- f - f_try
      pred_red <- as.numeric(-(t(g) %*% p_step + 0.5 * t(p_step) %*% H %*% p_step))
      
      if (actual_red > 0 && pred_red > 0) { # Successful step
        x_old <- x; f_old <- f
        x <- x_try; f <- f_try; g <- safe_grad(x)
        lambda <- max(1e-12, lambda / ctrl$lambda_factor) # Towards Newton
      } else { # Unsuccessful step
        lambda <- min(1e12, lambda * ctrl$lambda_factor) # Towards Gradient Descent
        if (lambda > 1e10) { status <- "divergence_warning_lambda_max"; break }
      }
    }
    
    # ---------- Final Hessian Verification (Post-Loop) ----------
    if (converged && use_posdef) {
      H_final <- safe_hess(x)
      hpd <- is_pd_mat(H_final)
      Hess_is_pd_cache <- hpd$is_pd
      Hess_min_eig_cache <- hpd$min_eig
      
      if (!isTRUE(Hess_is_pd_cache)) {
        status <- "converged_but_not_positive_definite"
        converged <- FALSE # Strict AND logic requirement
      }
    }
  })
  
  # ---------- Output ----------
  return(list(
    par          = x,
    objective    = f,
    converged    = converged,
    status       = status,
    iter         = it,
    time         = as.numeric(tinfo["user.self"] + tinfo["sys.self"]),
    max_grad     = g_inf,
    Hess_is_pd   = Hess_is_pd_cache,
    hessian      = H_final 
  ))
}