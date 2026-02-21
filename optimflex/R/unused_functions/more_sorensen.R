# ============================================================
# more_sorensen: Trust-Region Optimization (Moré-Sorensen Solver)
#   - Solves the subproblem exactly via Cholesky-based iteration
#   - Optimized by performing Hessian PD verification only at the final step
#   - Convergence criteria: All selected flags must be met (Strict AND logic)
#   - Output format matched with 'bfgs' and 'gauss_newton'
# ============================================================
more_sorensen <- function(
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
    use_posdef     = TRUE,  # Verification at the final step only
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
    
    # Trust-Region specific settings
    rinit           = 1.0,    # Initial radius
    rmax            = 100.0,  # Maximum radius
    rmin            = 1e-12,  # Minimum radius
    eta             = 0.15,   # Step acceptance threshold
    
    verbose         = FALSE, 
    keep_history    = FALSE  
  )
  ctrl <- utils::modifyList(ctrl0, control) 
  
  # ---------- Helpers ----------
  max_abs <- function(x) { 
    x <- as.numeric(x)
    if (length(x) == 0L) return(NA_real_)
    max(abs(x), na.rm = TRUE) 
  }
  
  rel_fn <- function(num, den) { 
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
    H <- tryCatch(numDeriv::hessian(objective, x, ...), error = function(e) NULL)
    if (is.matrix(H)) 0.5 * (H + t(H)) else NULL 
  }
  
  # ---------- Moré-Sorensen Subproblem Solver ----------
  solve_ms <- function(g, H, delta) {
    n <- length(g)
    # 1. Attempt Newton step (lambda = 0)
    res <- tryCatch({
      L <- chol(H)
      p <- backsolve(L, backsolve(L, -g, transpose = TRUE))
      list(p = p, lambda = 0, ok = TRUE)
    }, error = function(e) list(ok = FALSE))
    
    if (res$ok && sqrt(sum(res$p^2)) <= delta) return(res)
    
    # 2. Iteratively find lambda > 0 such that ||p(lambda)|| = delta
    lambda <- max(0, -min(eigen(H, only.values = TRUE)$values) + 1e-4)
    for (i in 1:20) {
      M <- H + diag(lambda, n)
      L <- tryCatch(chol(M), error = function(e) NULL)
      if (is.null(L)) { lambda <- lambda * 2; next }
      
      p <- backsolve(L, backsolve(L, -g, transpose = TRUE))
      p_norm <- sqrt(sum(p^2))
      if (abs(p_norm - delta) < 1e-6 * delta) break
      
      q <- backsolve(L, p, transpose = TRUE)
      q_norm_sq <- sum(q^2)
      lambda <- lambda + (p_norm / delta)^2 * ((p_norm - delta) / q_norm_sq)
    }
    list(p = p, lambda = lambda, ok = TRUE)
  }
  
  # ---------- Fast Convergence Checker (Excludes PD check) ----------
  check_convergence_fast <- function(g_inf, g, H, p_step, f_old, f_new, x_old, x_new, n) {
    res <- TRUE 
    if (use_grad) res <- res && (g_inf <= ctrl$tol_grad) 
    if (use_abs_f && !is.na(f_old)) res <- res && (abs(f_new - f_old) <= ctrl$tol_abs_f)
    if (use_rel_f && !is.na(f_old)) res <- res && (abs(rel_fn(f_new - f_old, f_old)) <= ctrl$tol_rel_f)
    if (use_abs_x && !is.null(x_old)) res <- res && (max_abs(x_new - x_old) <= ctrl$tol_abs_x)
    if (use_rel_x && !is.null(x_old)) { 
      rel_x <- max(abs(x_new - x_old) / pmax(1, abs(x_old)))
      res <- res && (rel_x <= ctrl$tol_rel_x)
    }
    if (use_pred_f || use_pred_f_avg) {
      pred_dec <- as.numeric(-(t(g) %*% p_step + 0.5 * t(p_step) %*% H %*% p_step))
      if (use_pred_f) res <- res && (pred_dec <= ctrl$tol_pred_f)
      if (use_pred_f_avg) res <- res && ((pred_dec / n) <= ctrl$tol_pred_f_avg)
    }
    return(res)
  }
  
  # ---------- Initialization ----------
  x <- as.numeric(start); n <- length(x)
  f <- tryCatch(objective(x, ...), error = function(e) NA_real_)
  g <- safe_grad(x)
  r <- ctrl$rinit 
  it <- 0L; x_old <- NULL; f_old <- NA_real_; converged <- FALSE; status <- NULL
  Hess_is_pd_cache <- NA; Hess_min_eig_cache <- NA_real_
  p_step <- rep(0, n)
  
  # ---------- Main Optimization Loop ----------
  tinfo <- system.time({
    repeat {
      it <- it + 1L; if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break } 
      g_inf <- max_abs(g) 
      H_curr <- safe_hess(x) 
      
      # Step 1: Fast convergence check within loop
      if (check_convergence_fast(g_inf, g, H_curr, p_step, f_old, f, x_old, x, n)) { 
        status <- "converged"; converged <- TRUE; break
      }
      
      # Solve Subproblem via Moré-Sorensen
      ms_res <- solve_ms(g, H_curr, r)
      if (!ms_res$ok) { status <- "subproblem_solver_failed"; break }
      p_step <- ms_res$p
      
      # Evaluate Step Acceptance
      x_try <- x + p_step
      f_try <- tryCatch(objective(x_try, ...), error = function(e) NA_real_)
      pred_dec <- as.numeric(-(t(g) %*% p_step + 0.5 * t(p_step) %*% H_curr %*% p_step))
      act_dec  <- f - f_try
      rho <- if (pred_dec > 0) act_dec / pred_dec else -Inf
      
      if (rho >= ctrl$eta && f_try < f) { # Accept Step
        x_old <- x; f_old <- f
        x <- x_try; f <- f_try; g <- safe_grad(x)
        if (rho > 0.75) r <- min(ctrl$rmax, r * 2.0)
      } else { # Reject Step and shrink radius
        r <- max(ctrl$rmin, r * 0.5)
        if (r < ctrl$rmin) { status <- "radius_too_small"; break }
      }
    }
    
    # ---------- Final Hessian Verification (Post-Loop) ----------
    if (converged && use_posdef) {
      H_final <- safe_hess(x)
      hpd <- is_pd_mat(H_final)
      Hess_is_pd_cache <- hpd$is_pd
      Hess_min_eig_cache <- hpd$min_eig
      
      # If Hessian is not PD, it may be a saddle point or non-optimal [cite: 131, 298]
      if (!isTRUE(Hess_is_pd_cache)) {
        status <- "converged_but_not_positive_definite"
        converged <- FALSE # Violation of strict AND logic
      }
    }
  })
  
  # ---------- Output Construction ----------
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