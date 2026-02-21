# ============================================================
# gauss_newton: Gauss-Newton / Fisher Scoring Optimization
#   - Approximates Hessian using the Information Matrix or JT*J
#   - Performance optimized by checking Hessian PD only once at the final step
#   - Strict 'AND' logic for convergence criteria
# ============================================================
gauss_newton <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,  # Usually the Information Matrix or JT*J approximation
    # Convergence selection flags (AND logic)
    use_abs_f      = FALSE, 
    use_rel_f      = FALSE, 
    use_abs_x      = FALSE, 
    use_rel_x      = TRUE,  
    use_grad       = TRUE,  
    use_posdef     = TRUE,  # Strict verification at the final step
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
    wolfe_c1        = 1e-4,
    wolfe_c2        = 0.9,
    ls_alpha0       = 1.0,
    ls_expand       = 2.0,
    ls_max_steps    = 50L,
    verbose         = FALSE,
    keep_history    = FALSE,
    regularization  = 1e-8
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
    H <- tryCatch(numDeriv::hessian(objective, x, ...), error = function(e) NULL)
    if (is.matrix(H)) 0.5 * (H + t(H)) else NULL
  }
  
  # ---------- Fast Convergence Checker (Excludes PD check) ----------
  check_convergence_fast <- function(g_inf, g, p_step, f_old, f_new, x_old, x_new, n) {
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
      if (all(is.finite(p_step))) {
        pred_dec <- as.numeric(0.5 * t(g) %*% (-p_step))
        if (use_pred_f) res <- res && (pred_dec <= ctrl$tol_pred_f)
        if (use_pred_f_avg) res <- res && ((pred_dec / n) <= ctrl$tol_pred_f_avg)
      } else res <- FALSE
    }
    return(res)
  }
  
  # ---------- Line Search (Strong Wolfe) ----------
  wolfe_ls <- function(x, f0, g0, p) {
    phi <- function(a) safe_obj(x + a * p)
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
          if (dj * (ahi - alo) >= 0) { ahi <- alo; fhi <- flo }; alo <- aj; flo <- fj
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
      if (dpi >= 0) return(zoom(a1, fi, a0, f_prev)); a0 <- a1; f_prev <- fi; a1 <- a1 * ctrl$ls_expand
    }
    list(ok = FALSE, alpha = a1, x.new = x + a1*p, f.new = phi(a1), g.new = gphi(a1), why = "ls_limit")
  }
  
  # ---------- Initialization ----------
  x <- as.numeric(start); n <- length(x); f <- safe_obj(x); g <- safe_grad(x)
  it <- 0L; x_old <- NULL; f_old <- NA_real_; converged <- FALSE; status <- NULL
  Hess_is_pd_cache <- NA; Hess_min_eig_cache <- NA_real_; p <- rep(NA_real_, n)
  g_inf <- max_abs(g)
  H_final <- NULL
  # ---------- Main Optimization Loop ----------
  tinfo <- system.time({
    repeat {
      it <- it + 1L; if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      g_inf <- max_abs(g)
      
      # Fast convergence check within loop
      if (check_convergence_fast(g_inf, g, p, f_old, f, x_old, x, n)) { 
        status <- "converged"; converged <- TRUE; break 
      }
      
      # Gauss-Newton step using approximated Hessian (H)
      H <- safe_hess(x)
      if (is.null(H)) { status <- "hessian_failed"; break }
      p <- tryCatch(solve(H + diag(ctrl$regularization, n), -g), error = function(e) -g)
      
      ls <- wolfe_ls(x, f, g, p)
      if (!ls$ok) {
        if (check_convergence_fast(max_abs(ls$g.new), ls$g.new, p, f_old, ls$f.new, x_old, ls$x.new, n)) { 
          status <- "converged"; converged <- TRUE; x <- ls$x.new; f <- ls$f.new; g <- ls$g.new 
        } else { status <- paste0("line_search_failed:", ls$why) }; break
      }
      x_old <- x; f_old <- f; x <- ls$x.new; f <- ls$f.new; g <- ls$g.new
    }
    
    # ---------- Final Hessian Verification (Post-Loop) ----------
    if (converged && use_posdef) {
      H_final <- safe_hess(x)
      hpd <- is_pd_mat(H_final)
      Hess_is_pd_cache <- hpd$is_pd
      Hess_min_eig_cache <- hpd$min_eig
      
      # If Hessian is not PD, consider it as a non-optimal critical point
      if (!isTRUE(Hess_is_pd_cache)) {
        status <- "converged_but_not_positive_definite"
        converged <- FALSE # Strict AND logic
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