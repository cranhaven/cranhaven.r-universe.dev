# ============================================================
# port: Trust-Region Quasi-Newton (PORT-style)
#   - Trust-region step selection with box constraints
#   - BFGS Hessian update (approximate Hessian B)
#   - Performance optimized: Hessian PD check performed only at the final step
#   - Convergence criteria: All selected flags must be met (Strict AND logic)
# ============================================================
port <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,
    lower          = -Inf,
    upper          = Inf,
    # Convergence selection flags (Strict AND logic)
    use_abs_f      = FALSE, 
    use_rel_f      = FALSE, 
    use_abs_x      = FALSE, 
    use_rel_x      = TRUE,  
    use_grad       = TRUE,  
    use_posdef     = TRUE,  # Verification at the final step
    use_pred_f     = FALSE, 
    use_pred_f_avg = FALSE, 
    control        = list()
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
    initial_delta   = 1.0,    # Initial trust-region radius
    max_delta       = 100.0,  # Maximum trust-region radius
    eta             = 0.15,   # Step acceptance threshold
    Hinv_init_diag  = 1.0,
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
  
  project <- function(x, l, u) pmax(l, pmin(x, u))
  
  get_projected_grad <- function(x, g, l, u) {
    pg <- g
    pg[x <= l & g > 0] <- 0
    pg[x >= u & g < 0] <- 0
    return(pg)
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
      g <- tryCatch(gradient(x), error = function(e) NULL)
      if (!is.null(g) && all(is.finite(g))) return(as.numeric(g))
    }
    numDeriv::grad(objective, x)
  }
  
  safe_hess <- function(x) {
    if (!is.null(hessian)) {
      H <- tryCatch(hessian(x), error = function(e) NULL)
      if (is.matrix(H)) return(0.5 * (H + t(H)))
    }
    H <- tryCatch(numDeriv::hessian(objective, x), error = function(e) NULL)
    if (is.matrix(H)) 0.5 * (H + t(H)) else NULL
  }
  
  # ---------- Fast Convergence Checker (Excludes PD check) ----------
  check_convergence_fast <- function(pg_inf, g, p_step, f_old, f_new, x_old, x_new, n, B) {
    res <- TRUE
    if (use_grad) res <- res && (pg_inf <= ctrl$tol_grad)
    if (use_abs_f && !is.na(f_old)) res <- res && (abs(f_new - f_old) <= ctrl$tol_abs_f)
    if (use_rel_f && !is.na(f_old)) {
      denom <- max(1, abs(f_old))
      res <- res && (abs(f_new - f_old) / denom <= ctrl$tol_rel_f)
    }
    if (use_abs_x && !is.null(x_old)) res <- res && (max_abs(x_new - x_old) <= ctrl$tol_abs_x)
    if (use_rel_x && !is.null(x_old)) {
      rel_x <- max(abs(x_new - x_old) / pmax(1, abs(x_old)))
      res <- res && (rel_x <= ctrl$tol_rel_x)
    }
    # Predicted decrease for TR: -(g^T * p + 0.5 * p^T * B * p)
    if (use_pred_f || use_pred_f_avg) {
      pred_dec <- as.numeric(-(t(g) %*% p_step + 0.5 * t(p_step) %*% B %*% p_step))
      if (use_pred_f) res <- res && (pred_dec <= ctrl$tol_pred_f)
      if (use_pred_f_avg) res <- res && ((pred_dec / n) <= ctrl$tol_pred_f_avg)
    }
    return(res)
  }
  
  # ---------- Initialization ----------
  x <- project(as.numeric(start), lower, upper); n <- length(x)
  f <- objective(x); g <- safe_grad(x)
  B <- diag(1/ctrl$Hinv_init_diag, n) # Approximate Hessian (B)
  delta <- ctrl$initial_delta
  it <- 0L; x_old <- NULL; f_old <- NA_real_; converged <- FALSE; status <- NULL
  Hess_is_pd_cache <- NA; Hess_min_eig_cache <- NA_real_
  p_step <- rep(0, n)
  
  # ---------- Main Optimization Loop ----------
  tinfo <- system.time({
    repeat {
      it <- it + 1L; if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      
      pg <- get_projected_grad(x, g, lower, upper); pg_inf <- max_abs(pg)
      
      # Fast convergence check within loop
      if (check_convergence_fast(pg_inf, g, p_step, f_old, f, x_old, x, n, B)) {
        status <- "converged"; converged <- TRUE; break
      }
      
      # 1. Solve Trust-Region Subproblem (Cauchy point approximation)
      gBg <- as.numeric(t(g) %*% B %*% g)
      tau <- if (gBg <= 0) 1 else min(1, (max_abs(g)^3) / (delta * gBg))
      p_step <- project(-tau * (delta / max(1e-10, max_abs(g))) * g, lower - x, upper - x)
      
      # 2. Evaluate Trial Step
      x_try <- project(x + p_step, lower, upper)
      f_try <- objective(x_try)
      actual_red <- f - f_try
      pred_red <- as.numeric(-(t(g) %*% p_step + 0.5 * t(p_step) %*% B %*% p_step))
      rho <- if (pred_red > 0) actual_red / pred_red else 0
      
      # 3. Update Radius and Hessian [PORT-style logic]
      if (rho > ctrl$eta && f_try < f) { # Accept step
        x_old <- x; f_old <- f; g_old <- g
        x <- x_try; f <- f_try; g <- safe_grad(x)
        if (rho > 0.75) delta <- min(ctrl$max_delta, 2 * delta)
        
        # Quasi-Newton BFGS Update for B (Hessian approximation)
        s <- x - x_old; y <- g - g_old; sy <- sum(s * y)
        if (sy > 1e-10) {
          Bs <- B %*% s
          B <- B - (Bs %*% t(Bs)) / as.numeric(t(s) %*% Bs) + (y %*% t(y)) / sy
        }
      } else { # Reject step and shrink trust region
        delta <- 0.25 * delta
        if (delta < 1e-16) { status <- "trust_region_radius_too_small"; break }
      }
    }
    
    # ---------- Final Hessian Verification (Post-Loop) ----------
    if (converged && use_posdef) {
      H_final <- safe_hess(x) # Calculate actual Hessian for SE/Verification
      hpd <- is_pd_mat(H_final)
      Hess_is_pd_cache <- hpd$is_pd
      Hess_min_eig_cache <- hpd$min_eig
      
      if (!isTRUE(Hess_is_pd_cache)) {
        status <- "converged_but_not_positive_definite"
        converged <- FALSE # Strict AND logic violation
      }
    }
  })
  
  # ---------- Return Output ----------
  list(
    par          = x,
    objective    = f,
    converged    = converged,
    status       = status,
    iter         = it,
    time         = as.numeric(tinfo["user.self"] + tinfo["sys.self"]),
    max_grad     = pg_inf,
    Hess_is_pd   = Hess_is_pd_cache,
    Hess_min_eig = Hess_min_eig_cache
  )
}