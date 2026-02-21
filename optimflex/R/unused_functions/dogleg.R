dogleg <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,
    lower          = -Inf,
    upper          = Inf,
    # Convergence selection flags (Strict AND logic; same as bfgs)
    use_abs_f      = FALSE, # |f_new - f_old| < tol_abs_f
    use_rel_f      = FALSE, # |(f_new - f_old) / f_old| < tol_rel_f
    use_abs_x      = FALSE, # max|x_new - x_old| < tol_abs_x
    use_rel_x      = TRUE,  # max|(x_new - x_old) / x_old| < tol_rel_x
    use_grad       = TRUE,  # max|projected gradient| < tol_grad
    use_posdef     = TRUE,  # Verify exact Hessian PD after initial convergence
    use_pred_f     = FALSE, # 0.5 * g^T * B^{-1} * g < tol_pred_f   (free vars)
    use_pred_f_avg = FALSE, # (0.5 * g^T * B^{-1} * g) / nfree < tol_pred_f_avg
    control        = list()
) {
  
  # ---------- Default Controls (aligned with bfgs + trust-region extras) ----------
  ctrl0 <- list(
    max_iter        = 10000L,
    tol_abs_f       = 1e-6,
    tol_rel_f       = 1e-6,
    tol_abs_x       = 1e-6,
    tol_rel_x       = 1e-6,
    tol_grad        = 1e-4,
    tol_pred_f      = 1e-4,
    tol_pred_f_avg  = 1e-4,
    
    # Trust-region specifics
    initial_delta   = 1.0,
    delta_max       = 100,
    rho_accept      = 0.1,
    rho_expand      = 0.75,
    delta_shrink    = 0.25,
    delta_expand    = 2.0,
    
    # BFGS on Hessian approximation B
    reset_on_curvature = TRUE,
    curvature_eps   = 1e-12,
    Hinv_init_diag  = 1.0,    # used to set B0 = diag(1 / Hinv_init_diag)
    
    eps             = 1e-12,
    verbose         = FALSE
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
  
  safe_obj <- function(x) {
    fx <- tryCatch(objective(x), error = function(e) NA_real_)
    if (!is.finite(fx)) Inf else as.numeric(fx)
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
  
  is_pd_mat <- function(M) {
    if (is.null(M)) return(list(is_pd = FALSE, min_eig = NA_real_))
    Ms <- 0.5 * (M + t(M))
    ev <- tryCatch(eigen(Ms, symmetric = TRUE, only.values = TRUE)$values,
                   error = function(e) NA_real_)
    min_eig <- suppressWarnings(min(ev))
    list(is_pd = is.finite(min_eig) && (min_eig > 0), min_eig = min_eig)
  }
  
  projected_info <- function(x, g, lower, upper) {
    is_free <- !((x <= lower + 1e-10 & g > 0) | (x >= upper - 1e-10 & g < 0))
    free_idx <- which(is_free)
    pg_inf <- if (length(free_idx) > 0L) max(abs(g[free_idx])) else 0
    list(free_idx = free_idx, pg_inf = pg_inf)
  }
  
  # ---------- Fast Convergence Checker (excludes PD check; AND logic) ----------
  check_convergence_fast <- function(
    g_inf, g_f, B_f, f_old, f_new, x_old, x_new, nfree
  ) {
    res <- TRUE
    
    if (use_grad) res <- res && (g_inf <= ctrl$tol_grad)
    
    if (use_abs_f && !is.na(f_old)) res <- res && (abs(f_new - f_old) <= ctrl$tol_abs_f)
    if (use_rel_f && !is.na(f_old)) res <- res && (abs(rel(f_new - f_old, f_old)) <= ctrl$tol_rel_f)
    
    if (use_abs_x && !is.null(x_old)) res <- res && (max_abs(x_new - x_old) <= ctrl$tol_abs_x)
    if (use_rel_x && !is.null(x_old)) {
      rel_x <- max(abs(x_new - x_old) / pmax(1, abs(x_old)))
      res <- res && (rel_x <= ctrl$tol_rel_x)
    }
    
    # Predicted decrease proxy: 0.5 * g_f^T * B_f^{-1} * g_f
    if (use_pred_f || use_pred_f_avg) {
      pred_dec <- Inf
      if (nfree > 0L) {
        v <- tryCatch(solve(B_f, g_f), error = function(e) NULL)
        if (!is.null(v) && all(is.finite(v))) {
          pred_dec <- as.numeric(0.5 * crossprod(g_f, v))
        }
      } else {
        pred_dec <- 0
      }
      
      if (use_pred_f) res <- res && (pred_dec <= ctrl$tol_pred_f)
      if (use_pred_f_avg) res <- res && ((pred_dec / max(1L, nfree)) <= ctrl$tol_pred_f_avg)
    }
    
    res
  }
  
  # ---------- Dogleg step (standard) on free vars ----------
  dogleg_step <- function(g, B, delta, eps = 1e-12) {
    ng <- length(g)
    if (ng == 0L) return(rep(0, 0))
    
    gnorm <- sqrt(sum(g * g))
    if (!is.finite(gnorm) || gnorm <= eps) return(rep(0, ng))
    
    # Newton step
    pN <- tryCatch(solve(B, -g), error = function(e) NULL)
    if (is.null(pN) || any(!is.finite(pN))) {
      return(-(delta / max(eps, gnorm)) * g)
    }
    
    norm_pN <- sqrt(sum(pN^2))
    if (!is.finite(norm_pN) || norm_pN <= eps) {
      return(-(delta / max(eps, gnorm)) * g)
    }
    
    if (norm_pN <= delta) return(pN)
    
    # Cauchy point
    gBg <- as.numeric(crossprod(g, B %*% g))
    if (!is.finite(gBg) || gBg <= eps) {
      return(-(delta / max(eps, gnorm)) * g)
    }
    
    alpha <- sum(g^2) / gBg
    pC <- -alpha * g
    norm_pC <- sqrt(sum(pC^2))
    
    if (!is.finite(norm_pC) || norm_pC <= eps) {
      return(-(delta / max(eps, gnorm)) * g)
    }
    
    if (norm_pC >= delta) {
      return((delta / norm_pC) * pC)
    }
    
    # Interpolate between pC and pN to hit boundary
    d <- pN - pC
    a <- sum(d^2)
    b <- 2 * sum(pC * d)
    c <- sum(pC^2) - delta^2
    disc <- max(0, b^2 - 4 * a * c)
    tau <- (-b + sqrt(disc)) / (2 * a)
    pC + tau * d
  }
  
  # ---------- Initialization ----------
  x <- pmax(lower, pmin(as.numeric(start), upper))
  n <- length(x)
  
  f <- safe_obj(x)
  g <- safe_grad(x)
  
  B <- diag(1 / ctrl$Hinv_init_diag, n)
  delta <- ctrl$initial_delta
  
  it <- 0L
  converged <- FALSE
  status <- NULL
  
  x_old <- NULL
  f_old <- NA_real_
  
  g_inf <- NA_real_
  Hess_is_pd_cache <- NA
  Hess_min_eig_cache <- NA_real_
  
  # ---------- Main Loop ----------
  tinfo <- system.time({
    repeat {
      it <- it + 1L
      if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      
      # Projected gradient info
      pi <- projected_info(x, g, lower, upper)
      free_idx <- pi$free_idx
      g_inf <- pi$pg_inf
      
      g_f <- g[free_idx]
      B_f <- B[free_idx, free_idx, drop = FALSE]
      nfree <- length(free_idx)
      
      # Fast convergence check (AND; PD excluded)
      if (check_convergence_fast(g_inf, g_f, B_f, f_old, f, x_old, x, nfree)) {
        converged <- TRUE
        status <- "converged"
        break
      }
      
      # Trust-region dogleg step on free vars
      p_f <- tryCatch(
        dogleg_step(g_f, B_f, delta, eps = ctrl$eps),
        error = function(e) {
          ng <- sqrt(sum(g_f * g_f))
          if (!is.finite(ng) || ng < ctrl$eps) rep(0, length(g_f))
          else -(delta / ng) * g_f
        }
      )
      
      # Apply step + projection
      p_full <- rep(0, n)
      p_full[free_idx] <- p_f
      x_try <- pmax(lower, pmin(x + p_full, upper))
      
      s <- x_try - x
      f_try <- safe_obj(x_try)
      
      actual_red <- f - f_try
      pred_red <- as.numeric(-(t(g) %*% s + 0.5 * t(s) %*% B %*% s))
      rho <- if (is.finite(pred_red) && pred_red > 0) actual_red / pred_red else 0
      
      if (is.finite(rho) && rho > ctrl$rho_accept && actual_red > 0) {
        # Accept
        x_old <- x
        f_old <- f
        
        g_old <- g
        x <- x_try
        f <- f_try
        g_new <- safe_grad(x)
        
        # BFGS update for B
        s_acc <- x - x_old
        y <- g_new - g_old
        sy <- sum(s_acc * y)
        
        if (is.finite(sy) && sy > ctrl$curvature_eps) {
          Bs <- B %*% s_acc
          sBs <- as.numeric(t(s_acc) %*% Bs)
          
          if (is.finite(sBs) && sBs > ctrl$curvature_eps) {
            B <- B - (Bs %*% t(Bs)) / sBs + (y %*% t(y)) / sy
            B <- 0.5 * (B + t(B))
          } else if (isTRUE(ctrl$reset_on_curvature)) {
            B <- diag(1 / ctrl$Hinv_init_diag, n)
          }
        } else if (isTRUE(ctrl$reset_on_curvature)) {
          B <- diag(1 / ctrl$Hinv_init_diag, n)
        }
        
        g <- g_new
        
        # TR radius update
        if (rho > ctrl$rho_expand) {
          delta <- min(ctrl$delta_max, ctrl$delta_expand * delta)
        }
        
      } else {
        # Reject
        delta <- ctrl$delta_shrink * delta
        if (delta < 1e-14) {
          status <- "trust_region_radius_too_small"
          break
        }
      }
    }
    
    # ---------- Post-convergence exact Hessian PD check ----------
    if (converged && use_posdef) {
      H_final <- safe_hess(x)
      hpd <- is_pd_mat(H_final)
      Hess_is_pd_cache <- hpd$is_pd
      Hess_min_eig_cache <- hpd$min_eig
      
      if (!isTRUE(Hess_is_pd_cache)) {
        status <- "converged_but_not_positive_definite"
        converged <- FALSE
      }
    }
  })
  
  # ---------- Return (aligned with bfgs output) ----------
  list(
    par          = x,
    objective    = f,
    converged    = converged,
    status       = status,
    iter         = it,
    time         = as.numeric(tinfo["user.self"] + tinfo["sys.self"]),
    max_grad     = g_inf,
    Hess_is_pd   = Hess_is_pd_cache,
    hessian      = H_final 
  )
}
