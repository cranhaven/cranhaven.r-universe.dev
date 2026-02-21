#' @title Limited-memory BFGS with Box Constraints (L-BFGS-B)
#'
#' @description
#' Performs bound-constrained minimization using the L-BFGS-B algorithm. 
#' This implementation handles box constraints via Generalized Cauchy Point (GCP) 
#' estimation and subspace minimization, featuring a limited-memory (two-loop recursion) 
#' inverse Hessian approximation.
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
#' Byrd, R. H., Lu, P., Nocedal, J., & Zhu, C. (1995). A limited memory algorithm 
#' for bound constrained optimization. \emph{SIAM Journal on Scientific Computing}, 
#' 16(5), 1190-1208.
#'
#' Morales, J. L., & Nocedal, J. (2011). L-BFGS-B: Remark on algorithm 778: 
#' L-BFGS-B: Fortran subroutines for large-scale bound-constrained optimization. 
#' \emph{ACM Transactions on Mathematical Software}, 38(1), 1-4.
#'
#' @param start Numeric vector. Initial values for the parameters.
#' @param objective Function. The scalar objective function to be minimized.
#' @param gradient Function (optional). Returns the gradient vector. If \code{NULL}, 
#' numerical derivatives are used.
#' @param hessian Function (optional). Returns the Hessian matrix. Used for final 
#' positive definiteness verification if \code{use_posdef = TRUE}.
#' @param lower,upper Numeric vectors. Lower and upper bounds for the parameters. 
#' Can be scalars if all parameters share the same bounds.
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
#'   \item \code{m}: Number of L-BFGS memory updates (default: 5).
#'   \item \code{tol_abs_f}, \code{tol_rel_f}: Tolerances for function value change.
#'   \item \code{tol_abs_x}, \code{tol_rel_x}: Tolerances for parameter change.
#'   \item \code{tol_grad}: Tolerance for the projected gradient (default: 1e-4).
#'   \item \code{tol_pred_f}, \code{tol_pred_f_avg}: Tolerances for predicted decrease.
#'   \item \code{wolfe_c1}: Parameter for Armijo condition (default: 1e-4).
#'   \item \code{ls_alpha0}: Initial step length for line search (default: 1.0).
#'   \item \code{ls_shrink}: Shrinkage factor for line search (default: 0.5).
#'   \item \code{ls_max_steps}: Maximum line search steps (default: 50).
#'   \item \code{curvature_eps}: Small constant for curvature (\eqn{s^T y}) check.
#'   \item \code{bound_eps}: Tolerance for determining if a parameter is at its bound.
#' }
#'
#' @return A list containing:
#' \item{par}{Optimized parameter vector (projected into feasible region).}
#' \item{objective}{Final objective function value.}
#' \item{converged}{Logical; TRUE if all selected criteria were met (AND rule).}
#' \item{status}{Termination status message (e.g., "converged", "stalled_at_bounds").}
#' \item{iter}{Number of iterations performed.}
#' \item{time}{Execution time in seconds.}
#' \item{max_grad}{Maximum absolute element of the final \bold{projected gradient}.}
#' \item{Hess_is_pd}{Logical; TRUE if the computed Hessian was positive definite.}
#' \item{Hess_min_eig}{Minimum eigenvalue of the computed Hessian.}
#' \item{hessian}{The Hessian matrix at the solution, if computed.}
#' \item{history_par, history_obj}{Optimization history if \code{keep_history = TRUE}.}
#'
#' @examples
#' # 1. Define a simple quadratic function
#' quad_func <- function(x) {
#'   (x[1] - 3)^2 + (x[2] - 2)^2
#' }
#' 
#' # 2. Run optimization with box constraints
#' # Target minimum is at (3, 2), but we constrain x[1] to be <= 2
#' res <- l_bfgs_b(
#'   start = c(0, 0),
#'   objective = quad_func,
#'   lower = c(-Inf, -Inf),
#'   upper = c(2, Inf),
#'   use_rel_x = TRUE,
#'   use_grad = TRUE
#' )
#' 
#' # 3. Result will be at the boundary x[1] = 2
#' print(res$par) 
#' print(res$status)
#' 
#' @export
l_bfgs_b <- function(
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
    use_grad       = TRUE,   # max|projected_g| < tol_grad
    use_posdef     = TRUE,   # final exact Hessian PD check
    use_pred_f     = FALSE,  # projection-consistent proxy
    use_pred_f_avg = FALSE,
    control        = list()
) {
  
  # ---------- Default Controls ----------
  ctrl0 <- list(
    max_iter        = 10000L,
    m               = 5L,
    
    tol_abs_f       = 1e-6,
    tol_rel_f       = 1e-6,
    tol_abs_x       = 1e-6,
    tol_rel_x       = 1e-6,
    tol_grad        = 1e-4,
    tol_pred_f      = 1e-4,
    tol_pred_f_avg  = 1e-4,
    
    # Projected Armijo line search
    wolfe_c1        = 1e-4,
    ls_alpha0       = 1.0,
    ls_max_steps    = 50L,
    ls_shrink       = 0.5,
    
    # Safeguards
    curvature_eps   = 1e-10,  # for sy
    gamma_eps       = 1e-12,  # for scaling
    bound_eps       = 1e-10,  # bound activity tolerance
    eps             = 1e-12,
    
    # Numerical derivatives
    use_num_grad_if_missing = TRUE,
    use_num_hess_if_missing = TRUE,
    
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
  
  expand_bounds <- function(b, n) {
    if (length(b) == 1L) return(rep(as.numeric(b), n))
    if (length(b) != n) stop("lower/upper must have length 1 or length(start).")
    as.numeric(b)
  }
  
  project <- function(x, l, u) pmax(l, pmin(x, u))
  
  at_lower <- function(x, l) is.finite(l) & (x <= l + ctrl$bound_eps)
  at_upper <- function(x, u) is.finite(u) & (x >= u - ctrl$bound_eps)
  
  safe_obj <- function(x) {
    fx <- tryCatch(objective(x), error = function(e) NA_real_)
    if (!is.finite(fx)) Inf else as.numeric(fx)
  }
  
  safe_grad <- function(x) {
    if (!is.null(gradient)) {
      g <- tryCatch(gradient(x), error = function(e) NULL)
      if (!is.null(g) && all(is.finite(g))) return(as.numeric(g))
    }
    if (!isTRUE(ctrl$use_num_grad_if_missing)) stop("Analytic gradient not provided.")
    numDeriv::grad(objective, x)
  }
  
  safe_hess <- function(x) {
    if (!is.null(hessian)) {
      H <- tryCatch(hessian(x), error = function(e) NULL)
      if (is.matrix(H)) return(0.5 * (H + t(H)))
    }
    if (!isTRUE(ctrl$use_num_hess_if_missing)) return(NULL)
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
  
  # Projected gradient (KKT-like stationarity surrogate)
  get_projected_grad <- function(x, g, l, u) {
    pg <- as.numeric(g)
    al <- at_lower(x, l)
    au <- at_upper(x, u)
    pg[al & g > 0] <- 0
    pg[au & g < 0] <- 0
    pg
  }
  
  # Feasibility enforcement for directions at bounds
  enforce_feasible_direction <- function(x, p, l, u) {
    p2 <- as.numeric(p)
    al <- at_lower(x, l)
    au <- at_upper(x, u)
    p2[al & p2 < 0] <- 0
    p2[au & p2 > 0] <- 0
    p2
  }
  
  # Indices not fixed by bounds (purely geometric: not at bounds)
  free_by_bounds <- function(x, l, u) {
    which(!(at_lower(x, l) | at_upper(x, u)))
  }
  
  # L-BFGS two-loop: returns H * v with H0 = (1/theta) I
  two_loop <- function(v, s_list, y_list, rho_list, theta) {
    mcur <- length(s_list)
    if (mcur == 0L) return((1 / max(ctrl$eps, theta)) * v)
    
    q <- as.numeric(v)
    alpha <- numeric(mcur)
    for (i in rev(seq_len(mcur))) {
      alpha[i] <- rho_list[[i]] * sum(s_list[[i]] * q)
      q <- q - alpha[i] * y_list[[i]]
    }
    
    r <- (1 / max(ctrl$eps, theta)) * q
    
    for (i in seq_len(mcur)) {
      beta <- rho_list[[i]] * sum(y_list[[i]] * r)
      r <- r + s_list[[i]] * (alpha[i] - beta)
    }
    r
  }
  
  # Scalar theta for B0 ~ theta I (used in GCP model and H0 = (1/theta) I)
  compute_theta <- function(s_list, y_list) {
    mcur <- length(s_list)
    if (mcur == 0L) return(1.0)
    s <- s_list[[mcur]]
    y <- y_list[[mcur]]
    sy <- sum(s * y)
    yy <- sum(y * y)
    if (!is.finite(sy) || !is.finite(yy) || sy <= ctrl$gamma_eps || yy <= ctrl$gamma_eps) return(1.0)
    # Common scaling for B0: theta = (y'y)/(s'y)
    as.numeric(yy / sy)
  }
  
  # Generalized Cauchy point (approx):
  # - Follow projected steepest descent direction d = -pg
  # - Use quadratic model with B0 = theta I (scalar) along piecewise path with breakpoints
  generalized_cauchy_point <- function(x, g, l, u, theta) {
    pg <- get_projected_grad(x, g, l, u)
    d <- -pg
    d <- enforce_feasible_direction(x, d, l, u)
    
    if (max_abs(d) == 0 || !is.finite(max_abs(d))) {
      return(list(xc = x, d = d))
    }
    
    # Breakpoints where x + t d hits bounds
    t <- rep(Inf, length(x))
    idx_pos <- which(d > 0)
    idx_neg <- which(d < 0)
    if (length(idx_pos) > 0L) t[idx_pos] <- (u[idx_pos] - x[idx_pos]) / d[idx_pos]
    if (length(idx_neg) > 0L) t[idx_neg] <- (l[idx_neg] - x[idx_neg]) / d[idx_neg]
    
    # Keep only positive breakpoints
    t[!is.finite(t) | t <= 0] <- Inf
    
    ord <- order(t)
    t_sorted <- t[ord]
    
    # Active set along the path: start with all indices with finite direction
    active <- which(d != 0)
    gtd <- sum(pg[active] * d[active])  # note pg and d opposite sign -> gtd <= 0 typically
    dd  <- sum(d[active] * d[active])
    
    if (!is.finite(gtd) || !is.finite(dd) || dd <= ctrl$eps) {
      return(list(xc = x, d = d))
    }
    
    # Minimize m(t) = g^T (t d) + 0.5 * theta * ||t d||^2 on each segment
    # derivative: m'(t) = gtd + theta * t * dd
    t_prev <- 0.0
    
    for (k in seq_along(t_sorted)) {
      tk <- t_sorted[k]
      if (!is.finite(tk)) break
      
      # Candidate minimizer in this segment
      t_star <- -gtd / (max(ctrl$eps, theta) * dd)
      
      if (t_star >= t_prev && t_star <= tk) {
        xc <- project(x + t_star * d, l, u)
        return(list(xc = xc, d = d))
      }
      
      # Otherwise move to breakpoint tk and freeze that coordinate
      xc_k <- project(x + tk * d, l, u)
      
      # Identify which variable hits bound at tk (may be multiple)
      hit <- which(abs(t - tk) <= 1e-14)
      if (length(hit) > 0L) {
        # Remove hit indices from active set
        active <- setdiff(active, hit)
      }
      
      # Update gtd and dd for remaining active variables
      if (length(active) == 0L) {
        return(list(xc = xc_k, d = d))
      }
      
      gtd <- sum(pg[active] * d[active])
      dd  <- sum(d[active] * d[active])
      t_prev <- tk
      
      if (!is.finite(gtd) || !is.finite(dd) || dd <= ctrl$eps) {
        return(list(xc = xc_k, d = d))
      }
    }
    
    # If never minimized before all breakpoints, take the last feasible point
    xc <- project(x + t_prev * d, l, u)
    list(xc = xc, d = d)
  }
  
  # Projection-consistent predicted decrease proxy (for optional criteria)
  pred_dec_proxy <- function(pg_old, s_last, free_idx_old) {
    if (is.null(pg_old) || is.null(s_last) || length(free_idx_old) == 0L) return(NA_real_)
    val <- -sum(pg_old[free_idx_old] * s_last[free_idx_old])
    if (!is.finite(val)) NA_real_ else max(0, as.numeric(val))
  }
  
  # Fast convergence check (AND; PD excluded)
  check_convergence_fast <- function(pg_inf, f_old, f_new, x_old, x_new,
                                     pg_old, s_last, free_idx_old, nfree_old) {
    res <- TRUE
    if (use_grad) res <- res && (pg_inf <= ctrl$tol_grad)
    
    if (use_abs_f && !is.na(f_old)) res <- res && (abs(f_new - f_old) <= ctrl$tol_abs_f)
    if (use_rel_f && !is.na(f_old)) res <- res && (abs((f_new - f_old) / max(1, abs(f_old))) <= ctrl$tol_rel_f)
    
    if (use_abs_x && !is.null(x_old)) res <- res && (max_abs(x_new - x_old) <= ctrl$tol_abs_x)
    if (use_rel_x && !is.null(x_old)) {
      rel_x <- max(abs(x_new - x_old) / pmax(1, abs(x_old)))
      res <- res && (rel_x <= ctrl$tol_rel_x)
    }
    
    if (use_pred_f || use_pred_f_avg) {
      pd <- pred_dec_proxy(pg_old, s_last, free_idx_old)
      if (use_pred_f) res <- res && (!is.na(pd) && pd <= ctrl$tol_pred_f)
      if (use_pred_f_avg) res <- res && (!is.na(pd) && (pd / max(1L, nfree_old)) <= ctrl$tol_pred_f_avg)
    }
    
    res
  }
  
  # ---------- Initialization ----------
  x <- as.numeric(start)
  n <- length(x)
  
  lower <- expand_bounds(lower, n)
  upper <- expand_bounds(upper, n)
  
  x <- project(x, lower, upper)
  
  f <- safe_obj(x)
  g <- safe_grad(x)
  
  # L-BFGS memory (full-length vectors, zeros allowed)
  s_list <- list()
  y_list <- list()
  rho_list <- list()
  
  it <- 0L
  converged <- FALSE
  status <- NULL
  
  x_old <- NULL
  f_old <- NA_real_
  g_old <- NULL
  
  # For predicted decrease proxy
  pg_old <- NULL
  free_idx_old <- integer(0)
  nfree_old <- 0L
  s_last <- NULL
  
  # PD check outputs
  H_final <- NULL
  Hess_is_pd_cache <- NA
  Hess_min_eig_cache <- NA_real_
  
  # Optional history
  hist_par <- if (isTRUE(ctrl$keep_history)) list() else NULL
  hist_obj <- if (isTRUE(ctrl$keep_history)) numeric(0) else NULL
  
  tinfo <- system.time({
    repeat {
      it <- it + 1L
      if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      
      # Current projected gradient and stationarity measure
      pg <- get_projected_grad(x, g, lower, upper)
      pg_inf <- max_abs(pg)
      
      # Convergence check (start of iteration)
      if (check_convergence_fast(
        pg_inf = pg_inf,
        f_old = f_old, f_new = f,
        x_old = x_old, x_new = x,
        pg_old = pg_old, s_last = s_last,
        free_idx_old = free_idx_old, nfree_old = nfree_old
      )) {
        status <- "converged"
        converged <- TRUE
        break
      }
      
      # Compute theta scaling from memory
      theta <- compute_theta(s_list, y_list)
      
      # 1) Generalized Cauchy point (approx)
      gcp <- generalized_cauchy_point(x, g, lower, upper, theta)
      x_c <- gcp$xc
      
      # Determine free set at Cauchy point (geometric)
      free_c <- free_by_bounds(x_c, lower, upper)
      
      # 2) Subspace step using L-BFGS inverse on gradient at x (L-BFGS-B style)
      # Build p_sub = - H * g, then restrict to free_c
      Hg <- two_loop(g, s_list, y_list, rho_list, theta = theta)
      p_sub <- -Hg
      p_sub[setdiff(seq_len(n), free_c)] <- 0
      p_sub <- enforce_feasible_direction(x_c, p_sub, lower, upper)
      
      # Candidate point: x_trial = P[x_c + p_sub]
      x_trial <- project(x_c + p_sub, lower, upper)
      
      # Direction from current x
      d <- x_trial - x
      
      # Fallback if direction collapses
      if (max_abs(d) == 0 || !is.finite(max_abs(d))) {
        d <- -pg
        d <- enforce_feasible_direction(x, d, lower, upper)
        x_trial <- project(x + d, lower, upper)
        d <- x_trial - x
        if (max_abs(d) == 0 || !is.finite(max_abs(d))) {
          status <- "stalled_at_bounds"
          break
        }
      }
      
      # 3) Projection-aware Armijo line search along d (uses actual step s)
      alpha <- ctrl$ls_alpha0
      step_ok <- FALSE
      x_new <- x
      f_new <- f
      s_new <- rep(0, n)
      
      for (ls_it in seq_len(ctrl$ls_max_steps)) {
        x_try <- project(x + alpha * d, lower, upper)
        s_try <- x_try - x
        
        if (max_abs(s_try) == 0) {
          alpha <- alpha * ctrl$ls_shrink
          next
        }
        
        f_try <- safe_obj(x_try)
        armijo_rhs <- f + ctrl$wolfe_c1 * sum(g * s_try)
        
        if (is.finite(f_try) && f_try <= armijo_rhs) {
          step_ok <- TRUE
          x_new <- x_try
          f_new <- f_try
          s_new <- s_try
          break
        }
        
        alpha <- alpha * ctrl$ls_shrink
      }
      
      if (!step_ok) {
        # Hard fallback: projected steepest descent
        d2 <- -pg
        d2 <- enforce_feasible_direction(x, d2, lower, upper)
        alpha <- ctrl$ls_alpha0
        step_ok2 <- FALSE
        
        for (ls_it in seq_len(ctrl$ls_max_steps)) {
          x_try <- project(x + alpha * d2, lower, upper)
          s_try <- x_try - x
          if (max_abs(s_try) == 0) { alpha <- alpha * ctrl$ls_shrink; next }
          f_try <- safe_obj(x_try)
          armijo_rhs <- f + ctrl$wolfe_c1 * sum(g * s_try)
          if (is.finite(f_try) && f_try <= armijo_rhs) {
            step_ok2 <- TRUE
            x_new <- x_try
            f_new <- f_try
            s_new <- s_try
            break
          }
          alpha <- alpha * ctrl$ls_shrink
        }
        
        if (!step_ok2) { status <- "line_search_failed"; break }
      }
      
      # Accept
      x_old <- x
      f_old <- f
      g_old <- g
      
      pg_old <- pg
      free_idx_old <- free_by_bounds(x_old, lower, upper)
      nfree_old <- length(free_idx_old)
      s_last <- s_new
      
      x <- x_new
      f <- f_new
      g <- safe_grad(x)
      
      if (isTRUE(ctrl$keep_history)) {
        hist_par[[length(hist_par) + 1L]] <- x
        hist_obj <- c(hist_obj, f)
      }
      
      # 4) L-BFGS memory update, restricted to indices free at both ends (geometric)
      free_new <- free_by_bounds(x, lower, upper)
      free_both <- intersect(free_idx_old, free_new)
      
      s <- x - x_old
      y <- g - g_old
      
      s_mod <- rep(0, n)
      y_mod <- rep(0, n)
      if (length(free_both) > 0L) {
        s_mod[free_both] <- s[free_both]
        y_mod[free_both] <- y[free_both]
      }
      
      sy <- sum(s_mod * y_mod)
      
      if (is.finite(sy) && sy > ctrl$curvature_eps) {
        if (length(s_list) >= ctrl$m) {
          s_list[[1]] <- NULL
          y_list[[1]] <- NULL
          rho_list[[1]] <- NULL
        }
        s_list[[length(s_list) + 1L]] <- s_mod
        y_list[[length(y_list) + 1L]] <- y_mod
        rho_list[[length(rho_list) + 1L]] <- 1 / sy
      }
    }
    
    # Final exact Hessian PD check
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
  
  out <- list(
    par            = x,
    objective      = f,
    converged      = converged,
    status         = status,
    iter           = it,
    time           = as.numeric(tinfo["user.self"] + tinfo["sys.self"]),
    max_grad       = max_abs(get_projected_grad(x, g, lower, upper)),
    Hess_is_pd     = Hess_is_pd_cache,
    Hess_min_eig   = Hess_min_eig_cache,
    hessian        = H_final
  )
  
  if (isTRUE(ctrl$keep_history)) {
    out$history_par <- hist_par
    out$history_obj <- hist_obj
  }
  
  out
}
