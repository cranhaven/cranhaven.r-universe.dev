#' @title Double Dogleg Optimization with Box Constraints
#'
#' @description
#' Performs bound-constrained minimization using a trust-region strategy with 
#' a double dogleg step. The algorithm maintains a BFGS approximation of the 
#' Hessian matrix and ensures steps remain within a trust region while 
#' respecting parameter bounds.
#'
#' @section Comparison with Existing Functions:
#' This function adds three features for rigorous convergence control. First, it 
#' applies an \bold{AND rule}: all selected convergence criteria must be satisfied 
#' simultaneously. Second, users can choose among \bold{eight distinct criteria} 
#' (e.g., changes in $f$, $x$, projected gradient, or predicted decrease) instead 
#' of relying on fixed defaults. Third, it provides an \bold{optional verification} 
#' using the Hessian computed from derivatives (analytically when provided, or 
#' via numerical differentiation). Checking the positive definiteness of this 
#' Hessian at the final solution reduces the risk of declaring convergence at 
#' non-minimizing stationary points, such as saddle points.
#'
#' @references
#' Dennis, J. E., & Mei, H. H. (1979). Two new unconstrained optimization 
#' algorithms which use function and gradient values. \emph{Journal of 
#' Optimization Theory and Applications}, 28(4), 453-482.
#'
#' Powell, M. J. D. (1970). A hybrid method for non-linear equations. 
#' \emph{Numerical Methods for Nonlinear Algebraic Equations}, 87-114.
#'
#' Nocedal, J., & Wright, S. J. (2006). \emph{Numerical Optimization}. Springer.
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
#'   \item \code{initial_delta}: Initial trust-region radius (default: 1.0).
#'   \item \code{delta_max}: Maximum trust-region radius (default: 100).
#'   \item \code{dd_bias}: Bias parameter for the double dogleg step (default: 0.8).
#'   \item \code{rho_accept}: Threshold for step acceptance (default: 0.1).
#'   \item \code{rho_expand}: Threshold for trust-region expansion (default: 0.75).
#'   \item \code{delta_shrink}, \code{delta_expand}: Radius adjustment factors.
#'   \item \code{tol_abs_f}, \code{tol_rel_f}: Tolerances for function-based criteria.
#'   \item \code{tol_abs_x}, \code{tol_rel_x}: Tolerances for parameter-based criteria.
#'   \item \code{tol_grad}: Tolerance for the projected gradient (default: 1e-4).
#'   \item \code{reset_on_curvature}: Reset Hessian to Identity if curvature is poor.
#'   \item \code{bound_eps}: Tolerance for determining if a parameter is at its bound.
#' }
#'
#' @return A list containing:
#' \item{par}{Optimized parameter vector.}
#' \item{objective}{Final objective function value.}
#' \item{converged}{Logical; TRUE if all selected criteria were met (AND rule).}
#' \item{status}{Termination status message.}
#' \item{iter}{Number of iterations performed.}
#' \item{time}{Execution time in seconds.}
#' \item{max_grad}{Maximum absolute element of the final projected gradient.}
#' \item{Hess_is_pd}{Logical; TRUE if the computed Hessian was positive definite.}
#' \item{Hess_min_eig}{Minimum eigenvalue of the computed Hessian.}
#' \item{hessian}{The Hessian matrix at the solution, if computed.}
#' \item{trust_radius}{Final trust-region radius (\code{delta}).}
#'
#' @examples
#' # 1. Define a simple quadratic function
#' quad_func <- function(x) {
#'   (x[1] - 3)^2 + (x[2] - 2)^2
#' }
#' 
#' # 2. Run optimization with box constraints
#' # Target minimum is at (3, 2), but we constrain x[1] to be <= 2
#' res <- double_dogleg(
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
double_dogleg <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,
    lower          = -Inf,
    upper          = Inf,
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
  
  # --- 1. Default Controls ---
  ctrl0 <- list(
    max_iter        = 10000L,
    tol_abs_f       = 1e-6,
    tol_rel_f       = 1e-6,
    tol_abs_x       = 1e-6,
    tol_rel_x       = 1e-6,
    tol_grad        = 1e-4,
    tol_pred_f      = 1e-4,
    tol_pred_f_avg  = 1e-4,
    
    # Trust-region / double dogleg
    initial_delta   = 1.0,
    delta_max       = 100,
    dd_bias         = 0.8,
    rho_accept      = 0.1,
    rho_expand      = 0.75,
    delta_shrink    = 0.25,
    delta_expand    = 2.0,
    
    # BFGS on Hessian approximation B
    reset_on_curvature = TRUE,
    curvature_eps   = 1e-12,
    Hinv_init_diag  = 1.0,
    
    # Bound/robustness epsilons
    bound_eps       = 1e-10,
    eps             = 1e-12,
    verbose         = FALSE
  )
  ctrl <- utils::modifyList(ctrl0, control)
  
  # --- 2. Helpers ---
  expand_bounds <- function(b, n) {
    b <- as.numeric(b)
    if (length(b) == 1L) return(rep(b, n))
    if (length(b) != n) stop("lower/upper must have length 1 or length(start).")
    b
  }
  
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
    H <- if (!is.null(hessian)) {
      tryCatch(hessian(x), error = function(e) NULL)
    } else {
      tryCatch(numDeriv::hessian(objective, x), error = function(e) NULL)
    }
    if (is.matrix(H)) return(0.5 * (H + t(H)))
    NULL
  }
  
  is_pd <- function(M) {
    if (is.null(M)) return(list(ok = FALSE, min_eig = NA_real_))
    Ms <- 0.5 * (M + t(M))
    ev <- tryCatch(eigen(Ms, symmetric = TRUE, only.values = TRUE)$values,
                   error = function(e) NA_real_)
    m_eig <- suppressWarnings(min(ev))
    list(ok = is.finite(m_eig) && (m_eig > 0), min_eig = m_eig)
  }
  
  # Active-set definition for bounds (projected-gradient sense)
  projected_info <- function(x, g, lower, upper) {
    # Fixed if at bound and gradient points outward (cannot reduce f by moving outward)
    is_fixed <- (x <= lower + ctrl$bound_eps & g > 0) |
      (x >= upper - ctrl$bound_eps & g < 0)
    free_idx <- which(!is_fixed)
    pg_inf <- if (length(free_idx) > 0L) max(abs(g[free_idx])) else 0
    list(is_fixed = is_fixed, free_idx = free_idx, pg_inf = pg_inf)
  }
  
  # Scale step so that x + tau*p stays within bounds, with a single tau in (0,1]
  step_to_bounds_scale <- function(x, p, lower, upper) {
    tau <- 1.0
    idx_pos <- which(p > 0)
    idx_neg <- which(p < 0)
    
    if (length(idx_pos) > 0L) {
      tau_pos <- (upper[idx_pos] - x[idx_pos]) / p[idx_pos]
      tau <- min(tau, suppressWarnings(min(tau_pos, na.rm = TRUE)))
    }
    if (length(idx_neg) > 0L) {
      tau_neg <- (lower[idx_neg] - x[idx_neg]) / p[idx_neg]  # p<0 => ratio positive
      tau <- min(tau, suppressWarnings(min(tau_neg, na.rm = TRUE)))
    }
    
    if (!is.finite(tau)) tau <- 1.0
    tau <- max(0.0, min(1.0, tau))
    tau
  }
  
  # --- 3. Dogleg step on free subspace ---
  compute_step <- function(g, B, delta, bias, eps) {
    ng <- length(g)
    gnorm <- sqrt(sum(g * g))
    if (!is.finite(gnorm) || gnorm <= eps) return(list(p = rep(0, ng), type = "zero"))
    
    # Newton step: B p = -g
    nwtstp <- tryCatch(solve(B, -g), error = function(e) NULL)
    if (is.null(nwtstp) || any(!is.finite(nwtstp))) {
      return(list(p = -(delta / gnorm) * g, type = "gradient_fallback"))
    }
    
    nwtnrm <- sqrt(sum(nwtstp * nwtstp))
    if (is.finite(nwtnrm) && nwtnrm <= delta) return(list(p = nwtstp, type = "full_newton"))
    
    # Cauchy step along -g
    Bg <- tryCatch(B %*% g, error = function(e) NULL)
    gBg <- if (!is.null(Bg)) as.numeric(crossprod(g, Bg)) else NA_real_
    
    if (!is.finite(gBg) || gBg <= eps) {
      # If curvature is bad, fall back to scaled gradient
      return(list(p = -(delta / gnorm) * g, type = "cauchy_fallback"))
    }
    
    alpha <- (gnorm^2) / gBg
    cp <- -alpha * g
    cnorm <- sqrt(sum(cp * cp))
    
    if (!is.finite(cnorm) || cnorm >= delta) {
      return(list(p = -(delta / gnorm) * g, type = "cauchy_truncated"))
    }
    
    # Relaxed Newton
    ghinvg <- as.numeric(crossprod(g, -nwtstp))
    if (!is.finite(ghinvg) || ghinvg <= eps) {
      return(list(p = -(delta / gnorm) * g, type = "relax_fallback"))
    }
    
    relax <- 1.0 - bias * (1.0 - (gnorm * cnorm) / ghinvg)
    relax <- max(0, min(1, relax))
    np_relaxed <- relax * nwtstp
    
    # Interpolate on dogleg path: cp -> np_relaxed
    d <- np_relaxed - cp
    a_sq <- sum(d^2)
    b_sq <- 2 * sum(cp * d)
    c_sq <- sum(cp^2) - delta^2
    
    disc <- max(0, b_sq^2 - 4 * a_sq * c_sq)
    if (!is.finite(a_sq) || a_sq <= eps || !is.finite(disc)) {
      return(list(p = -(delta / gnorm) * g, type = "dogleg_fallback"))
    }
    
    tau <- (-b_sq + sqrt(disc)) / (2 * a_sq)
    tau <- max(0, min(1, tau))
    list(p = cp + tau * d, type = "dogleg_interpolated")
  }
  
  # --- 4. Convergence checker (strict AND across selected criteria) ---
  check_convergence_fast <- function(
    pg_inf, g_f, B_f, f_old, f_new, x_old, x_new, nfree
  ) {
    ok <- TRUE
    
    if (use_grad) ok <- ok && (pg_inf <= ctrl$tol_grad)
    
    if (use_abs_f && !is.na(f_old)) ok <- ok && (abs(f_new - f_old) <= ctrl$tol_abs_f)
    if (use_rel_f && !is.na(f_old)) ok <- ok && (abs(rel(f_new - f_old, f_old)) <= ctrl$tol_rel_f)
    
    if (use_abs_x && !is.null(x_old)) ok <- ok && (max_abs(x_new - x_old) <= ctrl$tol_abs_x)
    if (use_rel_x && !is.null(x_old)) {
      rel_x <- max(abs(x_new - x_old) / pmax(1, abs(x_old)))
      ok <- ok && (rel_x <= ctrl$tol_rel_x)
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
      
      if (use_pred_f) ok <- ok && (pred_dec <= ctrl$tol_pred_f)
      if (use_pred_f_avg) ok <- ok && ((pred_dec / max(1L, nfree)) <= ctrl$tol_pred_f_avg)
    }
    
    ok
  }
  
  # --- 5. Initialization ---
  x <- as.numeric(start)
  n <- length(x)
  lower <- expand_bounds(lower, n)
  upper <- expand_bounds(upper, n)
  
  x <- pmax(lower, pmin(x, upper))
  f <- safe_obj(x)
  g <- safe_grad(x)
  
  B <- diag(1 / ctrl$Hinv_init_diag, n)  # Hessian approximation
  delta <- ctrl$initial_delta
  
  it <- 0L
  converged <- FALSE
  status <- "running"
  x_old <- NULL
  f_old <- NA_real_
  H_final <- NULL
  pd_status <- NA
  pd_min_eig <- NA_real_
  
  # --- 6. Optimization Loop ---
  tinfo <- system.time({
    repeat {
      it <- it + 1L
      if (it > ctrl$max_iter) { status <- "iteration_limit_reached"; break }
      
      # Active set / projected gradient info
      pi <- projected_info(x, g, lower, upper)
      free_idx <- pi$free_idx
      nfree <- length(free_idx)
      pg_inf <- pi$pg_inf
      
      if (nfree == 0L) { status <- "all_variables_fixed"; break }
      
      g_f <- g[free_idx]
      B_f <- B[free_idx, free_idx, drop = FALSE]
      
      # Convergence (AND across selected criteria)
      if (check_convergence_fast(pg_inf, g_f, B_f, f_old, f, x_old, x, nfree)) {
        converged <- TRUE
        status <- "converged"
        break
      }
      
      # Subspace dogleg step (free vars only)
      st <- compute_step(g_f, B_f, delta, ctrl$dd_bias, ctrl$eps)
      
      p_vec <- rep(0, n)
      p_vec[free_idx] <- st$p
      
      # Enforce bounds via single scaling to nearest bound (no coordinate clipping)
      tau_b <- step_to_bounds_scale(x, p_vec, lower, upper)
      x_try <- x + tau_b * p_vec
      
      # If step is too small, shrink delta and continue
      s <- x_try - x
      if (max_abs(s) < 1e-15) {
        delta <- ctrl$delta_shrink * delta
        if (delta < 1e-14) { status <- "step_size_too_small"; break }
        next
      }
      
      f_try <- safe_obj(x_try)
      actual_red <- f - f_try
      
      # Predicted reduction on free subspace model
      s_f <- s[free_idx]
      pred_red <- as.numeric(-(crossprod(g_f, s_f) + 0.5 * crossprod(s_f, B_f %*% s_f)))
      
      rho <- if (is.finite(pred_red) && pred_red > 0) actual_red / pred_red else 0
      
      if (is.finite(rho) && rho > ctrl$rho_accept && actual_red > 0) {
        # Accept
        x_old <- x
        f_old <- f
        g_old <- g
        
        x <- x_try
        f <- f_try
        g <- safe_grad(x)
        
        # Recompute active sets for update restriction
        pi_new <- projected_info(x, g, lower, upper)
        free_new <- pi_new$free_idx
        free_both <- intersect(free_idx, free_new)
        
        # BFGS update restricted to free_both
        y <- g - g_old
        s_mod <- rep(0, n)
        y_mod <- rep(0, n)
        if (length(free_both) > 0L) {
          s_mod[free_both] <- s[free_both]
          y_mod[free_both] <- y[free_both]
        }
        
        sy <- sum(s_mod * y_mod)
        
        if (is.finite(sy) && sy > ctrl$curvature_eps) {
          Bs <- B %*% s_mod
          sBs <- as.numeric(crossprod(s_mod, Bs))
          if (is.finite(sBs) && sBs > ctrl$curvature_eps) {
            B <- B - (Bs %*% t(Bs)) / sBs + (y_mod %*% t(y_mod)) / sy
            B <- 0.5 * (B + t(B))
          } else if (isTRUE(ctrl$reset_on_curvature)) {
            B <- diag(1 / ctrl$Hinv_init_diag, n)
          }
        } else if (isTRUE(ctrl$reset_on_curvature)) {
          B <- diag(1 / ctrl$Hinv_init_diag, n)
        }
        
        # Trust region radius update
        if (rho > ctrl$rho_expand) {
          delta <- min(ctrl$delta_max, ctrl$delta_expand * delta)
        }
      } else {
        # Reject
        delta <- ctrl$delta_shrink * delta
        if (delta < 1e-14) { status <- "trust_region_radius_too_small"; break }
      }
    }
    
    # Post-convergence PD check (exact Hessian)
    if (converged && use_posdef) {
      H_final <- safe_hess(x)
      pd_res <- is_pd(H_final)
      pd_status <- pd_res$ok
      pd_min_eig <- pd_res$min_eig
      if (!isTRUE(pd_status)) {
        status <- "converged_but_not_positive_definite"
        converged <- FALSE
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
    max_grad     = projected_info(x, g, lower, upper)$pg_inf,
    Hess_is_pd   = pd_status,
    hessian      = H_final,
    Hess_min_eig = pd_min_eig,
    trust_radius = delta
  )
}
