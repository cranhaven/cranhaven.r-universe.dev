#' Dogleg Trust-Region Optimization
#'
#' @description
#' Implements the standard Powell's Dogleg Trust-Region algorithm for non-linear optimization.
#'
#' @details
#' This function implements the classic Dogleg method within a Trust-Region framework, 
#' based on the strategy proposed by Powell (1970).
#' 
#' \bold{Trust-Region vs. Line Search:}
#' Trust-Region methods define a neighborhood around the current point (the trust region 
#' with radius \eqn{\Delta}) where a local quadratic model is assumed to be reliable. 
#' Unlike Line Search methods that first determine a search direction and then 
#' find an appropriate step length, this approach constrains the step size first 
#' and then finds the optimal update within that boundary.
#'
#' \bold{Powell's Dogleg Trajectory:}
#' The "Dogleg" trajectory is a piecewise linear path connecting:
#' \enumerate{
#'    \item The current point.
#'    \item The \bold{Cauchy Point} (\eqn{p_C}): The minimizer of the quadratic model along 
#'          the steepest descent direction.
#'    \item The \bold{Newton Point} (\eqn{p_N}): The unconstrained minimizer of the quadratic model \eqn{(B^{-1}g)}.
#' }
#' The algorithm selects a step along this path such that it minimizes the quadratic 
#' model while remaining within the radius \eqn{\Delta}.
#'
#' \bold{Relationship to Double Dogleg:}
#' While the \code{double_dogleg} algorithm (Dennis and Mei, 1979) introduces a bias 
#' point to follow the Newton direction more closely, this standard Dogleg follows 
#' the original two-segment trajectory.
#'
#' @references
#' \itemize{
#'    \item Powell, M. J. D. (1970). A Hybrid Method for Nonlinear Equations. 
#'          \emph{Numerical Methods for Nonlinear Algebraic Equations}.
#'    \item Nocedal, J., & Wright, S. J. (2006). \emph{Numerical Optimization}. Springer.
#' }
#'
#' @param start Numeric vector. Starting values for the optimization parameters.
#' @param objective Function. The objective function to minimize.
#' @param gradient Function (optional). Gradient of the objective function.
#' @param hessian Function (optional). Hessian matrix of the objective function.
#' @param lower Numeric vector. Lower bounds for box constraints.
#' @param upper Numeric vector. Upper bounds for box constraints.
#' @param control List. Control parameters including convergence flags:
#'    \itemize{
#'      \item \code{use_abs_f}: Logical. Use absolute change in objective for convergence.
#'      \item \code{use_rel_f}: Logical. Use relative change in objective for convergence.
#'      \item \code{use_abs_x}: Logical. Use absolute change in parameters for convergence.
#'      \item \code{use_rel_x}: Logical. Use relative change in parameters for convergence.
#'      \item \code{use_grad}: Logical. Use gradient norm for convergence.
#'      \item \code{use_posdef}: Logical. Verify positive definiteness at convergence.
#'      \item \code{use_pred_f}: Logical. Record predicted objective decrease.
#'      \item \code{use_pred_f_avg}: Logical. Record average predicted decrease.
#'    }
#' @param ... Additional arguments passed to objective, gradient, and Hessian functions.
#'
#' @return A list containing optimization results and iteration metadata.
#' @export
dogleg <- function(
    start, 
    objective, 
    gradient = NULL, 
    hessian  = NULL, 
    lower    = -Inf, 
    upper    = Inf,
    control  = list(), 
    ...
) {
  # ---------- 1. Configuration ----------
  ctrl0 <- list(
    use_abs_f       = FALSE, 
    use_rel_f       = FALSE, 
    use_abs_x       = FALSE, 
    use_rel_x       = TRUE,
    use_grad        = TRUE, 
    use_posdef      = TRUE,
    use_pred_f      = FALSE,
    use_pred_f_avg  = FALSE,
    
    max_iter        = 10000L, 
    tol_abs_f       = 1e-6, 
    tol_rel_f       = 1e-6, 
    tol_abs_x       = 1e-6, 
    tol_rel_x       = 1e-6,
    tol_grad        = 1e-4, 
    tol_pred_f      = 1e-4,
    tol_pred_f_avg  = 1e-4,
    initial_delta   = 1.0, 
    delta_max       = 100.0, 
    rho_accept      = 0.1, 
    rho_expand      = 0.75,
    delta_shrink    = 0.25, 
    delta_expand    = 2.0, 
    H_init_diag     = 1.0, 
    diff_method      = "forward",
    hessian_update   = "bfgs",
    use_damped      = TRUE, 
    damp_phi        = 0.2
  )
  ctrl <- utils::modifyList(ctrl0, control)
  
  # ---------- 2. Internal Helpers ----------
  eval_obj <- function(z) as.numeric(objective(z, ...))[1]
  
  grad_func <- if (!is.null(gradient)) {
    function(z) as.numeric(gradient(z, ...))
  } else {
    function(z) fast_grad(objective, z, diff_method = ctrl$diff_method, ...)
  }
  
  hess_func <- if (!is.null(hessian)) {
    function(z) hessian(z, ...)
  } else {
    function(z) fast_hess(objective, z, diff_method = ctrl$diff_method, ...)
  }
  
  project <- function(z, l, u) pmax(l, pmin(z, u))
  
  # ---------- 3. Initialization ----------
  x <- project(as.numeric(start), lower, upper)
  n <- length(x)
  
  # start_clock defined here for CPU time calculation
  start_clock <- proc.time() 
  
  f <- tryCatch(eval_obj(x), error = function(e) NA_real_)
  it <- 0L; converged <- FALSE; status <- "running"
  x_old <- x; f_old <- NA_real_; delta <- ctrl$initial_delta
  
  # Initialize Hessian approximation (B)
  B <- diag(ctrl$H_init_diag, n)
  H_eval <- NULL; g_inf <- NA_real_
  pred_dec <- NA_real_; pred_dec_avg <- NA_real_
  
  # ---------- 4. Main Loop ----------
  if (!is.finite(f)) {
    status <- "objective_error_at_start"
  } else {
    g <- grad_func(x)
    
    tryCatch({
      repeat {
        if (it >= ctrl$max_iter) { status <- "iteration_limit_reached"; break }
        it <- it + 1L
        
        # 4.1) Free Variables Identification (for Box Constraints)
        is_free <- !((x <= lower + 1e-10 & g > 0) | (x >= upper - 1e-10 & g < 0))
        free_idx <- which(is_free); nfree <- length(free_idx)
        
        if (nfree > 0L) {
          g_f <- g[free_idx]; g_inf <- max(abs(g_f), na.rm = TRUE)
          B_f <- B[free_idx, free_idx, drop = FALSE]
          
          # 4.2) Subproblem: Newton Point and Cauchy Point
          pN_f <- tryCatch(
            solve(B_f, -g_f), 
            error = function(e) {
              ev <- eigen(B_f, symmetric = TRUE, only.values = TRUE)$values
              shift <- max(abs(min(ev)) + 1e-6, max(abs(ev)) * 1e-7)
              solve(B_f + diag(shift, nfree), -g_f)
            }
          )
          
          gnorm <- sqrt(sum(g_f^2)); Bg <- as.numeric(B_f %*% g_f); gBg <- sum(g_f * Bg)
          alpha_c <- if (gBg > 1e-15) (gnorm^2) / gBg else delta / max(gnorm, 1e-12)
          pC_f <- -alpha_c * g_f
          
          # 4.3) Interpolate Dogleg Step based on Delta
          nPN <- sqrt(sum(pN_f^2)); nPC <- sqrt(sum(pC_f^2))
          
          if (nPN <= delta) {
            p_f <- pN_f
          } else if (nPC >= delta) {
            p_f <- (delta / nPC) * pC_f
          } else {
            d_v <- (pN_f - pC_f); aa <- sum(d_v^2); bb <- 2 * sum(pC_f * d_v); cc <- sum(pC_f^2) - delta^2
            tau <- (-bb + sqrt(max(0, bb^2 - 4 * aa * cc))) / (2 * (aa + 1e-16))
            p_f <- pC_f + tau * d_v
          }
          current_pred_dec <- as.numeric(-(sum(g_f * p_f) + 0.5 * sum(p_f * (B_f %*% p_f))))
        } else {
          g_inf <- 0; current_pred_dec <- 0
        }
        
        # 4.4) Convergence Check
        res_conv <- TRUE
        if (ctrl$use_grad) res_conv <- res_conv && (g_inf <= ctrl$tol_grad)
        if (ctrl$use_rel_x && it > 1L) res_conv <- res_conv && (max(abs(x - x_old)) / max(1, max(abs(x_old)))) <= ctrl$tol_rel_x
        
        if (res_conv && it > 1L) {
          if (isTRUE(ctrl$use_posdef)) {
            H_eval <- tryCatch(hess_func(x), error = function(e) NULL)
            if (is_pd_fast(H_eval)) { converged <- TRUE; status <- "converged"; break } else res_conv <- FALSE
          } else { converged <- TRUE; status <- "converged"; break }
        }
        
        # 4.5) Step Acceptance & Hessian Update (Damped BFGS)
        p_full <- rep(0, n); if (nfree > 0L) p_full[free_idx] <- p_f
        x_try <- project(x + p_full, lower, upper); f_try <- eval_obj(x_try)
        actual_red <- f - f_try
        rho <- if (is.finite(current_pred_dec) && current_pred_dec > 1e-15) actual_red / current_pred_dec else 0
        
        if (rho > ctrl$rho_accept && actual_red > 0) {
          g_new <- grad_func(x_try); s <- x_try - x; y <- g_new - g
          
          # Damped BFGS Update
          Bs <- as.numeric(B %*% s); sBs <- sum(s * Bs); sy <- sum(s * y)
          update_ok <- FALSE; y_star <- y; sy_star <- sy
          if (isTRUE(ctrl$use_damped)) {
            if (sy < ctrl$damp_phi * sBs) {
              theta_damp <- ((1 - ctrl$damp_phi) * sBs) / (sBs - sy)
              y_star <- theta_damp * y + (1 - theta_damp) * Bs; sy_star <- sum(s * y_star)
            }
            if (sy_star > 1e-12) { y <- y_star; sy <- sy_star; update_ok <- TRUE }
          } else { if (sy > 1e-12) update_ok <- TRUE }
          
          if (update_ok) {
            B <- B - (Bs %*% t(Bs)) / (sBs + 1e-16) + (y %*% t(y)) / (sy + 1e-16)
            B <- 0.5 * (B + t(B))
          }
          
          x_old <- x; f_old <- f; x <- x_try; f <- f_try; g <- g_new
          if (rho > ctrl$rho_expand) delta <- min(ctrl$delta_max, ctrl$delta_expand * delta)
        } else {
          delta <- ctrl$delta_shrink * delta
          if (delta < 1e-14) { status <- "radius_too_small"; break }
        }
      }
    }, error = function(e) { status <<- paste0("runtime_error: ", conditionMessage(e)) })
  }
  
  # ---------- 5. Final Reporting ----------
  if (is.null(H_eval)) H_eval <- tryCatch(hess_func(x), error = function(e) NULL)
  final_clock <- proc.time() - start_clock
  
  list(
    par              = x, 
    objective        = f, 
    converged        = converged, 
    status           = status, 
    iter             = it, 
    cpu_time         = as.numeric(final_clock[1] + final_clock[2]), 
    elapsed_time     = as.numeric(final_clock[3]), 
    max_grad         = as.numeric(g_inf), 
    Hessian          = H_eval
  )
}