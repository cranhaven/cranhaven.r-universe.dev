#' Double Dogleg Trust-Region Optimization
#'
#' @description
#' Implements the Double Dogleg Trust-Region algorithm for non-linear optimization.
#'
#' @details
#' This function implements the Double Dogleg method within a Trust-Region framework, 
#' primarily based on the work of Dennis and Mei (1979). 
#' 
#' \bold{Trust-Region vs. Line Search:}
#' While Line Search methods (like BFGS) first determine a search direction and then 
#' find an appropriate step length, Trust-Region methods define a neighborhood 
#' around the current point (the trust region with radius \eqn{\Delta}) where a local 
#' quadratic model is assumed to be reliable. The algorithm then finds a step that 
#' minimizes this model within the radius. This approach is generally more robust, 
#' especially when the Hessian is not positive definite.
#'
#' \bold{Powell's Dogleg vs. Double Dogleg:}
#' Powell's original Dogleg method (1970) constructs a trajectory consisting of 
#' two line segments: one from the current point to the Cauchy point, and another 
#' from the Cauchy point to the Newton point. The "Double Dogleg" modification 
#' by Dennis and Mei (1979) introduces an intermediate "bias" point (\eqn{p_W}) 
#' along the Newton direction.
#' \itemize{
#'   \item \bold{Cauchy Point (\eqn{p_C}):} The minimizer of the quadratic model along 
#'         the steepest descent direction.
#'   \item \bold{Newton Point (\eqn{p_N}):} The minimizer of the quadratic model \eqn{(B^{-1}g)}.
#'   \item \bold{Double Dogleg Point (\eqn{p_W}):} A point defined as \eqn{\gamma \cdot p_N}, 
#'         where \eqn{\gamma} is a scaling factor (bias) that ensures the path stays 
#'         closer to the Newton direction while maintaining monotonic descent in 
#'         the model.
#' }
#' This modification allows the algorithm to perform more like a Newton method 
#' earlier in the optimization process compared to the standard Dogleg.
#'
#' @references
#' \itemize{
#'   \item Dennis, J. E., & Mei, H. H. (1979). Two New Unconstrained Optimization 
#'         Algorithms which use Function and Gradient Values. 
#'         \emph{Journal of Optimization Theory and Applications}, 28(4), 453-482.
#'   \item Powell, M. J. D. (1970). A Hybrid Method for Nonlinear Equations. 
#'         \emph{Numerical Methods for Nonlinear Algebraic Equations}.
#'   \item Nocedal, J., & Wright, S. J. (2006). \emph{Numerical Optimization}. Springer.
#' }
#'
#' @param start Numeric vector. Starting values for the optimization parameters.
#' @param objective Function. The objective function to minimize.
#' @param gradient Function (optional). Gradient of the objective function.
#' @param hessian Function (optional). Hessian matrix of the objective function.
#' @param lower Numeric vector. Lower bounds for box constraints.
#' @param upper Numeric vector. Upper bounds for box constraints.
#' @param control List. Control parameters including convergence flags starting with 'use_'.
#'   \itemize{
#'     \item \code{use_abs_f}: Logical. Use absolute change in objective for convergence.
#'     \item \code{use_rel_f}: Logical. Use relative change in objective for convergence.
#'     \item \code{use_abs_x}: Logical. Use absolute change in parameters for convergence.
#'     \item \code{use_rel_x}: Logical. Use relative change in parameters for convergence.
#'     \item \code{use_grad}: Logical. Use gradient norm for convergence.
#'     \item \code{use_posdef}: Logical. Verify positive definiteness at convergence.
#'     \item \code{use_pred_f}: Logical. Record predicted objective decrease.
#'     \item \code{use_pred_f_avg}: Logical. Record average predicted decrease.
#'   }
#' @param ... Additional arguments passed to objective, gradient, and Hessian functions.
#'
#' @return A list containing optimization results and iteration metadata.
#' @export
double_dogleg <- function(
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
    dd_bias         = 0.8, 
    rho_accept      = 0.1, 
    rho_expand      = 0.75,
    delta_shrink    = 0.25, 
    delta_expand    = 2.0, 
    H_init_diag     = 1.0, 
    ridge_eps       = 1e-9, 
    diff_method      = "forward",
    hessian_update   = "bfgs",
    use_damped      = TRUE, 
    damp_phi        = 0.2
  )
  ctrl <- utils::modifyList(ctrl0, control)
  ctrl$diff_method <- match.arg(ctrl$diff_method, c("forward", "central", "richardson"))
  
  if (ctrl$diff_method == "richardson") {
    if (!requireNamespace("numDeriv", quietly = TRUE)) stop("Package 'numDeriv' required.")
  }
  
  # ---------- 2. Internal Helpers ----------
  eval_obj <- function(z) as.numeric(objective(z, ...))[1]
  
  grad_func <- if (!is.null(gradient)) {
    function(z) as.numeric(gradient(z, ...))
  } else if (ctrl$diff_method == "richardson") {
    function(z) as.numeric(numDeriv::grad(objective, z, method = "Richardson", ...))
  } else {
    function(z) fast_grad(objective, z, diff_method = ctrl$diff_method, ...)
  }
  
  hess_func <- if (!is.null(hessian)) {
    function(z) hessian(z, ...)
  } else if (ctrl$diff_method == "richardson") {
    function(z) numDeriv::hessian(objective, z, method = "Richardson", ...)
  } else {
    function(z) fast_hess(objective, z, diff_method = ctrl$diff_method, ...)
  }
  
  # ---------- 3. Initialization ----------
  x <- pmax(lower, pmin(as.numeric(start), upper))
  n <- length(x)
  
  start_clock <- proc.time() 
  f <- tryCatch(eval_obj(x), error = function(e) NA_real_)
  
  it <- 0L; converged <- FALSE; status <- "running"
  x_old <- x; f_old <- NA_real_; delta <- ctrl$initial_delta
  
  # Initialize Hessian approximation
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
        free_idx <- which(is_free)
        nfree <- length(free_idx)
        
        if (nfree > 0L) {
          g_f <- g[free_idx]
          g_inf <- max(abs(g_f), na.rm = TRUE)
          B_f <- B[free_idx, free_idx, drop = FALSE]
          
          # 4.2) Double Dogleg Subproblem Step
          pN_f <- tryCatch(
            solve(B_f, -g_f), 
            error = function(e) {
              ev <- eigen(B_f, symmetric = TRUE, only.values = TRUE)$values
              minev <- min(ev)
              shift <- if (minev < 0) abs(minev) + 1e-6 else 1e-6
              shift <- max(shift, max(abs(ev)) * 1e-7)
              solve(B_f + diag(shift, nfree), -g_f)
            }
          )
          
          gnorm <- sqrt(sum(g_f^2))
          Bg <- as.numeric(B_f %*% g_f)
          gBg <- sum(g_f * Bg)
          alpha_c <- if (gBg > 1e-15) (gnorm^2) / gBg else delta / max(gnorm, 1e-12)
          pC_f <- -alpha_c * g_f
          
          ghinvg <- sum(g_f * (-pN_f))
          gamma <- if (ghinvg > 1e-15 && gBg > 1e-15) ctrl$dd_bias * (gnorm^4 / (gBg * ghinvg)) else 1.0
          gamma <- max(alpha_c, min(1.0, gamma))
          pW_f <- gamma * pN_f
          
          nPN <- sqrt(sum(pN_f^2)); nPC <- sqrt(sum(pC_f^2)); nPW <- sqrt(sum(pW_f^2))
          
          # Interpolation logic for step selection
          if (nPN <= delta) {
            p_f <- pN_f
          } else if (nPC >= delta) {
            p_f <- (delta / nPC) * pC_f
          } else if (nPW <= delta) {
            d <- (pN_f - pW_f); aa <- sum(d^2); bb <- 2 * sum(pW_f * d); cc <- sum(pW_f^2) - delta^2
            tau <- (-bb + sqrt(max(0, bb^2 - 4 * aa * cc))) / (2 * (aa + 1e-16))
            p_f <- pW_f + tau * d
          } else {
            d <- (pW_f - pC_f); aa <- sum(d^2); bb <- 2 * sum(pC_f * d); cc <- sum(pC_f^2) - delta^2
            tau <- (-bb + sqrt(max(0, bb^2 - 4 * aa * cc))) / (2 * (aa + 1e-16))
            p_f <- pC_f + tau * d
          }
          current_pred_dec <- as.numeric(-(sum(g_f * p_f) + 0.5 * sum(p_f * (B_f %*% p_f))))
        } else {
          g_inf <- 0; current_pred_dec <- 0
        }
        
        # 4.3) Convergence Verification
        res_conv <- TRUE
        if (ctrl$use_grad) res_conv <- res_conv && (g_inf <= ctrl$tol_grad)
        if (ctrl$use_abs_f && !is.na(f_old)) res_conv <- res_conv && (abs(f - f_old) <= ctrl$tol_abs_f)
        if (ctrl$use_rel_f && !is.na(f_old)) res_conv <- res_conv && (abs((f - f_old) / max(1, abs(f_old))) <= ctrl$tol_rel_f)
        if (ctrl$use_abs_x && it > 1L) res_conv <- res_conv && (max(abs(x - x_old)) <= ctrl$tol_abs_x)
        if (ctrl$use_rel_x && it > 1L) res_conv <- res_conv && (max(abs(x - x_old)) / max(1, max(abs(x_old)))) <= ctrl$tol_rel_x
        
        if (res_conv && it > 1) {
          if (isTRUE(ctrl$use_posdef)) {
            H_eval <- tryCatch(hess_func(x), error = function(e) NULL)
            if (is_pd_fast(H_eval)) { converged <- TRUE; status <- "converged"; break } else res_conv <- FALSE
          } else { converged <- TRUE; status <- "converged"; break }
        }
        
        # 4.4) Step Acceptance & Trust Region Management
        p_full <- rep(0, n)
        if (nfree > 0L) p_full[free_idx] <- p_f
        x_try <- pmax(lower, pmin(x + p_full, upper))
        f_try <- eval_obj(x_try)
        actual_red <- f - f_try
        rho <- if (is.finite(current_pred_dec) && current_pred_dec > 1e-15) actual_red / current_pred_dec else 0
        
        if (rho > ctrl$rho_accept && actual_red > 0) {
          g_new <- grad_func(x_try); s <- x_try - x; y <- g_new - g
          
          # Hessian Matrix Update
          if (ctrl$hessian_update == "full") {
            B <- tryCatch(hess_func(x_try), error = function(e) B)
            B <- 0.5 * (B + t(B))
          } else {
            Bs <- as.numeric(B %*% s); sBs <- sum(s * Bs); sy <- sum(s * y)
            
            # Initial Scaling
            if (it == 1L && is.finite(sy) && sy > 1e-12) {
              y_norm_sq <- sum(y * y)
              if (y_norm_sq > 1e-12) B <- B * (y_norm_sq / sy)
              Bs <- as.numeric(B %*% s); sBs <- sum(s * Bs) 
            }
            
            # Powell's Damping Strategy
            update_ok <- FALSE
            y_star <- y; sy_star <- sy
            if (isTRUE(ctrl$use_damped)) {
              if (is.finite(sBs) && sBs > 1e-12) {
                if (sy < ctrl$damp_phi * sBs) {
                  theta <- ((1 - ctrl$damp_phi) * sBs) / (sBs - sy)
                  y_star <- theta * y + (1 - theta) * Bs
                  sy_star <- sum(s * y_star)
                }
              }
              if (is.finite(sy_star) && sy_star > 1e-12) { y <- y_star; sy <- sy_star; update_ok <- TRUE }
            } else {
              if (is.finite(sy) && sy > 1e-12) update_ok <- TRUE
            }
            
            if (update_ok) {
              Bs <- as.numeric(B %*% s); sBs <- sum(s * Bs)
              B <- B - (Bs %*% t(Bs)) / (sBs + 1e-16) + (y %*% t(y)) / (sy + 1e-16)
              B <- 0.5 * (B + t(B))
            }
          }
          
          # Record Predictive Values
          if (isTRUE(ctrl$use_pred_f) || isTRUE(ctrl$use_pred_f_avg)) {
            pred_dec <- current_pred_dec; pred_dec_avg <- pred_dec / n
          }
          
          x_old <- x; f_old <- f; x <- x_try; f <- f_try; g <- g_new
          if (rho > ctrl$rho_expand) delta <- min(ctrl$delta_max, ctrl$delta_expand * delta)
        } else {
          # Step Rejected: Shrink Trust Region
          delta <- ctrl$delta_shrink * delta
          if (delta < 1e-14) { status <- "radius_too_small"; break }
        }
      }
    }, error = function(e) { status <<- paste0("runtime_error: ", conditionMessage(e)) })
  }
  
  # ---------- 5. Final Status & Output Construction ----------
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
    Hess_is_pd       = is_pd_fast(H_eval), 
    Hessian          = H_eval, 
    approx_hessian   = B,
    pred_dec         = pred_dec,
    pred_dec_avg     = pred_dec_avg
  )
}