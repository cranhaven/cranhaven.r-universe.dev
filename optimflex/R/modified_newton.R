#' Modified Newton-Raphson Optimization
#'
#' @description
#' Implements an optimized Newton-Raphson algorithm for non-linear optimization 
#' featuring dynamic ridge adjustment and backtracking line search.
#'
#' @details
#' \code{modified_newton} is a line search optimization algorithm that utilizes 
#' second-order curvature information (the Hessian matrix) to find the minimum 
#' of an objective function.
#' 
#' \bold{Modified Newton vs. Trust-Region:}
#' Unlike the \code{dogleg} and \code{double_dogleg} functions which use a 
#' Trust-Region approach to constrain the step size, this function uses a 
#' \bold{Line Search} approach. It first determines the Newton direction 
#' (the solution to \eqn{H \Delta x = -g}) and then performs a backtracking line 
#' search to find a step length \eqn{\alpha} that satisfies the sufficient decrease 
#' condition (Armijo condition).
#'
#' \bold{Dynamic Ridge Adjustment:}
#' If the Hessian matrix \eqn{H} is not positive definite (making it unsuitable for 
#' Cholesky decomposition), the algorithm applies a dynamic ridge adjustment. 
#' A diagonal matrix \eqn{\tau I} is added to the Hessian, where \eqn{\tau} is 
#' increased until the matrix becomes positive definite. This ensures the 
#' search direction always remains a descent direction.
#'
#' \bold{Differentiation Methods:}
#' The function allows for independent selection of differentiation methods for 
#' the gradient and Hessian:
#' \itemize{
#'    \item \code{forward}: Standard forward-difference numerical differentiation.
#'    \item \code{central}: Central-difference (more accurate but slower).
#'    \item \code{complex}: Complex-step differentiation (highly accurate for gradients).
#'    \item \code{richardson}: Richardson extrapolation via the \code{numDeriv} package.
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
#'      \item \code{grad_diff}: String. Method for gradient differentiation.
#'      \item \code{hess_diff}: String. Method for Hessian differentiation.
#'    }
#' @param ... Additional arguments passed to objective, gradient, and Hessian functions.
#'
#' @return A list containing optimization results and iteration metadata.
#' @export
modified_newton <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,
    lower          = -Inf,
    upper          = Inf,
    control        = list(),
    ...
) {
  
  # ---------- 1. Default Configuration (Synced with Suite) ----------
  ctrl0 <- list(
    # Convergence and recording flags
    use_abs_f       = FALSE,
    use_rel_f       = FALSE,
    use_abs_x       = FALSE,
    use_rel_x       = TRUE,
    use_grad        = TRUE,
    use_posdef      = TRUE,
    use_pred_f      = FALSE,
    use_pred_f_avg  = FALSE,
    
    # Algorithm parameters
    max_iter        = 1000L,
    tol_abs_f       = 1e-6,
    tol_rel_f       = 1e-6,
    tol_abs_x       = 1e-6,
    tol_rel_x       = 1e-6,
    tol_grad        = 1e-4,
    tol_pred_f      = 1e-4,
    tol_pred_f_avg  = 1e-4,
    wolfe_c1        = 1e-4,
    ls_alpha0       = 1.0,
    ls_max_steps    = 30L,
    ridge_offset    = 1e-4,
    grad_diff       = "forward",
    hess_diff       = "forward"
  )
  ctrl <- utils::modifyList(ctrl0, control)
  
  ctrl$grad_diff <- match.arg(ctrl$grad_diff, c("forward", "central", "richardson", "complex"))
  ctrl$hess_diff <- match.arg(ctrl$hess_diff, c("forward", "central", "richardson"))
  
  if (any(c(ctrl$grad_diff, ctrl$hess_diff) %in% c("richardson", "complex"))) {
    if (!requireNamespace("numDeriv", quietly = TRUE)) stop("Package 'numDeriv' required.")
  }
  
  # ---------- 2. Internal Helpers ----------
  eval_obj <- function(z) as.numeric(objective(z, ...))[1]
  
  grad_func <- if (!is.null(gradient)) {
    function(z) as.numeric(gradient(z, ...))
  } else if (ctrl$grad_diff == "richardson") {
    function(z) as.numeric(numDeriv::grad(objective, z, method = "Richardson", ...))
  } else if (ctrl$grad_diff == "complex") {
    function(z) as.numeric(numDeriv::grad(objective, z, method = "complex", ...))
  } else {
    function(z) fast_grad(objective, z, diff_method = ctrl$grad_diff, ...)
  }
  
  hess_func <- if (!is.null(hessian)) {
    function(z) hessian(z, ...)
  } else if (ctrl$hess_diff == "richardson") {
    function(z) numDeriv::hessian(objective, z, method = "Richardson", ...)
  } else {
    function(z) fast_hess(objective, z, diff_method = ctrl$hess_diff, ...)
  }
  
  # ---------- 3. Initialization ----------
  x <- as.numeric(start); n <- length(x); start_clock <- proc.time()
  f <- tryCatch(eval_obj(x), error = function(e) NA_real_)
  it <- 0L; x_old <- x; f_old <- f; converged <- FALSE; status <- "running"
  H_last <- NULL; is_pd <- FALSE; g_inf <- NA_real_
  
  if (!is.finite(f)) { 
    status <- "objective_error_at_start" 
  } else {
    g <- grad_func(x)
    
    # ---------- 4. Main Optimization Loop ----------
    tryCatch({
      repeat {
        if (it >= ctrl$max_iter) { status <- "iteration_limit_reached"; break }
        it <- it + 1L; g_inf <- max(abs(g), na.rm = TRUE)
        
        # 4.1) Single Hessian Evaluation: Forced symmetry
        H <- hess_func(x); H <- 0.5 * (H + t(H)); H_last <- H
        
        # 4.2) Step Calculation with Dynamic Ridge Adjustment
        R <- try(chol(H), silent = TRUE)
        if (!inherits(R, "try-error")) {
          is_pd <- TRUE
          step <- backsolve(R, forwardsolve(t(R), -g))
        } else {
          is_pd <- FALSE
          tau <- ctrl$ridge_offset
          repeat {
            H_mod <- H + diag(tau, n)
            R <- try(chol(H_mod), silent = TRUE)
            if (!inherits(R, "try-error")) {
              step <- backsolve(R, forwardsolve(t(R), -g))
              break
            }
            tau <- tau * 10
            if (tau > 1e6) { status <- "ridge_failed"; break }
          }
        }
        
        if (status == "ridge_failed") break
        gTp <- sum(g * step) 
        
        # 4.3) Convergence Verification (Accessing flags via ctrl$)
        res_conv <- TRUE
        if (ctrl$use_grad) res_conv <- res_conv && (g_inf <= ctrl$tol_grad)
        if (ctrl$use_abs_f && !is.na(f_old)) res_conv <- res_conv && (abs(f - f_old) <= ctrl$tol_abs_f)
        if (ctrl$use_rel_f && !is.na(f_old)) {
          res_conv <- res_conv && (abs((f - f_old) / max(1, abs(f_old))) <= ctrl$tol_rel_f)
        }
        if (ctrl$use_abs_x && it > 1L) res_conv <- res_conv && (max(abs(x - x_old)) <= ctrl$tol_abs_x)
        if (ctrl$use_rel_x && it > 1L) {
          res_conv <- res_conv && (max(abs(x - x_old)) / max(1, max(abs(x_old))) <= ctrl$tol_rel_x)
        }
        
        if (res_conv) {
          if (isTRUE(ctrl$use_posdef)) {
            if (is_pd) { converged <- TRUE; status <- "converged"; break } else { res_conv <- FALSE }
          } else { converged <- TRUE; status <- "converged"; break }
        }
        
        # 4.4) Backtracking Line Search (Armijo condition)
        alpha <- ctrl$ls_alpha0; ls_ok <- FALSE
        for (ls_it in seq_len(ctrl$ls_max_steps)) {
          xi <- x + alpha * step; fi <- eval_obj(xi)
          if (is.finite(fi) && fi <= f + ctrl$wolfe_c1 * alpha * gTp) {
            x_old <- x; f_old <- f; x <- xi; f <- fi; ls_ok <- TRUE; break
          }
          alpha <- alpha * 0.5 
        }
        if (!ls_ok) { status <- "line_search_failed"; break }
        
        g <- grad_func(x)
      }
    }, error = function(e) { status <<- paste0("runtime_error: ", conditionMessage(e)) })
  }
  
  # ---------- 5. Final Reporting ----------
  final_clock <- proc.time() - start_clock
  list(
    par = x, objective = f, converged = converged, status = status, iter = it,
    cpu_time = as.numeric(final_clock[1] + final_clock[2]), 
    elapsed_time = as.numeric(final_clock[3]),
    max_grad = as.numeric(g_inf), Hess_is_pd = is_pd, Hessian = H_last
  )
}