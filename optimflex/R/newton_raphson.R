#' Pure Newton-Raphson Optimization
#'
#' @description
#' Implements the standard Newton-Raphson algorithm for non-linear optimization 
#' without Hessian modifications or ridge adjustments.
#'
#' @details
#' \code{newton_raphson} provides a classic second-order optimization approach. 
#' 
#' \bold{Comparison with Modified Newton:}
#' Unlike \code{modified_newton}, this function does not apply dynamic ridge 
#' adjustments (Levenberg-Marquardt style) to the Hessian. If the Hessian is 
#' singular or cannot be inverted via \code{solve()}, the algorithm will 
#' terminate. This "pure" implementation is often preferred in simulation 
#' studies where the behavior of the exact Newton step is of interest.
#'
#' \bold{Predicted Decrease:}
#' This function explicitly calculates the \bold{Predicted Decrease} (\eqn{pred\_dec}), 
#' which is the expected reduction in the objective function value based on 
#' the local quadratic model: 
#' \deqn{m(p) = f + g^T p + \frac{1}{2} p^T H p}
#'
#' \bold{Stability and Simulations:}
#' All return values are explicitly cast to scalars (e.g., \code{as.numeric}, 
#' \code{as.logical}) to ensure stability when the function is called within 
#' large-scale simulation loops or packaged into data frames.
#'
#' @references
#' \itemize{
#'    \item Nocedal, J., & Wright, S. J. (2006). \emph{Numerical Optimization}. Springer.
#'    \item Bollen, K. A. (1989). \emph{Structural Equations with Latent Variables}. Wiley.
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
#'      \item \code{diff_method}: String. Method for numerical differentiation.
#'    }
#' @param ... Additional arguments passed to objective, gradient, and Hessian functions.
#'
#' @return A list containing optimization results and iteration metadata.
#' @export
newton_raphson <- function(
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
    diff_method      = "forward"
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
  x <- as.numeric(start); n <- length(x); start_clock <- proc.time()
  f <- tryCatch(eval_obj(x), error = function(e) NA_real_)
  it <- 0L; x_old <- x; f_old <- NA_real_; converged <- FALSE; status <- "running"
  H_last <- NULL; g_inf <- NA_real_; is_pd <- FALSE; pred_dec <- NA_real_
  
  if (!is.finite(f)) { 
    status <- "objective_error_at_start" 
  } else {
    g <- grad_func(x)
    
    # ---------- 4. Main Optimization Loop ----------
    tryCatch({
      repeat {
        if (it >= ctrl$max_iter) { status <- "iteration_limit_reached"; break }
        it <- it + 1L; g_inf <- max(abs(g), na.rm = TRUE)
        
        # 4.1) Hessian Evaluation: Symmetry enforcement
        H <- hess_func(x); H <- 0.5 * (H + t(H)); H_last <- H
        
        # 4.2) Pure Newton Step: Direct solve without modifications
        is_pd <- is_pd_fast(H)
        step <- try(solve(H, -g), silent = TRUE)
        
        if (inherits(step, "try-error") || any(!is.finite(step))) { 
          status <- "singular_hessian_no_step"; break 
        }
        
        gTp <- sum(g * step)
        
        # 4.3) Predicted Decrease calculation
        if (isTRUE(ctrl$use_pred_f) || isTRUE(ctrl$use_pred_f_avg)) {
          pred_dec <- as.numeric(-(gTp + 0.5 * sum(step * (H %*% step))))
        }
        
        # 4.4) Convergence Verification (Accessing flags via ctrl$)
        res_conv <- TRUE
        if (ctrl$use_grad) res_conv <- res_conv && (g_inf <= ctrl$tol_grad)
        if (ctrl$use_abs_f) res_conv <- res_conv && (!is.na(f) && abs(f) <= ctrl$tol_abs_f)
        if (ctrl$use_rel_f && it > 1L && !is.na(f_old)) {
          res_conv <- res_conv && (abs((f - f_old) / max(1, abs(f_old))) <= ctrl$tol_rel_f)
        }
        if (ctrl$use_abs_x) res_conv <- res_conv && (max(abs(x - x_old)) <= ctrl$tol_abs_x)
        if (ctrl$use_rel_x && it > 1L) {
          res_conv <- res_conv && (max(abs(x - x_old)) / max(1, max(abs(x_old))) <= ctrl$tol_rel_x)
        }
        if (isTRUE(ctrl$use_pred_f)) {
          res_conv <- res_conv && (!is.na(pred_dec) && abs(pred_dec) <= ctrl$tol_pred_f)
        }
        
        if (res_conv) {
          if (isTRUE(ctrl$use_posdef)) {
            if (is_pd) { converged <- TRUE; status <- "converged"; break } else { res_conv <- FALSE } 
          } else { 
            converged <- TRUE; status <- "converged"; break 
          }
        }
        
        # 4.5) Backtracking Line Search
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
  
  # ---------- 5. Final Reporting (Scalar-Safe) ----------
  final_clock <- proc.time() - start_clock
  
  list(
    par          = x, 
    objective    = as.numeric(f)[1], 
    converged    = as.logical(converged)[1], 
    status       = as.character(status)[1], 
    iter         = as.integer(it)[1],
    cpu_time     = as.numeric(final_clock[1] + final_clock[2])[1], 
    elapsed_time = as.numeric(final_clock[3])[1],
    max_grad     = as.numeric(g_inf)[1], 
    Hess_is_pd   = as.logical(is_pd)[1], 
    Hessian      = H_last,
    pred_dec     = as.numeric(pred_dec)[1]
  )
}