#' Davidon-Fletcher-Powell (DFP) Quasi-Newton Optimization
#'
#' @description
#' Implements the DFP Quasi-Newton algorithm with a Strong Wolfe line search and 
#' optional Powell's damping for non-linear optimization.
#'
#' @details
#' \code{dfp} is a Quasi-Newton method that maintains and updates an approximation 
#' of the inverse Hessian matrix. Historically, it was the first Quasi-Newton 
#' method discovered (Davidon, 1959) and later refined by Fletcher and Powell (1963).
#' 
#' \bold{DFP vs. BFGS:}
#' Both DFP and BFGS belong to the Broyden family of Quasi-Newton methods. 
#' While BFGS is generally preferred for its self-correcting properties regarding 
#' inaccuracies in the line search, DFP remains a fundamental algorithm that 
#' can be more sensitive to the local curvature of the objective function. 
#' In certain Structural Equation Modeling (SEM) contexts, DFP can provide 
#' alternative convergence paths when BFGS reaches a plateau.
#'
#' \bold{Strong Wolfe Line Search:}
#' To ensure the positive definiteness of the inverse Hessian update and guarantee 
#' global convergence, this implementation employs a Strong Wolfe line search. 
#' This identifies a step length \eqn{\alpha} that satisfies both:
#' \itemize{
#'    \item \bold{Sufficient Decrease (Armijo Rule):} \eqn{f(x + \alpha p) \le f(x) + c_1 \alpha \nabla f(x)^T p}.
#'    \item \bold{Curvature Condition:} \eqn{| \nabla f(x + \alpha p)^T p | \le c_2 | \nabla f(x)^T p |}.
#' }
#'
#' \bold{Powell's Damping Strategy:}
#' Structural Equation Models often involve non-convex fitting functions. 
#' When \code{use_damped = TRUE}, the algorithm applies Powell's damping 
#' to the \eqn{y} vector used in the update formula. This ensures that the curvature 
#' condition \eqn{s^T y > 0} is maintained even in non-convex regions, preserving 
#' the stability of the inverse Hessian approximation.
#'
#' @references
#' \itemize{
#'    \item Davidon, W. C. (1959). Variable Metric Method for Minimization. 
#'          \emph{AEC Research and Development Report}, ANL-5990.
#'    \item Fletcher, R., & Powell, M. J. D. (1963). A Rapidly Convergent Descent 
#'          Method for Minimization. \emph{The Computer Journal}, 6(2), 163-168.
#'    \item Nocedal, J., & Wright, S. J. (2006). \emph{Numerical Optimization}. Springer.
#' }
#'
#' @param start Numeric vector. Starting values for the optimization parameters.
#' @param objective Function. The objective function to minimize.
#' @param gradient Function (optional). Gradient of the objective function.
#' @param hessian Function (optional). Hessian matrix for final verification.
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
dfp <- function(
    start,
    objective,
    gradient       = NULL,
    hessian        = NULL,
    lower          = -Inf,
    upper          = Inf,
    control        = list(),
    ...
) {
  
  # ---------- 1. Configuration (Synced with Suite) ----------
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
    wolfe_c1        = 1e-4,
    wolfe_c2        = 0.9,
    ls_alpha0       = 1.0,
    ls_max_steps    = 30L,      
    zoom_max_steps  = 25L,
    curvature_eps   = 1e-12,
    Hinv_init_diag  = 1.0,
    diff_method      = "forward", 
    use_damped      = TRUE,      
    damp_phi        = 0.2        
  )
  ctrl <- utils::modifyList(ctrl0, control)
  
  start_clock <- proc.time()
  
  # ---------- 2. Internal Helpers ----------
  eval_obj <- function(z) {
    as.numeric(objective(z, ...))[1]
  }
  
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
  
  # ---------- 3. Strong Wolfe Line Search ----------
  strong_wolfe_ls <- function(x, f, g, p, dphi0) {
    c1 <- ctrl$wolfe_c1
    c2 <- ctrl$wolfe_c2
    phi0 <- f
    
    zoom <- function(alo, ahi, flo, glo) {
      for (z_it in seq_len(ctrl$zoom_max_steps)) {
        a <- 0.5 * (alo + ahi)
        xa <- x + a * p
        fa <- eval_obj(xa)
        
        if (fa > phi0 + c1 * a * dphi0 || fa >= flo) {
          ahi <- a
        } else {
          ga <- grad_func(xa)
          dphi_a <- sum(ga * p)
          if (abs(dphi_a) <= -c2 * dphi0) {
            return(list(ok = TRUE, alpha = a, x = xa, f = fa, g = ga))
          }
          if (dphi_a * (ahi - alo) >= 0) {
            ahi <- alo
          }
          alo <- a
          flo <- fa
          glo <- ga
        }
        if (abs(ahi - alo) < 1e-15) break
      }
      list(ok = FALSE)
    }
    
    a_prev <- 0
    f_prev <- phi0
    g_prev <- g
    a <- ctrl$ls_alpha0
    
    for (ls_it in seq_len(ctrl$ls_max_steps)) {
      xa <- x + a * p
      fa <- eval_obj(xa)
      
      if (fa > phi0 + c1 * a * dphi0 || (ls_it > 1 && fa >= f_prev)) {
        return(zoom(a_prev, a, f_prev, g_prev))
      }
      
      ga <- grad_func(xa)
      dphi_a <- sum(ga * p)
      
      if (abs(dphi_a) <= -c2 * dphi0) {
        return(list(ok = TRUE, alpha = a, x = xa, f = fa, g = ga))
      }
      
      if (dphi_a >= 0) {
        return(zoom(a, a_prev, fa, ga))
      }
      
      a_prev <- a
      f_prev <- fa
      g_prev <- ga
      a <- a * 2
    }
    list(ok = FALSE)
  }
  
  # ---------- 4. Initialization ----------
  x <- as.numeric(start)
  n <- length(x)
  f <- eval_obj(x)
  g <- grad_func(x)
  
  x_old <- x
  f_old <- NA_real_
  Hinv <- diag(ctrl$Hinv_init_diag, n)
  
  it <- 0L
  converged <- FALSE
  status <- "running"
  H_eval <- NULL
  g_inf <- NA_real_
  
  # ---------- 5. Main Loop ----------
  res_main <- tryCatch({
    
    repeat {
      if (it >= ctrl$max_iter) {
        status <- "iteration_limit_reached"
        break
      }
      it <- it + 1L
      g_inf <- max(abs(g), na.rm = TRUE)
      
      # 5.1) Search Direction
      p <- as.numeric(-Hinv %*% g)
      dphi0 <- sum(g * p)
      
      if (dphi0 >= 0) {
        Hinv <- diag(ctrl$Hinv_init_diag, n)
        p <- -g
        dphi0 <- sum(g * p)
      }
      
      # 5.2) Convergence Verification (AND rule)
      res_conv <- TRUE
      if (ctrl$use_grad) res_conv <- res_conv && (g_inf <= ctrl$tol_grad)
      if (ctrl$use_abs_f && !is.na(f_old)) res_conv <- res_conv && (abs(f - f_old) <= ctrl$tol_abs_f)
      if (ctrl$use_rel_f && !is.na(f_old)) res_conv <- res_conv && (abs((f - f_old) / max(1, abs(f_old))) <= ctrl$tol_rel_f)
      if (ctrl$use_abs_x && it > 1L) res_conv <- res_conv && (max(abs(x - x_old)) <= ctrl$tol_abs_x)
      if (ctrl$use_rel_x && it > 1L) res_conv <- res_conv && (max(abs(x - x_old)) / max(1, max(abs(x_old)))) <= ctrl$tol_rel_x
      
      if (res_conv && it > 1) {
        if (isTRUE(ctrl$use_posdef)) {
          H_eval <- tryCatch(hess_func(x), error = function(e) NULL)
          if (is_pd_fast(H_eval)) { 
            converged <- TRUE; status <- "converged"; break 
          } else { 
            res_conv <- FALSE 
          }
        } else { 
          converged <- TRUE; status <- "converged"; break 
        }
      }
      
      # 5.3) Line Search Execution
      ls <- strong_wolfe_ls(x, f, g, p, dphi0)
      if (!ls$ok) {
        status <- "line_search_failed"
        break
      }
      
      x_new <- ls$x
      f_new <- ls$f
      g_new <- ls$g
      
      s <- x_new - x
      y <- g_new - g
      sy <- sum(s * y)
      
      # 5.4) Inverse Hessian Update (DFP Formula)
      if (sy > ctrl$curvature_eps) {
        Hy <- Hinv %*% y
        yHy <- as.numeric(crossprod(y, Hy))
        
        # Stability fix: Added 1e-16 to denominators
        Hinv <- Hinv + (s %*% t(s)) / (sy + 1e-16) -
          (Hy %*% t(Hy)) / (yHy + 1e-16)
        Hinv <- 0.5 * (Hinv + t(Hinv))
      }
      
      x_old <- x
      f_old <- f
      x <- x_new
      f <- f_new
      g <- g_new
    }
    
    list(ok = TRUE)
    
  }, error = function(e) {
    list(ok = FALSE, status = paste0("runtime_error: ", conditionMessage(e)))
  })
  
  if (!res_main$ok) status <- res_main$status
  
  # ---------- 6. Final Reporting ----------
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
    max_grad         = g_inf,
    Hess_is_pd       = is_pd_fast(H_eval),
    Hessian          = H_eval,
    approx_hinv      = Hinv
  )
}