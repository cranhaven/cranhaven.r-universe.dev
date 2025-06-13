# Fitting functions
# Arseniy Khvorov
# Created 2019/07/31
# Last edit 2019/10/16

#' Fitter function for the scaled logit model
#'
#' Computing engine behind \code{\link{sclr}}.
#'
#' The likelihood maximisation can use the Newton-Raphson or the gradient
#' ascent algorithms.
#'
#' @param y A vector of observations.
#' @param x A design matrix.
#' @param tol Tolerance.
#' @param algorithm Algorithms to run. "newton-raphson" or "gradient-ascent".
#'   If a character vector, the algorithms will be applied in the order they
#'   are present in the vector.
#' @param nr_iter Maximum allowed iterations for Newton-Raphson.
#' @param ga_iter Maximum allowed iterations for gradient ascent.
#' @param n_conv Number of times the algorithm has to converge (to work around
#'   local maxima).
#' @param conventional_names If \code{TRUE}, estimated parameter names will be
#'   (Baseline), (Intercept) and the column names in the model matrix. Otherwise
#'   - lambda, beta_0 and beta_ prefix in front of column names in the model
#'   matrix.
#' @param seed Seed for the algorithms.
#'
#' @importFrom rlang abort
#'
#' @export
sclr_fit <- function(y, x, 
                     tol = 10^(-7), 
                     algorithm = c("newton-raphson", "gradient-ascent"),
                     nr_iter = 2e3, ga_iter = 2e3, n_conv = 3,
                     conventional_names = FALSE, seed = NULL) {
  alg_dict <- list(
    "newton-raphson" = list(fun = newton_raphson, max = nr_iter),
    "gradient-ascent" = list(fun = gradient_ascent, max = ga_iter)
  )
  
  if (!all(algorithm %in% names(alg_dict)))
    abort(
      paste0(
        "`algorithm` should be in: ", 
        paste(names(alg_dict), collapse = " ")
      )
    )
  
  x_coeffs <- get_x_coeffs(x) # To avoid recalculations
  
  for (alg_name in algorithm) {
    out <- run_algorithm(
      alg_name, alg_dict[[alg_name]]$fun,
      n_conv, y, x, x_coeffs, alg_dict[[alg_name]]$max, 
      tol, conventional_names, seed
    )
    if (!is.null(out$parameters) && !is.null(out$covariance_mat)) return(out)
  }
  out
}

#' Run one algorithm
#'
#' @param name Name of the algorithm
#' @param fun Function that runs it
#' @inheritParams newton_raphson
#'
#' @noRd
run_algorithm <- function(name, fun, n_conv, y, x, 
                          x_coeffs, max_iter, tol, conventional_names, 
                          seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  init_mats <- lapply(
    1:n_conv, function(i) get_init_pars_mat(y, x, conventional_names)
  )
  
  rets <- lapply(
    1:n_conv,
    function(i) fun(y, x, init_mats[[i]], x_coeffs, max_iter, tol)
  )
  lls <- sapply(
    rets, function(ret) {
      if (is.null(ret$found)) return(-Inf)
      sclr_log_likelihood(x = x, y = y, pars = ret$found)
    }
  )
  if (all(lls == -Inf)) 
    warn(
      paste0(
        name, " did not converge, check for boundary with check_baseline()"
      )
    )
  else if (sum(lls == -Inf) > 0) 
    warn(paste0(
      name, " only converged ", length(lls) - sum(lls == -Inf),
      " time(s) out of ", n_conv
    ))
  list(
    parameters = rets[[which.max(lls)]]$found,
    covariance_mat = rets[[which.max(lls)]]$cov,
    algorithm = name,
    algorithm_return = rets
  )
}

#' Generalised Newton-Raphson
#'
#' @param y Model matrix
#' @param x Model response
#' @param pars_mat Initial parameter matrix
#' @param x_coeffs Matrix of pairwise products of x
#' @param max_iter Maximum allowed iterations
#' @param tol Tolerance
#' @param conventional_names Whether to give conventional names to parameters
#' @param seed Seed
#'
#' @noRd
newton_raphson <- function(y, x, pars_mat,
                           x_coeffs, max_iter, tol, 
                           seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)

  add_reset <- function(out, ind, reason) {
    out[["reset_index"]] <- c(out[["reset_index"]], ind)
    out[["reset_reason"]] <- c(out[["reset_reason"]], reason)
    out
  }
  
  out <- list(init_mat = pars_mat)
  cur_iter <- 1

  for (cur_iter in 1:max_iter) {

    pars_mat_prev <- pars_mat # Save for later
    
    # Calculate the commonly occuring expession
    exp_Xb <- get_exp_Xb(y, x, pars_mat)
    
    # Scores
    scores_mat <- get_scores(y, x, pars_mat, exp_Xb)
    if (any(is.na(scores_mat))) {
      pars_mat <- guess_again(pars_mat)
      out <- add_reset(out, cur_iter, "could not calculate scores")
      next
    }
    
    # Log-likelihood second derivative (negative of information) matrix
    hessian_mat <- get_hessian(y, x, pars_mat, exp_Xb, x_coeffs)
    if (any(is.na(hessian_mat))) {
      pars_mat <- guess_again(pars_mat)
      out <- add_reset(out, cur_iter, "could not calculate hessian")
      next
    }
    
    # Invert to get the negative of the covariance matrix
    inv_hes_mat <- try(solve(hessian_mat), silent = TRUE)
    if (inherits(inv_hes_mat, "try-error") || 
        any(is.na(inv_hes_mat))) {
      pars_mat <- guess_again(pars_mat)
      out <- add_reset(out, cur_iter, "could not invert hessian")
      next
    }
    
    pars_mat <- pars_mat_prev - inv_hes_mat %*% scores_mat

    if (has_converged(pars_mat, pars_mat_prev, tol)) {
      if (!is_maximum(hessian_mat)) {
        pars_mat <- guess_again(pars_mat)
        out <- add_reset(out, cur_iter, "converged not to maximum")
        next
      }
      pars <- pars_mat[, 1]
      names(pars) <- rownames(pars_mat)
      out[["found"]] <- pars
      covariance_mat <- -inv_hes_mat
      dimnames(covariance_mat) <- list(names(pars), names(pars))
      out[["cov"]] <- covariance_mat
      out[["last_iter"]] <- cur_iter
      break
    }
  }
  out
}

#' Gradient ascent algorithm
#'
#' @inheritParams newton_raphson
#'
#' @noRd
gradient_ascent <- function(y, x, pars_mat,
                            x_coeffs, max_iter, tol, 
                            seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  control <- 1
  low_inc_count <- 0
  out <- list(init_mat = pars_mat)
  for (cur_iter in 1:max_iter) {
    pars_mat_prev <- pars_mat
    exp_xb <- get_exp_Xb(y, x, pars_mat)
    scores <- get_scores(y, x, pars_mat, exp_xb)
    step <- scores / sum(abs(scores)) * control
    
    pars_mat <- pars_mat + step
    
    ll_diff <- sclr_log_likelihood(x = x, y = y, pars = pars_mat) -
      sclr_log_likelihood(x = x, y = y, pars = pars_mat_prev)
    
    if (ll_diff <= 0) {
      low_inc_count <- 0
      control <- control * 0.5
    } else if (ll_diff > 0 && ll_diff < 0.001) {
      low_inc_count <- low_inc_count + 1
    } else low_inc_count <- 0
    
    if (has_converged(pars_mat, pars_mat_prev, tol) ||
        low_inc_count / cur_iter >= 0.2) {
      pars <- pars_mat[, 1]
      names(pars) <- rownames(pars_mat)
      out[["found"]] <- pars
      x_coeffs <- get_x_coeffs(x)
      covariance_mat <- -solve(get_hessian(y, x, pars_mat, exp_xb, x_coeffs))
      dimnames(covariance_mat) <- list(names(pars), names(pars))
      out[["cov"]] <- covariance_mat
      out[["last_iter"]] <- cur_iter
      break
    }
  }
  out
}

#' Initial random parameter matrix
#'
#' @param y Model response.
#' @param x Model matrix.
#' @param conventional_names Controls parameter names.
#' @param seed Seed.
#'
#' @noRd
get_init_pars_mat <- function(y, x, conventional_names, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  pars_mat <- matrix(rnorm(ncol(x) + 1, 0, 2), ncol = 1)
  rownames(pars_mat) <- get_par_names(x, conventional_names)
  pars_mat
}

#' Create a new guess
#'
#' Creates a matrix with new parameter guesses.
#' The matrix has the same structure as the matrix with previous guesses.
#'
#' @param pars_mat The current matrix of parameter guesses.
#' 
#' @importFrom stats runif rnorm
#'
#' @noRd
guess_again <- function(pars_mat) {
  delta <- matrix(rnorm(nrow(pars_mat), mean = 0, sd = 2), ncol = 1)
  rownames(delta) <- rownames(pars_mat)
  delta
}

#' Check convergence
#'
#' @param pars_mat Current guess
#' @param pars_mat_prev Last guess
#' @param tol Tolerance
#'
#' @noRd
has_converged <- function(pars_mat, pars_mat_prev, tol) {
  deltas <- abs(pars_mat - pars_mat_prev)
  all(deltas < tol)
}

#' Check maximum
#'
#' @param hessian The second derivative matrix
#'
#' @noRd
is_maximum <- function(hessian) {
  eigenvals <- eigen(hessian, symmetric = TRUE, only.values = TRUE)$values
  all(eigenvals < 0)
}
