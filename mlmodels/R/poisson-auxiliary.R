## Gradients by Observation ====================================================
#' @keywords internal
.ml_poisson_gradientObs <- function(object)
{
  if (!inherits(object, "ml_poisson"))
    cli::cli_abort("`object` must be a model of class 'ml_poisson' (from ml_poisson).",
                   call = NULL)
  
  b <- coef(object)
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  w <- object$model$weights %||% rep(1, length(y))   # default to 1 if NULL
  
  k <- length(b)
  
  xb <- x %*% cbind(b)
  mu <- exp(xb)
  
  g <- as.vector(w * (y - mu)) * x
  
  return(g)
}

## Hessians by Observation =====================================================
#' @keywords internal
.ml_poisson_hessianObs <- function(object)
{
  if (!inherits(object, "ml_poisson"))
    cli::cli_abort("`object` must be a model of class 'ml_poisson' (from ml_poisson).",
                   call = NULL)
  
  b <- coef(object)
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  w <- object$model$weights %||% rep(1, length(y))   # default to 1 if NULL
  
  k <- length(b)
  
  xb <- x %*% cbind(b)
  mu <- exp(xb)
  
  s <- as.vector(- mu * w)
  
  H_stacked <- matrix(0, nrow = nrow(x) * k, ncol = k)
  
  for (i in seq_len(nrow(x))) {
    xi  <- cbind(x[i, ])
    
    start_row <- (i - 1) * k + 1
    end_row <- i * k
    
    H_stacked[start_row:end_row, ] <- s[i] * tcrossprod(xi)
  }
  
  return(H_stacked)
}

## Log-likelihood by Observation ===============================================
#' @keywords internal
.ml_poisson_loglikeObs <- function(object)
{
  if (!inherits(object, "ml_poisson"))
    cli::cli_abort("`object` must be a model of class 'ml_poisson' (from ml_poisson).",
                   call = NULL)
  
  b <- coef(object)
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  w <- object$model$weights %||% rep(1, length(y))   # default to 1 if NULL
  
  k <- length(b)
  
  xb <- x %*% cbind(b)
  mu <- exp(xb)
  
  ll <- w * (y * xb - mu - lfactorial(y))
  
  return(ll)
}

## ML EVALUATOR ================================================================
#' @keywords internal
.ml_poisson_ll <- function(b, y, x, w = NULL)
{
  if(is.null(w))
    w <- rep(1, nrow(x))
  
  xb <- x %*% cbind(b)
  eta <- pmin(pmax(xb, -100), 100)
  mu <- exp(eta)
  
  # Log-likelihood
  ll <- w * (y * eta - mu - lfactorial(y))
  
  # gradient
  g <- as.vector(w * (y - mu)) * x
  
  # Hessian
  s <- as.vector(- mu * w)
  H <- crossprod(x * s, x)
  
  # Attach gradient and Hessian as attributes
  attr(ll, "gradient") <- g
  attr(ll, "hessian")  <- H
  
  return(ll)
}

## VCOV HELPERS ================================================================
# --- 1. vcov_boot -------------------------------------------------------------
#' @keywords internal
.vcov_boot.ml_poisson <- function(object,
                                  repetitions = 999,
                                  seed = NULL,
                                  cl_var = NULL,
                                  progress = TRUE,
                                  ...)
{
  # --- 0. Validity Checks -----------------------------------------------------
  if(!inherits(object, "ml_poisson"))
    cli::cli_abort("`object` needs to be of class 'ml_poisson'.")
  
  if (is.null(seed)) seed <- sample.int(1e6, 1)
  set.seed(seed)
  
  if (is.null(object$model$value$outcomes) ||
      is.null(object$model$value$predictors))
    cli::cli_abort("The sample data was not stored properly.")
  
  # --- 1. Sample data extraction. ---------------------------------------------
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  n <- nrow(x)
  if(is.null(object$model$weights))
    w <- rep(1,n)
  else
    w <- object$model$weights
  
  # Prepare for clustered bootstrap if requested
  is_clustered <- !is.null(cl_var)
  if (is_clustered) {
    cluster_ids <- unique(cl_var[object$model$sample])
    n_cluster <- length(cluster_ids)
  }
  
  # --- 2. Bootstrap area ------------------------------------------------------
  if (progress) {
    if (is_clustered) {
      cli::cli_alert_info("Clustered bootstrap with {.val {repetitions}} repetitions and {.val {n_cluster}} clusters.")
    } else {
      cli::cli_alert_info("Bootstrap with {.val {repetitions}} repetitions.")
    }
    cat(cli::col_blue(" 0"))
    for (i in seq(10, 50, by = 10)) cat(cli::col_blue(sprintf("%10d", i)))
    cat("\n")
    cat(cli::col_blue(strrep("=", 52), "\n"))
  }
  
  # Storage
  coef_matrix <- matrix(NA_real_, nrow = repetitions, ncol = length(coef(object)))
  success <- logical(repetitions)
  
  if (!is.null(object$model$constraints$maxLik)) {
    cli::cli_warn(
      c("Bootstrap variance with constraints may be unreliable.",
        "i" = "Different bootstrap samples often produce infeasible log-likelihoods at the supplied starting values.",
        "i" = "Equality constraints in particular lead to very low convergence rates.",
        "i" = "Consider using `type = 'robust'` or `type = 'cluster'` (with `cl_var`) instead.")
    )
  }
  
  # --- 2.1 Bootstrap loop -----------------------------------------------------
  for (i in seq_len(repetitions)) {
    if (progress && i %% 50 == 1 && i > 1) cat("\n ")
    else if(progress && i == 1) cat(" ")
    
    tryCatch({
      # Draw bootstrap sample
      if (is_clustered) {
        boot_idx <- sample(seq_len(n_cluster), n_cluster, replace = TRUE)
        boot_clusters <- cluster_ids[boot_idx]
        boot_idx <- which(cl_var[object$model$sample] %in% boot_clusters)
      } else {
        boot_idx <- sample(n, n, replace = TRUE)
      }
      
      y_boot <- y[boot_idx]
      x_boot <- x[boot_idx, , drop = FALSE]
      
      w_boot <- w[boot_idx]
      
      suppressMessages({
        # Fit on bootstrap sample using internal fit function
        updated <- .ml_poisson.fit(y = y_boot,
                                   x = x_boot,
                                   w = w_boot,
                                   constraints = object$model$constraints$maxLik,
                                   start       = object$model$start,
                                   method      = object$model$method,
                                   control     = object$model$control)
      })
      
      if (updated$code %in% c(0L, 1L, 2L, 8L)) {
        coef_matrix[i, ] <- coef(updated)
        success[i] <- TRUE
        if (progress) cat(cli::col_green("."))
      } else {
        success[i] <- FALSE
        if (progress) cat(cli::col_red("x"))
      }
    }, error = function(e) {
      success[i] <- FALSE
      if (progress) cat(cli::col_red("x"))
      print(e)
    })
  }
  
  if (progress) {
    cat("\n")
    cat(cli::col_blue(strrep("=", 52), "\n"))
  }
  
  # --- 3. Final reporting -----------------------------------------------------
  if (progress) {
    cat("\n")
    cli::cli_text("Bootstrapping finished - {round(mean(success) * 100, 1)}% of replications converged.")
  }
  
  if (mean(success) < 0.7) {
    cli::cli_warn("Low convergence rate in bootstrap - results may be unreliable.")
  }
  
  # Compute variance from successful replications only
  valid_rows <- complete.cases(coef_matrix)
  vcov_boot <- var(coef_matrix[valid_rows, , drop = FALSE])
  
  attr(vcov_boot, "repetitions") <- repetitions
  attr(vcov_boot, "n_success") <- sum(success)
  attr(vcov_boot, "n_failure") <- repetitions - sum(success)
  attr(vcov_boot, "success_rate") <- mean(success) * 100
  
  dimnames(vcov_boot) <- list(names(coef(object)), names(coef(object)))
  vcov_boot
}

# --- 2. vcov_jack -------------------------------------------------------------
#' @keywords internal
.vcov_jack.ml_poisson <- function(object,
                                  cl_var = NULL,
                                  progress = TRUE,
                                  ...)
{
  if(!inherits(object, "ml_poisson"))
    cli::cli_abort("`object` needs to be of class 'ml_poisson'.")
  
  if (is.null(object$model$value$outcomes) ||
      is.null(object$model$value$predictors))
    cli::cli_abort("The sample data was not stored properly.")
  
    # --- 2.1. Sample data extraction. -------------------------------------------
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  n_obs <- nrow(x)
  if(is.null(object$model$weights))
    w <- rep(1,n_obs)
  else
    w <- object$model$weights
  
  # --- 2.2. Prepare for clustered jackknife if requested ----------------------
  is_clustered <- !is.null(cl_var)
  if (is_clustered) {
    cl_var <- cl_var[object$model$sample]
    cluster_ids <- unique(cl_var)
    n_cluster   <- length(cluster_ids)
    n_jack <- n_cluster
  }
  else
    n_jack <- n_obs
  
  # --- 2.3. Jackknife loop ----------------------------------------------------
  if (progress) {
    if (is_clustered) {
      cli::cli_alert_info("Clustered jackknife variance with {.val {n_jack}} clusters.")
    } else {
      cli::cli_alert_info("Jackknife variance.")
    }
    cat(cli::col_blue(" 0"))
    for (i in seq(10, 50, by = 10)) cat(cli::col_blue(sprintf("%10d", i)))
    cat("\n")
    cat(cli::col_blue(strrep("=", 52), "\n"))
  }
  
  success     <- logical(n_jack)
  coef_matrix <- matrix(NA_real_, nrow = n_jack, ncol = length(coef(object)))
  
  for (i in seq_len(n_jack)) {
    if (progress && i %% 50 == 1 && i > 1) cat("\n ")
    else if(progress && i == 1) cat(" ")
    
    tryCatch({
      if (is_clustered) {
        # We are looping through cluster ids, so we have to leave out the current
        # cluster from the data.
        keep_idx <- cl_var != cluster_ids[i]
        
        y_jack <- y[keep_idx]
        x_jack <- x[keep_idx, , drop = FALSE]
        w_jack <- w[keep_idx]
      } else {
        y_jack <- y[-i]
        x_jack <- x[-i, , drop = FALSE]
        w_jack <- w[-i]
      }
      
      suppressMessages({
        updated <- .ml_poisson.fit(y = y_jack,
                                   x = x_jack,
                                   w = w_jack,
                                   constraints = object$model$constraints$maxLik,
                                   start       = object$model$start,
                                   method      = object$model$method,
                                   control     = object$model$control)
      })
      
      if (updated$code %in% c(0L, 1L, 2L, 8L)) {
        if (progress) cat(cli::col_green("."))
        success[i] <- TRUE
        coef_matrix[i, ] <- coef(updated)
      } else {
        if (progress) cat(cli::col_red("x"))
        success[i] <- FALSE
        coef_matrix[i, ] <- NA_real_
      }
    }, error = function(e) {
      if (progress) cat(cli::col_red("x"))
      success[i] <- FALSE
      coef_matrix[i, ] <- NA_real_
    })
  }
  
  if (progress) {
    cat("\n")
    cat(cli::col_blue(strrep("=", 52), "\n"))
  }
  
  # --- 2.4. Reporting ---------------------------------------------------------
  success_rate <- mean(success) * 100
  if(progress && success_rate == 100)
  {
    cli::cli_text("Jackknife finished - {.val {round(success_rate, 1)}}% of replications converged.")
  }
  
  valid_rows <- complete.cases(coef_matrix)
  valid_coef <- coef_matrix[valid_rows, , drop = FALSE]
  n_valid    <- nrow(valid_coef)
  
  if (n_valid == 0) {
    cli::cli_abort("All jackknife replications failed.")
  }
  
  if (n_valid < n_jack) {
    cli::cli_warn(
      "Jackknife variance computed from only {.val {n_valid}} out of {.val {n_jack}} successful replications ({.val {round(success_rate, 1)}}%)."
    )
  }
  
  # --- 2.5. Calculation -------------------------------------------------------
  theta_bar <- colMeans(valid_coef)
  centered  <- sweep(valid_coef, 2, theta_bar, FUN = "-")
  vcov_jack <- (n_valid - 1) / n_valid * crossprod(centered)
  
  attr(vcov_jack, "n_success") <- n_valid
  attr(vcov_jack, "success_rate") <- n_valid / n_jack * 100
  
  dimnames(vcov_jack) <- list(names(coef(object)), names(coef(object)))
  vcov_jack
}
