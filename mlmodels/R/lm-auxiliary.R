## Gradients by Observation ====================================================
#' @keywords internal
.ml_lm_gradientObs <- function(object)
{
  if (!inherits(object, "ml_lm"))
    cli::cli_abort("`object` must be a model of class 'ml_lm' (from ml_lm).",
                   call = NULL)
  
  b <- coef(object)
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  z <- as.matrix(object$model$scale$predictors)
  w <- if(is.null(object$model$weights))
    rep(1, nrow(x))
  else
    object$model$weights
  k1 <- ncol(x)
  k <- k1 + ncol(z)
  
  if (length(b) != k)
    cli::cli_abort("The length of the coefficients ({length(b)}) \\
                   does not match with the number of parameters ({k}).",
                   call = NULL)
  
  beta <- b[1:k1]
  delta <- b[(k1+1):k]
  
  xb <- x %*% cbind(beta)
  zd <- z %*% cbind(delta)
  s <- exp(zd)
  u <- y - xb
  
  gb <- w * as.vector(u / s^2) * x
  
  # Partial with respect to delta.
  gd <- w * as.vector((u / s)^2 - 1) * z
  
  return(cbind(gb,gd))
}

## Hessians by Observation =====================================================
#' @keywords internal
.ml_lm_hessianObs <- function(object)
{
  if (!inherits(object, "ml_lm"))
    cli::cli_abort("`object` must be a model of class 'ml_lm' (from ml_lm).",
                   call = NULL)
  
  b <- coef(object)
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  z <- as.matrix(object$model$scale$predictors)
  w <- if(is.null(object$model$weights))
    rep(1, nrow(x))
  else
    object$model$weights
  k1 <- ncol(x)
  k <- k1 + ncol(z)
  
  if (length(b) != k)
    cli::cli_abort("The length of the coefficients ({length(b)}) \\
                   does not match with the number of parameters ({k}).",
                   call = NULL)
  
  beta <- b[1:k1]
  delta <- b[(k1+1):k]
  
  xb <- x %*% cbind(beta)
  zd <- z %*% cbind(delta)
  s <- exp(zd)
  u <- y - xb
  
  s_bb <- as.vector(- w * s^(-2))
  s_bd <- as.vector(- 2 * w * (u / s^2))
  s_dd <- as.vector(- 2 * w * (u / s)^2)
  
  H_stacked <- matrix(0, nrow = nrow(x) * k, ncol = k)
  
  for(i in seq_len(nrow(x)))
  {
    # Extracting the elements for the observation we need.
    xi <- cbind(x[i, ])
    zi <- cbind(z[i, ])
    
    # Second partial with respect both times to beta.
    hbb <- s_bb[i] * tcrossprod(xi)
    
    # Second partial first with respect to beta and then to s
    hbs <- s_bd[i] * tcrossprod(xi,zi)
    
    # Transpose that.
    hsb <- t(hbs)
    
    # Second partial with respect both times to lnsigma.
    hss <- s_dd[i] * tcrossprod(zi)
    
    
    start_row <- (i - 1) * k + 1
    end_row <- i * k
    
    # Form the observation's Hessian
    H_stacked[start_row:end_row, ] <- rbind(cbind(hbb, hbs),
                                            cbind(hsb, hss))
  }
  
  # Stack all individual Hessians
  H_stacked
}

## Log-likelihood by observations ==============================================
#' @keywords internal
.ml_lm_loglikeObs <- function(object)
{
  if (!inherits(object, "ml_lm"))
    cli::cli_abort("`object` must be a model of class 'ml_lm' (from ml_lm).",
                   call = NULL)
  
  b <- coef(object)
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  z <- as.matrix(object$model$scale$predictors)
  w <- if(is.null(object$model$weights))
    rep(1, nrow(x))
  else
    object$model$weights
  k1 <- ncol(x)
  k <- k1 + ncol(z)
  
  if (length(b) != k)
    cli::cli_abort("The length of the coefficients ({length(b)}) \\
                   does not match with the number of parameters ({k}).",
                   call = NULL)
  
  beta <- b[1:k1]
  delta <- b[(k1+1):k]
  
  xb <- x %*% cbind(beta)
  zd <- z %*% cbind(delta)
  s <- exp(zd)
  u <- y - xb
  
  ll <- dnorm(u / s, log = TRUE) - zd
  if(object$model$log_info$value$is_log) ll <- ll - y # y because it's already log-transformed
  ll <- ll * w
  
  return(ll)
}

## ML EVALUATOR ================================================================
#' @keywords internal
.ml_lm_ll <- function(b, y, x, z, w = NULL, lognormal = FALSE)
{
  # The last coefficient in b is the coefficient for the natural log of sigma
  k1 <- ncol(x) # Number of coefficients for the mean.
  k <- k1 + ncol(z) # Total number of coefficients.

  # If we don't have weights we set them to ones to be able to use the same
  # formulas underneath.
  if(is.null(w))
    w <- rep(1, nrow(x))

  # Extract the coefficients for the mean.
  beta <- b[1:k1]
  delta <- b[(k1+1):k]

  # Useful operations
  xb <- x %*% cbind(beta)
  zd <- z %*% cbind(delta)
  s <- exp(zd)
  u <- y - xb

  ## LL
  ll <- dnorm(u / s, log = TRUE) - zd
  if(lognormal) ll <- ll - y # y because it's already log-transformed
  ll <- ll * w

  ## GRADIENT
  # Partial with respect to beta.
  gb <- w * as.vector(u / s^2) * x

  # Partial with respect to delta.
  gd <- w * as.vector((u / s)^2 - 1) * z

  ## HESSIAN

  s_bb <- as.vector(- w * s^(-2))
  s_bd <- as.vector(- 2 * w * (u / s^2))
  s_dd <- as.vector(- 2 * w * (u / s)^2)
  
  H_bb <- crossprod(x * s_bb, x)
  H_bd <- crossprod(x * s_bd, z)
  H_dd <- crossprod(z * s_dd, z)
  
  H <- rbind(cbind(H_bb, H_bd),
             cbind(t(H_bd), H_dd))

  # Set the attribute in ll to pass it back to maxLik
  attr(ll, "gradient") <- cbind(gb, gd)
  
  # Set the attribute in ll to pass it back to maxLik
  attr(ll, "hessian") <- H

  return(ll)
}

## VARIANCE HELPERS ============================================================
# --- 1. vcov_boot -------------------------------------------------------------
#' @keywords internal
.vcov_boot.ml_lm <- function(object,
                             repetitions = 999,
                             seed = NULL,
                             cl_var = NULL,
                             progress = TRUE,
                             ...)
{
  
  # --- 1.0. Validity Checks ---------------------------------------------------
  if(!inherits(object, "ml_lm"))
    cli::cli_abort("`object` needs to be of class 'ml_lm'.")

  if (is.null(seed)) seed <- sample.int(1e6, 1)
  set.seed(seed)

  if (is.null(object$model$value$outcomes) ||
      is.null(object$model$value$predictors) ||
      is.null(object$model$scale$predictors))
    cli::cli_abort("The sample data was not stored properly.")

  # --- 1.1. Sample data extraction. -------------------------------------------
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  z <- as.matrix(object$model$scale$predictors)
  n <- nrow(x)
  if(is.null(object$model$weights))
    w <- rep(1,n)
  else
    w <- object$model$weights

  is_clustered <- !is.null(cl_var)
  if (is_clustered) {
    cluster_ids <- unique(cl_var[object$model$sample])
    n_cluster   <- length(cluster_ids)
  }
  
  # --- 1.2. Bootstrap area ----------------------------------------------------
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

  success     <- logical(repetitions)
  coef_matrix <- matrix(NA_real_, nrow = repetitions, ncol = length(coef(object)))

  if (!is.null(object$model$constraints$maxLik)) {
    cli::cli_warn(
      c("Bootstrap variance with constraints may be unreliable.",
        "i" = "Different bootstrap samples often produce infeasible log-likelihoods at the supplied starting values.",
        "i" = "Equality constraints in particular lead to very low convergence rates.",
        "i" = "Consider using `type = 'robust'` or `type = 'cluster'` (with `cl_var`) instead.")
    )
  }
  
  # --- 1.2.1 Bootstrap loop ---------------------------------------------------
  for (i in seq_len(repetitions)) {
    if (progress && i %% 50 == 1 && i > 1) cat("\n ")
    else if(progress && i == 1) cat(" ")

    tryCatch({
      if (is_clustered) {
        sampled_clusters <- sample(cluster_ids, size = n_cluster, replace = TRUE)
        boot_idx <- unlist(lapply(sampled_clusters, function(cid) {
          which(cl_var[object$model$sample] == cid)
        }))
      } else {
        boot_idx <- sample(n, n, replace = TRUE)
      }
      
      y_boot <- y[boot_idx]
      x_boot <- x[boot_idx, , drop = FALSE]
      z_boot <- z[boot_idx, , drop = FALSE]
      w_boot <- w[boot_idx]
      
      suppressMessages({
        updated <- .ml_lm.fit(y = y_boot,
                              x = x_boot,
                              z = z_boot,
                              w = w_boot,
                              lognormal = object$model$log_info$value$is_log,
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

  # --- 1.3. Final reporting ---------------------------------------------------
  if (progress) {
    cat("\n")
    cli::cli_text("Bootstrapping finished - {round(mean(success) * 100, 1)}% of replications converged.")
  }
  
  if (mean(success) < 0.7) {
    cli::cli_warn("Low convergence rate - bootstrap results may be unreliable.")
  }

  # Variance from successful replications only
  valid_rows <- complete.cases(coef_matrix)
  vcov_boot  <- var(coef_matrix[valid_rows, , drop = FALSE])
  
  attr(vcov_boot, "repetitions") <- repetitions
  attr(vcov_boot, "n_success") <- sum(success)
  attr(vcov_boot, "n_failure") <- repetitions - sum(success)
  attr(vcov_boot, "success_rate") <- mean(success) * 100

  dimnames(vcov_boot) <- list(names(coef(object)), names(coef(object)))
  vcov_boot
}

# --- 2. vcov_jack -------------------------------------------------------------
#' @keywords internal
.vcov_jack.ml_lm <- function(object,
                             cl_var = NULL,
                             progress = TRUE,
                             ...)
{
  if(!inherits(object, "ml_lm"))
    cli::cli_abort("`object` needs to be of class 'ml_lm'.")
  
  if (is.null(object$model$value$outcomes) ||
      is.null(object$model$value$predictors) ||
      is.null(object$model$scale$predictors))
    cli::cli_abort("The sample data was not stored properly.")
  
  # --- 2.1. Sample data extraction. -------------------------------------------
  y <- object$model$value$outcomes[[1]]
  x <- as.matrix(object$model$value$predictors)
  z <- as.matrix(object$model$scale$predictors)
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
        z_jack <- z[keep_idx, , drop = FALSE]
        w_jack <- w[keep_idx]
      } else {
        y_jack <- y[-i]
        x_jack <- x[-i, , drop = FALSE]
        z_jack <- z[-i, , drop = FALSE]
        w_jack <- w[-i]
      }
      
      suppressMessages({
        updated <- .ml_lm.fit(y = y_jack,
                              x = x_jack,
                              z = z_jack,
                              w = w_jack,
                              lognormal = object$model$log_info$value$is_log,
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
