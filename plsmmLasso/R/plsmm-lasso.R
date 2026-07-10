calc_criterion <- function(crit, lasso_output, log_lik, nonpara = FALSE) {
  n <- nrow(lasso_output$x_fit)
  k <- sum(lasso_output$theta != 0)
  d <- length(lasso_output$theta)

  if (nonpara == TRUE) {
    k <- k + length(lasso_output$selected_functions)
    d <- length(lasso_output$theta) + length(as.vector(lasso_output$alpha))
  }

  if (crit == "BIC") {
    return(-2 * log_lik + log(n) * k)
  }
  if (crit == "BICC") {
    return(-2 * log_lik + max(1, log(log(d))) * log(n) * k)
  }
  if (crit == "EBIC") {
    return(-2 * log_lik + (log(n) + 2 * log(d)) * k)
  }
}

init_params <- function(y, series) {
  y_series_means <- tapply(y,
    list(series = as.factor(series)),
    FUN = function(x) mean(x, na.rm = TRUE)
  )

  su <- stats::var(c(y_series_means), na.rm = TRUE)
  se <- stats::var(y, na.rm = TRUE) - su
  sr <- se / su
  return(list(sr = sr, se = se))
}

E_step <- function(x, y, series, f_fit, sr, ni, theta) {
  x <- cbind(1, x)
  res <- data.frame(
    series = series,
    resid = (y - f_fit - x %*% theta)
  )

  phi <- tapply(res$resid,
    list(series = as.factor(res$series)),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) / (ni + sr)

  df_phi <- data.frame(
    series = unique(series),
    phi = c(t(phi))
  )

  return(df_phi)
}

M_step_standard_error <- function(x, y, f_fit, sr, se, phi, ni, theta) {
  x <- cbind(1, x)
  n <- length(y)

  rep_phi <- rep(phi, ni)

  se <- sum((y - f_fit - x %*% theta - rep_phi)^2, na.rm = TRUE) + sum((ni * se) / (ni + sr))
  se <- se / n

  se[is.na(se) | se == Inf] <- 0

  return(se = se)
}

M_step_random_effects <- function(series, sr, se, phi, ni) {
  N <- length(unique(series))
  su <- sum((phi)^2, na.rm = T) + sum(se / (ni + sr), na.rm = TRUE)
  su <- su / N

  su[is.na(se) | se == Inf] <- 0
  return(su = su)
}

offset_random_effects <- function(y, phi, ni) {
  rep_phi <- rep(phi, ni)
  y <- y - rep_phi
  return(y)
}

joint_lasso <- function(x, y, t, name_group_var, bases, se, gamma,
                        lambda, pre_D, timexgroup) {
  x_stand <- scale(x, scale = TRUE)

  x_mean <- attr(x_stand, "scaled:center")
  x_sd <- attr(x_stand, "scaled:scale")

  x <- cbind(1, x)
  x_stand <- cbind(1, x_stand)

  combined_x_bases <- cbind(x_stand, bases)

  p <- ncol(x_stand)
  M <- ncol(bases)
  pM <- p + M

  D <- diag(1, nrow = pM, ncol = pM)
  
  if(!timexgroup | is.null(name_group_var)) {
    D[(p + 1):pM, (p + 1):pM] <- pre_D * (sqrt(se * gamma * log(M)) / lambda)
  } else {
    D[(p + 1):pM, (p + 1):pM] <- pre_D * (sqrt(se * gamma * log(M / 2)) / lambda)
  }

  D_inv <- D
  diag(D_inv) <- 1 / diag(D_inv)

  combined_x_bases_lasso <- combined_x_bases %*% D_inv

  y_stand <- scale(y)
  y_mean <- attr(y_stand, "scaled:center")
  y_sd <- attr(y_stand, "scaled:scale")



  coef_joint_lasso <- as.vector(stats::coef(glmnet::glmnet(combined_x_bases_lasso[, -1],
    y_stand,
    alpha = 1, lambda = lambda,
    standardize = FALSE,
    intercept = TRUE
  )))


  coef_joint_lasso[1] <- (coef_joint_lasso[1] - sum((x_mean / x_sd) * coef_joint_lasso[2:p])) * y_sd + y_mean

  coef_joint_lasso <- D_inv %*% coef_joint_lasso
  coef_joint_lasso[-1] <- coef_joint_lasso[-1] * y_sd
  coef_joint_lasso[2:p, 1] <- coef_joint_lasso[2:p, 1] / x_sd

  theta <- coef_joint_lasso[1:p, 1]
  names(theta) <- colnames(x)
  names(theta)[1] <- "Intercept"

  alpha <- coef_joint_lasso[(p + 1):nrow(coef_joint_lasso), 1]

  f_fit <- bases %*% alpha

  out_f <- data.frame(
    t = t,
    f_fit = f_fit,
    group = x[, name_group_var]
  )

  x_fit <- x %*% theta

  selected_functions <- which(alpha != 0)

  return(list(
    out_f = out_f, selected_functions = selected_functions, alpha = alpha,
    theta = theta, x_fit = x_fit
  ))
}

#' Fit a high-dimensional PLSMM
#'
#' Fits a partial linear semiparametric mixed effects model (PLSMM) via penalized maximum likelihood.
#'
#' @param x A matrix of predictor variables.
#' @param y A continuous vector of response variable.
#' @param series A variable representing different series or groups in the data modeled as a random intercept.
#' @param t A numeric vector indicating the timepoints.
#' @param name_group_var A character string specifying the name of the grouping variable in the \code{x} matrix.
#' @param bases A matrix of bases functions.
#' @param gamma The regularization parameter for the nonlinear effect of time.
#' @param lambda The regularization parameter for the fixed effects.
#' @param timexgroup Logical indicating whether to use a time-by-group interaction.
#'                   If \code{TRUE}, each group in \code{name_group_var} will have its own estimate of the time effect.
#' @param criterion The information criterion to be computed. Options are "BIC", "BICC", or "EBIC".
#' @param nonpara Logical. If TRUE, the \code{criterion} is computed using both the coefficients of the fixed-effects and the coefficients of the nonlinear function. If FALSE, only the coefficients of the fixed-effects are used.
#' @param cvg_tol Convergence tolerance for the algorithm.
#' @param max_iter Maximum number of iterations allowed for convergence.
#' @param verbose Logical indicating whether to print convergence details at each iteration. Default is \code{FALSE}.
#'
#' @return A list containing the following components:
#'   \item{lasso_output}{A list with the fitted values for the fixed effect and nonlinear effect. The estimated coeffcients for the fixed effects and nonlinear effect. The indices of the used bases functions.}
#'   \item{se}{Estimated standard deviation of the residuals.}
#'   \item{su}{Estimated standard deviation of the random intercept.}
#'   \item{out_phi}{Data frame containing the estimated individual random intercept.}
#'   \item{ni}{Number of timepoitns per observations.}
#'   \item{hyperparameters}{Data frame with lambda and gamma values.}
#'   \item{converged}{Logical indicating if the algorithm converged.}
#'   \item{crit}{Value of the selected information criterion.}
#'
#' @details
#' This function fits a PLSMM with a lasso penalty on the fixed effects
#' and the coefficient associated with the bases functions. It uses the Expectation-Maximization (EM) algorithm
#' for estimation. The bases functions represent a nonlinear effect of time.
#'
#' The model includes a random intercept for each level of the variable specified by \code{series}. Additionally, if \code{timexgroup} is
#' set to \code{TRUE}, the model includes a time-by-group interaction, allowing each group of \code{name_group_var} to have its own estimate
#' of the nonlinear function, which can capture group-specific nonlinearities over time. If \code{name_group_var} is set to \code{NULL} only
#' one nonlinear function for the whole data is being used 
#'
#' The algorithm iteratively updates the estimates until convergence or until the maximum number of iterations is reached.
#'
#' @examples
#' 
#' set.seed(123)
#' data_sim <- simulate_group_inter(
#'   N = 50, n_mvnorm = 3, grouped = TRUE,
#'   timepoints = 3:5, nonpara_inter = TRUE,
#'   sample_from = seq(0, 52, 13), 
#'   cos = FALSE, A_vec = c(1, 1.5)
#' )
#' sim <- data_sim$sim
#' x <- as.matrix(sim[, -1:-3])
#' y <- sim$y
#' series <- sim$series
#' t <- sim$t
#' bases <- create_bases(t)
#' lambda <- 0.0046
#' gamma <- 0.00000001
#' plsmm_output <- plsmm_lasso(x, y, series, t,
#'   name_group_var = "group", bases$bases,
#'   gamma = gamma, lambda = lambda, timexgroup = TRUE,
#'   criterion = "BIC"
#' )
#' # fixed effect coefficients
#' plsmm_output$lasso_output$theta
#' 
#' # fixed effect fitted values
#' plsmm_output$lasso_output$x_fit
#' 
#' # nonlinear functions coefficients
#' plsmm_output$lasso_output$alpha
#'
#'# nonlinear functions fitted values
#'plsmm_output$lasso_output$out_f
#'
#' # standard deviation of residuals
#' plsmm_output$se
#' 
#' # standard deviation of random intercept
#' plsmm_output$su
#' 
#' # series specific random intercept
#' plsmm_output$out_phi
#' @export
plsmm_lasso <- function(x, y, series, t, name_group_var = NULL, bases,
                       gamma, lambda, timexgroup, criterion, nonpara = FALSE,
                       cvg_tol = 0.001, max_iter = 100, verbose = FALSE) {
  # Check if x is a matrix
  if (!is.matrix(x)) {
    stop("Argument 'x' must be a matrix.")
  }

  # Check if y is a numerical vector
  if (!is.numeric(y)) {
    stop("Argument 'y' must be a numerical vector.")
  }

  # Check if t is a numerical vector
  if (!is.numeric(t)) {
    stop("Argument 't' must be a numerical vector.")
  }

  if(!is.null(name_group_var)) {
    # Check if name_group_var is a character
    if (!is.character(name_group_var)) {
      stop("Argument 'name_group_var' must be a character.")
    }
    
    # Check if name_group_var is present in column names of x
    if (!(name_group_var %in% colnames(x))) {
      stop("The variable specified in 'name_group_var' is not present as a column name in 'x'.")
    }
    
    # Check if x[, name_group_var] is a 0,1 binary vector
    if (any(x[, name_group_var] != 0 & x[, name_group_var] != 1)) {
      stop("The column specified by 'name_group_var' in 'x' must be a 0,1 binary vector.")
    } 
  }

  # Check if bases is a matrix
  if (!is.matrix(bases)) {
    stop("Argument 'bases' must be a matrix.")
  }

  if(is.null(name_group_var) & timexgroup) {
    warning("timexgroup has been set to FALSE. timexgroup cannot be TRUE if name_group_var is not provided.")
    timexgroup = FALSE
  }
  
  ni <- as.vector(table(series))

  if (timexgroup) {
    n <- length(y)
    vec_group <- x[, name_group_var]
    ref_group <- vec_group[1]
    M <- ncol(bases)

    index_ref_group <- vec_group == ref_group

    bases_timexgroup <- matrix(0, nrow = n, ncol = M * 2)

    bases_timexgroup[index_ref_group, 1:M] <- bases[index_ref_group, ]
    bases_timexgroup[!index_ref_group, (M + 1):(2 * M)] <- bases[!index_ref_group, ]

    bases <- bases_timexgroup

    bases <- bases_timexgroup
  }

  pre_D <- diag(sqrt(apply(bases^2, 2, sum)))

  ## Initialization
  out_init <- init_params(y = y, series = series)
  sr <- out_init$sr
  se <- out_init$se

  theta <- rep(0, ncol(x) + 1)

  lasso_init <- joint_lasso(
    x = x, y = y, t = t, name_group_var = name_group_var,
    bases = bases, se = se, gamma = gamma, lambda = lambda,
    pre_D = pre_D, timexgroup = timexgroup
  )

  out_f <- lasso_init$out_f
  theta <- lasso_init$theta

  max_iter <- max_iter
  cvg_crit <- Inf
  Iter <- 0
  while ((cvg_crit > cvg_tol) & (Iter < max_iter)) {
    Iter <- Iter + 1

    f_fit <- out_f$f_fit

    out_E <- E_step(
      x = x, y = y, series = series, f_fit = f_fit, sr = sr, ni = ni,
      theta = theta
    )
    # here
    phi_tmp <- out_E$phi
    se_tmp <- M_step_standard_error(
      x = x, y = y, f_fit = f_fit, sr = sr, se = se,
      phi = phi_tmp, ni = ni, theta = theta
    )

    su_tmp <- M_step_random_effects(
      series = series, sr = sr, se = se, phi = phi_tmp,
      ni = ni
    )

    sr_tmp <- se_tmp / su_tmp
    y_offset <- offset_random_effects(y = y, phi = phi_tmp, ni = ni)

    lasso_output <- joint_lasso(
      x = x, y = y_offset, t = t, name_group_var = name_group_var,
      bases = bases, se = se, gamma = gamma, lambda = lambda,
      pre_D = pre_D, timexgroup = timexgroup
    )

    out_f_tmp <- lasso_output$out_f
    theta_tmp <- lasso_output$theta



    delta_f <- 0
    delta_theta <- 0
    delta_se <- 0
    delta_su <- 0

    if (Iter == 2) {
      t2 <- c(out_f$f_fit, se, se / sr, theta)
    }
    if (Iter == 3) {
      t1 <- c(out_f$f_fit, se, se / sr, theta)
      t0 <- c(out_f_tmp$f_fit, se_tmp, se_tmp / sr_tmp, theta_tmp)
      tp0 <- (t2 - t1) / sum(((t2 - t1)^2)) + (t0 - t1) / sum(((t0 - t1)^2))
      tp0 <- t1 + tp0 / sum(tp0^2)
    }
    if (Iter > 3) {
      t2 <- t1
      t1 <- t0
      t0 <- c(out_f_tmp$f_fit, se_tmp, se_tmp / sr_tmp, theta_tmp)
      tp1 <- tp0
      tp0 <- (t2 - t1) / sum(((t2 - t1)^2)) + (t0 - t1) / sum(((t0 - t1)^2))
      tp0 <- t1 + tp0 / sum(tp0^2)
      cvg_crit <- sum((tp0 - tp1)^2)

      delta_f <- sum((out_f$f_fit - out_f_tmp$f_fit)^2)
      delta_theta <- sum((theta - theta_tmp)^2)
      delta_se <- sum((se - se_tmp)^2)
      delta_su <- sum((se / sr - se_tmp / sr_tmp)^2)
    }

    # Update
    se <- se_tmp
    su <- su_tmp
    out_f <- out_f_tmp
    sr <- sr_tmp
    theta <- theta_tmp

    if (verbose) {
      cat(
        "Iter ", Iter, "conv_crit", cvg_crit, "\n", 
        "param_conv:", "f", delta_f, "theta",
        delta_theta, "se", delta_se, "su", delta_su, "\n"
      )
    }
  }

  if(is.null(name_group_var)) {
    f_mean = mean(unique(lasso_output$out_f$f_fit))
    lasso_output$out_f$f_fit <- lasso_output$out_f$f_fit - f_mean
    lasso_output$theta["Intercept"] <- lasso_output$theta["Intercept"] + f_mean
    lasso_output$x_fit <- as.matrix(cbind(1, x)) %*% lasso_output$theta
  } else {
    group_0 <- lasso_output$out_f$group == 0
    
    f0_mean <- attr(scale(unique(lasso_output$out_f[group_0, ]$f_fit),
                          scale = FALSE
    ), "scaled:center")
    f1_mean <- attr(scale(unique(lasso_output$out_f[!group_0, ]$f_fit),
                          scale = FALSE
    ), "scaled:center")
    
    lasso_output$out_f[group_0, ]$f_fit <- lasso_output$out_f[group_0, ]$f_fit - f0_mean
    lasso_output$out_f[!group_0, ]$f_fit <- lasso_output$out_f[!group_0, ]$f_fit - f1_mean
    
    lasso_output$theta[name_group_var] <- lasso_output$theta[name_group_var] + (f1_mean - f0_mean)
    lasso_output$theta["Intercept"] <- lasso_output$theta["Intercept"] + f0_mean
    lasso_output$x_fit <- as.matrix(cbind(1, x)) %*% lasso_output$theta
  }
  

  hyperparameters <- data.frame(lambda = lambda, gamma = gamma)
  converged <- ifelse(Iter >= max_iter, FALSE, TRUE)

  Z <- stats::model.matrix(~ 0 + factor(series))
  logLik <- mvtnorm::dmvnorm(
    x = y,
    mean = as.vector(lasso_output$x_fit) + lasso_output$out_f$f_fit,
    sigma = diag(nrow(Z)) * se + su * Z %*% t(Z), log = TRUE
  )

  ic <- calc_criterion(
    crit = criterion, lasso_output = lasso_output,
    log_lik = logLik, nonpara = nonpara
  )

  return(list(
    lasso_output = lasso_output, se = se, su = su, out_phi = out_E, ni = ni,
    hyperparameters = hyperparameters, converged = converged, crit = ic
  ))
}