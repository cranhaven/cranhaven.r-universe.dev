# Modularized implementation of the GKw family fitting functions
# This file contains the main gkwfit function and all its helper functions

#' Convert family string to numeric code for TMB
#'
#' @param family Character string specifying the family.
#' @return Integer code for TMB. 0="gkw", 1="bkw", 2="kkw", 3="ekw", 4="mc", 5="kw", 6="beta"
#' @keywords internal
.family_to_code <- function(family) {
  codes <- c(gkw = 0, bkw = 1, kkw = 2, ekw = 3, mc = 4, kw = 5, beta = 6)
  return(codes[family])
}

#' Get family parameter information
#'
#' @param family Character string specifying the family.
#' @return List with parameter names and count.
#' @keywords internal
.get_family_param_info <- function(family) {
  family_params <- list(
    gkw = list(names = c("alpha", "beta", "gamma", "delta", "lambda"), n = 5),
    bkw = list(names = c("alpha", "beta", "gamma", "delta"), n = 4),
    kkw = list(names = c("alpha", "beta", "delta", "lambda"), n = 4),
    ekw = list(names = c("alpha", "beta", "lambda"), n = 3),
    mc = list(names = c("gamma", "delta", "lambda"), n = 3),
    kw = list(names = c("alpha", "beta"), n = 2),
    beta = list(names = c("gamma", "delta"), n = 2)
  )

  return(family_params[[family]])
}

#' Get default fixed parameters for a family
#'
#' @param family Character string specifying the family.
#' @return Named list of fixed parameters.
#' @keywords internal
.get_default_fixed <- function(family) {
  family_fixed <- list()

  if (family != "gkw") {
    # Set fixed parameters based on family
    if (family == "bkw") {
      family_fixed$lambda <- 1 # BKw: lambda = 1 fixed
    } else if (family == "kkw") {
      family_fixed$gamma <- 1 # KKw: gamma = 1 fixed
    } else if (family == "ekw") {
      family_fixed$gamma <- 1 # EKw: gamma = 1, delta = 0 fixed
      family_fixed$delta <- 0
    } else if (family == "mc") {
      family_fixed$alpha <- 1 # Mc: alpha = 1, beta = 1 fixed
      family_fixed$beta <- 1
    } else if (family == "kw") {
      family_fixed$gamma <- 1 # Kw: gamma = 1, delta = 0, lambda = 1 fixed
      family_fixed$delta <- 0
      family_fixed$lambda <- 1
    } else if (family == "beta") {
      family_fixed$alpha <- 1 # Beta: alpha = 1, beta = 1, lambda = 1 fixed
      family_fixed$beta <- 1
      family_fixed$lambda <- 1
    }
  }

  return(family_fixed)
}

#' Get default start values for a family
#'
#' @param family Character string specifying the family.
#' @return Named list of starting values.
#' @keywords internal
.get_default_start <- function(family) {
  if (family == "gkw") {
    start <- list(alpha = 2, beta = 2, gamma = 1, delta = 0.5, lambda = 2)
  } else if (family == "bkw") {
    start <- list(alpha = 2, beta = 2, gamma = 1, delta = 0.5) # lambda = 1 fixed
  } else if (family == "kkw") {
    start <- list(alpha = 2, beta = 2, delta = 0.5, lambda = 2) # gamma = 1 fixed
  } else if (family == "ekw") {
    start <- list(alpha = 2, beta = 2, lambda = 2) # gamma = 1, delta = 0 fixed
  } else if (family == "mc") {
    start <- list(gamma = 1, delta = 0.5, lambda = 2) # alpha = 1, beta = 1 fixed
  } else if (family == "kw") {
    start <- list(alpha = 2, beta = 2) # gamma = 1, delta = 0, lambda = 1 fixed
  } else if (family == "beta") {
    start <- list(gamma = 1, delta = 0.5) # alpha = 1, beta = 1, lambda = 1 fixed
  }

  return(start)
}

#' Validate data for GKw family distributions
#'
#' @param data Numeric vector with values in the (0, 1) interval.
#' @param n_params Number of parameters to estimate.
#' @return Validated and possibly adjusted data vector.
#' @keywords internal
.validate_data <- function(data, n_params) {
  # Data validation
  if (!is.numeric(data)) {
    stop("Data must be numeric")
  }

  if (any(is.na(data))) {
    warning("Missing values removed from data")
    data <- data[!is.na(data)]
  }

  if (length(data) < n_params) {
    stop(paste0("At least ", n_params, " observations are required to fit a ", n_params, "-parameter model"))
  }

  # Check data bounds with a small tolerance for numerical precision
  epsilon <- .Machine$double.eps^0.5
  if (any(data <= epsilon | data >= (1 - epsilon))) {
    # Try to automatically adjust boundary values
    orig_length <- length(data)
    boundary_vals <- data <= epsilon | data >= (1 - epsilon)

    if (sum(boundary_vals) > 0.1 * orig_length) {
      # If more than 10% of data is at boundaries, warn and stop
      stop(
        "Too many data values (", sum(boundary_vals),
        ") are at or beyond the boundaries of (0, 1). Please preprocess your data."
      )
    } else {
      # Adjust boundary values with a warning
      data[data <= epsilon] <- epsilon + epsilon * 10
      data[data >= (1 - epsilon)] <- (1 - epsilon) - epsilon * 10
      warning("Adjusted ", sum(boundary_vals), " data values that were at or beyond boundaries to be within (0, 1)")
    }
  }

  return(data)
}

#' Validate parameters for GKw family distributions
#'
#' @param start List with initial parameter values.
#' @param fixed List of parameters to be held fixed (not estimated).
#' @param param_names Character vector of parameter names needed for the model.
#' @return List containing validated start and fixed parameter lists.
#' @keywords internal
.validate_parameters <- function(start, fixed, param_names) {
  # Check that all required parameters for the selected family are provided
  if (!is.null(start)) {
    missing_params <- setdiff(param_names, names(start))

    if (length(missing_params) > 0) {
      stop("Missing parameters in 'start': ", paste(missing_params, collapse = ", "))
    }

    # Check for valid parameter values
    invalid_params <- names(start)[start <= 0]
    if (length(invalid_params) > 0) {
      warning(
        "Initial values for parameters must be positive. Adjusting: ",
        paste(invalid_params, collapse = ", ")
      )
      for (param in invalid_params) {
        start[[param]] <- 1.0 # Set to a reasonable default
      }
    }
  }

  # Apply fixed parameters if any
  if (!is.null(fixed)) {
    # Validate fixed parameters
    fixed_invalid <- names(fixed)[fixed <= 0 & names(fixed) != "delta"]
    if (length(fixed_invalid) > 0) {
      stop("Fixed parameters must be positive: ", paste(fixed_invalid, collapse = ", "))
    }

    # For delta, ensure it's at least 0
    if ("delta" %in% names(fixed) && fixed$delta < 0) {
      stop("Fixed parameter 'delta' must be non-negative")
    }

    for (param in names(fixed)) {
      if (param %in% names(start)) {
        start[[param]] <- fixed[[param]]
      } else if (!param %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
        warning("Ignoring unknown fixed parameter: ", param)
      }
    }
  }

  return(list(start = start, fixed = fixed))
}

#' Determine initial parameter values
#'
#' @param data Numeric vector with values in the (0, 1) interval.
#' @param start Optional list with initial parameter values.
#' @param fixed Optional list of parameters to be held fixed.
#' @param family Character string specifying the distribution family.
#' @param use_moments Logical; if TRUE, uses method of moments for initial values.
#' @param silent Logical; if TRUE, suppresses messages.
#' @return List with initial parameter values.
#' @keywords internal
.determine_start_values <- function(data, start, fixed, family, use_moments, silent) {
  param_info <- .get_family_param_info(family)
  param_names <- param_info$names

  # Get default fixed parameters for the family
  family_fixed <- .get_default_fixed(family)

  # Merge user-provided fixed parameters with family-specific fixed parameters
  if (!is.null(fixed)) {
    fixed <- c(fixed, family_fixed[!names(family_fixed) %in% names(fixed)])
  } else {
    fixed <- family_fixed
  }

  # Generate initial values using method of moments if requested
  if (use_moments) {
    if (!silent) {
      message("Computing starting values using method of moments...")
    }

    # Try to use gkwgetstartvalues with a fallback
    moment_start <- tryCatch(
      {
        gkwgetstartvalues(data, n_starts = 10)
      },
      error = function(e) {
        warning(
          "Error in method of moments estimation: ", e$message,
          ". Using default starting values."
        )
        c(alpha = 2, beta = 2, gamma = 1, delta = 0.5, lambda = 2)
      },
      warning = function(w) {
        warning("Warning in method of moments estimation: ", w$message)
        # Continue execution but still return the result
        NULL
      }
    )

    if (is.null(start) && !is.null(moment_start)) {
      start <- as.list(moment_start)
    } else if (!silent) {
      message("Using provided start values instead of method of moments estimates.")
    }
  }

  # If start is still NULL, use default starting values
  if (is.null(start)) {
    # Get default starting values for the family
    start <- .get_default_start(family)

    if (!silent) {
      message("Using default starting values for ", family, " family parameters")
    }
  }

  # Validate parameters
  valid <- .validate_parameters(start, fixed, param_names)
  start <- valid$start
  fixed <- valid$fixed

  return(list(start = start, fixed = fixed))
}


#' Fit GKw family distributions using TMB
#'
#' @param data Numeric vector with values in the (0, 1) interval.
#' @param family Character string specifying the distribution family.
#' @param start List with initial parameter values.
#' @param fixed List of parameters to be held fixed.
#' @param method Optimization method: "nlminb" or "optim".
#' @param hessian Logical; if TRUE, computes standard errors and covariance matrix.
#' @param conf.level Confidence level for intervals.
#' @param optimizer.control List of control parameters for the optimizer.
#' @param silent Logical; if TRUE, suppresses messages.
#'
#' @importFrom stats nlminb optim pnorm
#' @importFrom utils modifyList
#'
#' @return List containing fit results.
#' @keywords internal
.fit_tmb <- function(data, family, start, fixed, method, hessian, conf.level, optimizer.control, silent) {
  if (!requireNamespace("TMB", quietly = TRUE)) {
    stop("Package 'TMB' is required but not installed. Please install it with install.packages('TMB')")
  }

  # dll_name <- "gkwmletmb"

  # Check and compile TMB code
  tryCatch(
    {
      .check_and_compile_TMB_code("gkwmletmb", verbose = !silent)
    },
    error = function(e) {
      stop("Failed to compile TMB code: ", e$message)
    }
  )

  if (!silent) {
    message("Checking TMB model compilation...")
  }

  # Get the numeric code for family
  family_code <- .family_to_code(family)

  # Get the parameter names for this family
  param_names <- .get_family_param_info(family)$names

  # Create full parameter list for TMB
  full_start_tmb <- list(
    log_alpha = log(1), # Default values, will be overwritten if needed
    log_beta = log(1),
    log_gamma = log(1),
    log_delta = log(0.0001), # Small value for delta to avoid log(0)
    log_lambda = log(1)
  )

  # Update with user-provided start values
  for (param in names(start)) {
    full_start_tmb[[paste0("log_", param)]] <- log(start[[param]])
  }

  # Apply fixed parameters for TMB
  map <- list()

  # Add family-specific fixed parameters to the map
  all_fixed_params <- c(names(fixed), setdiff(
    c("alpha", "beta", "gamma", "delta", "lambda"),
    c(param_names, names(fixed))
  ))

  for (param in all_fixed_params) {
    param_name <- paste0("log_", param)
    if (param %in% names(fixed)) {
      # User or family provided fixed value
      full_start_tmb[[param_name]] <- log(fixed[[param]])
    } else if (param == "alpha" || param == "beta") {
      # Default alpha, beta = 1 when not specified
      full_start_tmb[[param_name]] <- log(1)
    } else if (param == "gamma") {
      # Default gamma = 1 when not specified
      full_start_tmb[[param_name]] <- log(1)
    } else if (param == "delta") {
      # Default delta = 0 when not specified (use small value to avoid log(0))
      full_start_tmb[[param_name]] <- log(0.0001)
    } else if (param == "lambda") {
      # Default lambda = 1 when not specified
      full_start_tmb[[param_name]] <- log(1)
    }

    # Add to map to keep parameter fixed
    map[[param_name]] <- factor(NA)
  }

  # Prepare data for TMB
  tmb_data <- list(
    x = data,
    family = family_code
  )

  # Create TMB object
  obj <- tryCatch(
    {
      TMB::MakeADFun(
        data = tmb_data,
        parameters = full_start_tmb,
        map = if (length(map) > 0) map else NULL,
        DLL = "gkwmletmb",
        silent = silent
      )
    },
    error = function(e) {
      stop("Error creating TMB model: ", e$message)
    }
  )

  # Set up optimizer controls based on the chosen method
  if (method == "nlminb") {
    control_defaults <- list(eval.max = 500, iter.max = 300, trace = ifelse(silent, 0, 1))
  } else if (method == "optim") {
    control_defaults <- list(maxit = 500, trace = ifelse(silent, 0, 1))
  } else {
    stop("Unknown optimizer method: ", method)
  }

  # Merge user controls with defaults, with user controls taking precedence
  control <- modifyList(control_defaults, optimizer.control)

  # Run optimization
  if (method == "nlminb") {
    opt <- tryCatch(
      {
        nlminb(
          start = obj$par,
          objective = obj$fn,
          gradient = obj$gr,
          control = control
        )
      },
      error = function(e) {
        stop("Optimization with nlminb failed: ", e$message)
      }
    )

    opt$convergence <- opt$convergence == 0
    opt$message <- opt$message
  } else {
    opt <- tryCatch(
      {
        optim(
          par = obj$par,
          fn = obj$fn,
          gr = obj$gr,
          method = method,
          control = control
        )
      },
      error = function(e) {
        stop("Optimization with optim failed: ", e$message)
      }
    )

    opt$objective <- opt$value

    opt$convergence <- opt$convergence == 0
    opt$message <- if (opt$convergence) "Successful convergence" else "Optimization failed to converge"
  }

  if (!opt$convergence) {
    warning("Model did not converge: ", opt$message)
  }

  # Get parameter estimates for all parameters
  all_params <- exp(opt$par)
  names(all_params) <- sub("log_", "", names(all_params))

  # Filter parameters to include only those relevant for the family
  filtered_coefficients <- all_params[paste0("log_", param_names) %in% names(opt$par)]
  names(filtered_coefficients) <- param_names[param_names %in% sub("log_", "", names(opt$par))]

  # If some parameters are missing (fixed), add them from the fixed list
  for (param in param_names) {
    if (!param %in% names(filtered_coefficients)) {
      if (param %in% names(fixed)) {
        filtered_coefficients[param] <- fixed[[param]]
      } else {
        # Use default values for missing parameters
        if (param == "alpha" || param == "beta" || param == "gamma" || param == "lambda") {
          filtered_coefficients[param] <- 1.0
        } else if (param == "delta") {
          filtered_coefficients[param] <- 0.0
        }
      }
    }
  }

  # Ensure the order matches param_names
  filtered_coefficients <- filtered_coefficients[param_names]

  # Calculate standard errors and Hessian if requested
  filtered_std_errors <- rep(NA, length(param_names))
  names(filtered_std_errors) <- param_names

  cov_matrix <- NULL
  coef_summary <- NULL
  conf_int <- NULL

  if (hessian) {
    sd_report <- tryCatch(
      {
        TMB::sdreport(obj)
      },
      error = function(e) {
        warning("Error calculating standard errors: ", e$message)
        NULL
      }
    )

    if (!is.null(sd_report) && !is.character(sd_report)) {
      # Extract parameter estimates, SEs, and covariance matrix
      cov_matrix <- sd_report$cov.fixed
      std_errors_log <- sqrt(diag(cov_matrix))

      # Get the SEs only for non-fixed parameters
      for (i in seq_along(param_names)) {
        param <- param_names[i]
        log_param <- paste0("log_", param)

        if (log_param %in% names(std_errors_log)) {
          # Use the Delta method to transform SEs to original scale
          filtered_std_errors[i] <- std_errors_log[log_param] * filtered_coefficients[i]
        }
      }

      # Create coefficient summary
      coef_summary <- data.frame(
        Estimate = filtered_coefficients,
        `Std. Error` = filtered_std_errors,
        `z value` = filtered_coefficients / filtered_std_errors,
        `Pr(>|z|)` = 2 * pnorm(abs(filtered_coefficients / filtered_std_errors), lower.tail = FALSE),
        row.names = param_names,
        check.names = FALSE
      )

      # Calculate confidence intervals
      z_value <- stats::qnorm(1 - (1 - conf.level) / 2)
      conf_int_params <- character()
      conf_int_estimates <- numeric()
      conf_int_se <- numeric()
      conf_int_lower <- numeric()
      conf_int_upper <- numeric()

      for (i in seq_along(param_names)) {
        if (!is.na(filtered_std_errors[i])) {
          param <- param_names[i]
          conf_int_params <- c(conf_int_params, param)
          conf_int_estimates <- c(conf_int_estimates, filtered_coefficients[i])
          conf_int_se <- c(conf_int_se, filtered_std_errors[i])
          lower <- max(filtered_coefficients[i] - z_value * filtered_std_errors[i], .Machine$double.eps)
          upper <- filtered_coefficients[i] + z_value * filtered_std_errors[i]
          conf_int_lower <- c(conf_int_lower, lower)
          conf_int_upper <- c(conf_int_upper, upper)
        }
      }

      if (length(conf_int_params) > 0) {
        conf_int <- data.frame(
          parameter = conf_int_params,
          estimate = conf_int_estimates,
          std.error = conf_int_se,
          lower = conf_int_lower,
          upper = conf_int_upper,
          row.names = NULL, check.names = FALSE
        )
      }
    } else {
      warning("Hessian calculation failed, standard errors not available")

      # Create coefficient summary without SEs
      coef_summary <- data.frame(
        Estimate = filtered_coefficients,
        `Std. Error` = filtered_std_errors,
        `z value` = rep(NA, length(param_names)),
        `Pr(>|z|)` = rep(NA, length(param_names)),
        row.names = param_names,
        check.names = FALSE
      )
    }
  } else {
    # If hessian = FALSE, only report parameter estimates
    coef_summary <- data.frame(
      Estimate = filtered_coefficients,
      `Std. Error` = filtered_std_errors,
      `z value` = rep(NA, length(param_names)),
      `Pr(>|z|)` = rep(NA, length(param_names)),
      row.names = param_names,
      check.names = FALSE
    )
  }

  # Calculate log likelihood, AIC, BIC
  loglik <- -opt$objective
  n <- length(data)
  k <- sum(!is.na(opt$par)) # Count non-fixed parameters
  aic <- -2 * loglik + 2 * k
  bic <- -2 * loglik + log(n) * k
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)

  # Create comprehensive result object for TMB
  result <- list(
    coefficients = filtered_coefficients, # Only the parameters for this family
    std.errors = filtered_std_errors,
    coef_summary = coef_summary,
    vcov = cov_matrix,
    loglik = loglik,
    AIC = aic,
    BIC = bic,
    AICc = aicc,
    data = data,
    nobs = n,
    df = k,
    convergence = opt$convergence,
    message = opt$message,
    method = "tmb",
    optimizer_method = method,
    conf.int = conf_int,
    conf.level = conf.level,
    optimizer = opt,
    obj = obj,
    fixed = fixed
  )

  return(result)
}



#' Calculate profile likelihoods
#'
#' @param result Fit result from TMB or Newton-Raphson.
#' @param data Numeric vector with values in the (0, 1) interval.
#' @param family Character string specifying the distribution family.
#' @param fixed List of parameters to be held fixed.
#' @param fit Estimation method: "tmb" or "nr".
#' @param method Optimization method (for TMB): "nlminb" or "optim".
#' @param npoints Number of points in profile.
#' @param silent Logical; if TRUE, suppresses messages.
#' @return List of profile likelihoods.
#' @keywords internal
.calculate_profiles <- function(result, data, family, fixed, fit, method, npoints, silent) {
  prof_list <- list()

  # Filter out fixed parameters
  param_info <- .get_family_param_info(family)
  params_to_profile <- setdiff(param_info$names, names(fixed))

  # Configure progress reporting
  total_profiles <- length(params_to_profile)
  if (!silent && total_profiles > 0) {
    message("Computing profile likelihoods for ", total_profiles, " parameters...")
  }

  # Process each parameter
  for (param_idx in seq_along(params_to_profile)) {
    param <- params_to_profile[param_idx]

    if (!silent) {
      message("  Computing profile for ", param, " (", param_idx, "/", total_profiles, ")...")
    }

    # Get current parameter estimate
    est_value <- result$coefficients[param]

    # Determine appropriate range for profiling
    if (!is.null(result$std.errors) && !is.na(result$std.errors[param])) {
      # Use standard error to determine range if available
      se <- result$std.errors[param]
      # Calculate range ensuring positive values and reasonable breadth
      min_value <- max(est_value - 3 * se, .Machine$double.eps * 10)
      max_value <- est_value + 3 * se

      profile_range <- seq(min_value, max_value, length.out = npoints)
    } else {
      # If no standard error, use a proportional range
      min_value <- max(est_value * 0.2, .Machine$double.eps * 10)
      max_value <- est_value * 2.0

      profile_range <- seq(min_value, max_value, length.out = npoints)
    }

    # Calculate log-likelihood at each point in the profile
    prof_ll <- numeric(length(profile_range))

    for (i in seq_along(profile_range)) {
      if (fit == "tmb") {
        # For TMB, modify log parameters
        tmb_par <- result$optimizer$par
        log_param <- paste0("log_", param)
        tmb_par[log_param] <- log(profile_range[i])

        # Evaluate negative log-likelihood
        prof_ll[i] <- -result$obj$fn(tmb_par)
      } else {
        # For Newton-Raphson, directly modify the parameter vector
        mod_params <- result$coefficients
        mod_params[param] <- profile_range[i]

        # Transform to the family-specific parameter vector
        param_names <- .get_family_param_info(family)$names
        family_mod_params <- mod_params[param_names]

        # Use the appropriate log-likelihood function based on family
        ll_func <- switch(family,
          "gkw" = llgkw,
          "bkw" = llbkw,
          "kkw" = llkkw,
          "ekw" = llekw,
          "mc" = llmc,
          "kw" = llkw,
          "beta" = llbeta
        )

        # Use ll_func directly (already negated for consistency)
        prof_ll[i] <- ll_func(family_mod_params, data)
      }
    }

    # Create profile data frame
    prof_list[[param]] <- data.frame(
      parameter = param,
      value = profile_range,
      loglik = prof_ll
    )
  }

  return(prof_list)
}

#' Fit submodels for comparison
#'
#' @param data Numeric vector with values in the (0, 1) interval.
#' @param result Main fit result.
#' @param fit Estimation method: "tmb" or "nr".
#' @param method Optimization method (for TMB): "nlminb" or "optim".
#' @param hessian Logical; if TRUE, computes standard errors and covariance matrix.
#' @param optimizer.control List of control parameters for the optimizer.
#' @param silent Logical; if TRUE, suppresses messages.
#'
#' @importFrom stats pchisq
#'
#' @return List containing submodel fits and LRT results.
#' @keywords internal
.fit_submodels <- function(data, result, fit, method, hessian, optimizer.control, silent) {
  # Only fit submodels if current model is GKw (full model)
  if (result$family != "gkw") {
    if (!silent) {
      message("Submodel fitting is only available when family = 'gkw'. Skipping.")
    }
    return(NULL)
  }

  if (!silent) {
    message("Fitting submodels for comparison...")
  }

  submodel_results <- list()
  submodel_families <- c("bkw", "kkw", "ekw", "mc", "kw", "beta")

  for (submodel in submodel_families) {
    if (!silent) {
      message("  Fitting ", submodel, " submodel...")
    }

    tryCatch(
      {
        # Use current parameter estimates as starting values for submodel
        submodel_start <- as.list(result$coefficients)

        # Subset to include only parameters needed for this family
        submodel_param_names <- .get_family_param_info(submodel)$names
        submodel_start <- submodel_start[submodel_param_names]

        submodel_result <- gkwfit(
          data = data,
          family = submodel,
          start = submodel_start,
          fit = fit,
          method = method,
          use_moments = FALSE,
          hessian = hessian,
          profile = FALSE,
          plot = FALSE,
          silent = TRUE,
          optimizer.control = optimizer.control
        )

        submodel_results[[submodel]] <- submodel_result
      },
      error = function(e) {
        warning("Failed to fit ", submodel, " submodel: ", e$message)
      }
    )
  }

  # Perform likelihood ratio tests
  lrt_results <- list()

  for (submodel in names(submodel_results)) {
    lrt_result <- data.frame(
      Model = paste(submodel, "vs GKw"),
      df = result$df - submodel_results[[submodel]]$df,
      loglik_full = result$loglik,
      loglik_reduced = submodel_results[[submodel]]$loglik,
      LR_statistic = 2 * (result$loglik - submodel_results[[submodel]]$loglik),
      p_value = 1 - pchisq(
        2 * (result$loglik - submodel_results[[submodel]]$loglik),
        result$df - submodel_results[[submodel]]$df
      )
    )
    lrt_results[[submodel]] <- lrt_result
  }

  return(list(submodels = submodel_results, lrt = lrt_results))
}



#' Generate diagnostic plots for distribution models
#'
#' This internal function creates a set of diagnostic plots for evaluating
#' the fit of various distribution families to bounded data in the (0, 1) interval.
#' It generates histograms with fitted density overlays, P-P plots, Q-Q plots,
#' and profile likelihood plots when available.
#'
#' @param result A list containing model fit results from TMB or Newton-Raphson,
#'        must include a 'coefficients' element with named parameters.
#' @param data Numeric vector with values in the (0, 1) interval.
#' @param family Character string specifying the distribution family.
#'        Supported values: "gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta".
#' @param silent Logical; if TRUE, suppresses messages. Default is FALSE.
#'
#' @return A list of ggplot2 objects:
#'   \item{histogram}{Histogram with fitted density overlay}
#'   \item{pp_plot}{Probability-Probability plot}
#'   \item{qq_plot}{Quantile-Quantile plot}
#'   \item{profile_*}{Profile likelihood plots for each parameter (if available)}
#'
#' @importFrom stats ecdf ppoints qchisq density
#'
#' @keywords internal
.generate_plots <- function(result, data, family, silent = FALSE) {
  plots <- list()

  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Package 'ggplot2' is required for plotting but not installed. Plotting will be disabled.")
    return(NULL)
  }

  if (!silent) {
    message("Generating diagnostic plots...")
  }

  # Calculate density values on a grid
  x_grid <- seq(0.001, 0.999, length.out = 200)

  # Use the appropriate density function based on family
  density_func <- switch(family,
    "gkw" = function(x) {
      dgkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "bkw" = function(x) {
      dbkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"]
      )
    },
    "kkw" = function(x) {
      dkkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["delta"], result$coefficients["lambda"]
      )
    },
    "ekw" = function(x) {
      dekw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["lambda"]
      )
    },
    "mc" = function(x) {
      dmc(
        x, result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "kw" = function(x) dkw(x, result$coefficients["alpha"], result$coefficients["beta"]),
    "beta" = function(x) dbeta_(x, result$coefficients["gamma"], result$coefficients["delta"])
  )

  density_values <- tryCatch(
    {
      sapply(x_grid, density_func)
    },
    error = function(e) {
      warning("Error calculating density values for plot: ", e$message)
      rep(NA, length(x_grid))
    }
  )

  # Create data frame for plotting
  plot_data <- data.frame(x = x_grid, density = density_values)

  # Create histogram with density curve
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = data, y = ggplot2::after_stat(density)),
      bins = min(30, ceiling(sqrt(length(data)))),
      fill = "lightblue", color = "black", alpha = 0.7
    ) +
    ggplot2::geom_line(
      data = plot_data, ggplot2::aes(x = x, y = density),
      color = "red", linewidth = 1
    ) +
    ggplot2::labs(
      title = paste("Fitted", toupper(family), "Distribution"),
      x = "Data", y = "Density"
    ) +
    ggplot2::theme_minimal()

  plots$histogram <- p1

  # P-P plot (Probability-Probability plot)
  ecdf_vals <- ecdf(data)(sort(data))

  # Use the appropriate CDF function based on family
  cdf_func <- switch(family,
    "gkw" = function(x) {
      pgkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "bkw" = function(x) {
      pbkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"]
      )
    },
    "kkw" = function(x) {
      pkkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["delta"], result$coefficients["lambda"]
      )
    },
    "ekw" = function(x) {
      pekw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["lambda"]
      )
    },
    "mc" = function(x) {
      pmc(
        x, result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "kw" = function(x) pkw(x, result$coefficients["alpha"], result$coefficients["beta"]),
    "beta" = function(x) pbeta_(x, result$coefficients["gamma"], result$coefficients["delta"])
  )

  # Calculate theoretical CDF values
  theor_cdf <- tryCatch(
    {
      sapply(sort(data), cdf_func)
    },
    error = function(e) {
      warning("Error calculating theoretical CDF for P-P plot: ", e$message)
      rep(NA, length(data))
    }
  )

  # Create P-P plot data frame
  pp_data <- data.frame(Empirical = ecdf_vals, Theoretical = theor_cdf)

  # Create P-P plot
  p2 <- ggplot2::ggplot(pp_data, ggplot2::aes(x = Theoretical, y = Empirical)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "P-P Plot",
      x = "Theoretical Probability", y = "Empirical Probability"
    ) +
    ggplot2::theme_minimal()

  plots$pp_plot <- p2

  # Q-Q plot (Quantile-Quantile plot)
  # Use the appropriate quantile function based on family
  quant_func <- switch(family,
    "gkw" = function(p) {
      qgkw(
        p, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "bkw" = function(p) {
      qbkw(
        p, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"]
      )
    },
    "kkw" = function(p) {
      qkkw(
        p, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["delta"], result$coefficients["lambda"]
      )
    },
    "ekw" = function(p) {
      qekw(
        p, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["lambda"]
      )
    },
    "mc" = function(p) {
      qmc(
        p, result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "kw" = function(p) qkw(p, result$coefficients["alpha"], result$coefficients["beta"]),
    "beta" = function(p) qbeta_(p, result$coefficients["gamma"], result$coefficients["delta"])
  )

  # Calculate theoretical quantiles
  theor_quant <- tryCatch(
    {
      sapply(ppoints(length(data)), quant_func)
    },
    error = function(e) {
      warning("Error calculating theoretical quantiles for Q-Q plot: ", e$message)
      rep(NA, length(data))
    }
  )

  # Create Q-Q plot data frame
  qq_data <- data.frame(Theoretical = theor_quant, Empirical = sort(data))

  # Create Q-Q plot
  p3 <- ggplot2::ggplot(qq_data, ggplot2::aes(x = Theoretical, y = Empirical)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "Q-Q Plot",
      x = "Theoretical Quantiles", y = "Empirical Quantiles"
    ) +
    ggplot2::theme_minimal()

  plots$qq_plot <- p3

  # Add profile likelihood plots if available
  if (!is.null(result$profile) && length(result$profile) > 0) {
    for (param in names(result$profile)) {
      prof_data <- result$profile[[param]]

      # Calculate reference line at max - qchisq(0.95, 1)/2 for 95% confidence
      ref_level <- max(prof_data$loglik, na.rm = TRUE) - qchisq(0.95, 1) / 2

      # Create profile likelihood plot
      p <- ggplot2::ggplot(prof_data, ggplot2::aes(x = value, y = loglik)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_vline(
          xintercept = result$coefficients[param],
          linetype = "dashed", color = "red"
        ) +
        ggplot2::geom_hline(
          yintercept = ref_level,
          linetype = "dotted", color = "blue"
        ) +
        ggplot2::labs(
          title = paste("Profile Likelihood for", param),
          x = param, y = "Log-likelihood"
        ) +
        ggplot2::theme_minimal()

      plots[[paste0("profile_", param)]] <- p
    }
  }
  return(plots)
}


#' Calculate goodness-of-fit statistics
#'
#' @param result Fit result from TMB or Newton-Raphson.
#' @param data Numeric vector with values in the (0, 1) interval.
#' @param family Character string specifying the distribution family.
#' @param silent Logical; if TRUE, suppresses messages.
#' @return List of goodness-of-fit statistics.
#' @importFrom stats ks.test var integrate
#' @keywords internal
.calculate_gof <- function(result, data, family, silent) {
  if (!silent) {
    message("Calculating goodness-of-fit statistics...")
  }

  gof <- list()

  # Use the appropriate CDF function based on family for Kolmogorov-Smirnov test
  cdf_func <- switch(family,
    "gkw" = function(x) {
      pgkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "bkw" = function(x) {
      pbkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"]
      )
    },
    "kkw" = function(x) {
      pkkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["delta"], result$coefficients["lambda"]
      )
    },
    "ekw" = function(x) {
      pekw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["lambda"]
      )
    },
    "mc" = function(x) {
      pmc(
        x, result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "kw" = function(x) pkw(x, result$coefficients["alpha"], result$coefficients["beta"]),
    "beta" = function(x) pbeta_(x, result$coefficients["gamma"], result$coefficients["delta"])
  )

  # Kolmogorov-Smirnov test
  ks_test <- suppressWarnings(
    ks.test(data, cdf_func)
  )
  gof$ks <- ks_test

  # Use the appropriate density function based on family
  density_func <- switch(family,
    "gkw" = function(x) {
      dgkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "bkw" = function(x) {
      dbkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["gamma"], result$coefficients["delta"]
      )
    },
    "kkw" = function(x) {
      dkkw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["delta"], result$coefficients["lambda"]
      )
    },
    "ekw" = function(x) {
      dekw(
        x, result$coefficients["alpha"], result$coefficients["beta"],
        result$coefficients["lambda"]
      )
    },
    "mc" = function(x) {
      dmc(
        x, result$coefficients["gamma"], result$coefficients["delta"],
        result$coefficients["lambda"]
      )
    },
    "kw" = function(x) dkw(x, result$coefficients["alpha"], result$coefficients["beta"]),
    "beta" = function(x) dbeta_(x, result$coefficients["gamma"], result$coefficients["delta"])
  )

  # Calculate diagnostic statistics
  diagnostics <- list(
    mean_obs = mean(data),
    var_obs = var(data),
    mean_fitted = tryCatch(
      {
        integrate(function(x) x * density_func(x), 0, 1)$value
      },
      error = function(e) NA
    ),
    var_fitted = tryCatch(
      {
        m1 <- integrate(function(x) x * density_func(x), 0, 1)$value
        m2 <- integrate(function(x) x^2 * density_func(x), 0, 1)$value
        m2 - m1^2
      },
      error = function(e) NA
    )
  )

  return(list(gof = gof, diagnostics = diagnostics))
}

#' Get default fixed parameters for each GKw family
#'
#' @param family Character string, the family name
#' @return Named list of parameters that are fixed by default for this family
#' @keywords internal
.get_family_fixed_defaults <- function(family) {
  fixed_params <- switch(family,
    "gkw" = list(), # All parameters free
    "bkw" = list(lambda = 1.0),
    "kkw" = list(gamma = 1.0),
    "ekw" = list(gamma = 1.0, delta = 0.0),
    "mc" = list(alpha = 1.0, beta = 1.0),
    "kw" = list(gamma = 1.0, delta = 0.0, lambda = 1.0),
    "beta" = list(alpha = 1.0, beta = 1.0, lambda = 1.0),
    list() # Default case: no fixed parameters
  )

  return(fixed_params)
}


#' Fit submodels for the GKw family for model comparison
#'
#' This internal function fits nested submodels for a given GKw family model,
#' to facilitate likelihood ratio testing and model comparison.
#'
#' @param data Original data used for the main model.
#' @param result Result from the main model fit.
#' @param method Optimization method used.
#' @param hessian Logical; if TRUE, compute standard errors for submodels.
#' @param optimizer.control Control parameters for optimization.
#' @param silent Logical; if TRUE, suppress messages.
#'
#' @return A list containing submodel fits and likelihood ratio test results.
#' @keywords internal
.fit_submodels_tmb <- function(data, result, method, hessian, optimizer.control, silent) {
  # Define the chain of nested models for each family
  model_chain <- list(
    gkw = c("gkw", "bkw", "kkw", "ekw", "kw", "beta"),
    bkw = c("bkw", "ekw", "kw"),
    kkw = c("kkw", "ekw", "kw"),
    ekw = c("ekw", "kw"),
    mc = c("mc", "beta"),
    kw = c("kw"),
    beta = c("beta")
  )

  family <- result$family
  subchain <- model_chain[[family]]

  # Remove the current model from the chain (we already have it)
  subchain <- subchain[subchain != family]

  if (length(subchain) == 0) {
    if (!silent) message("No submodels to fit for family ", family)
    return(NULL)
  }

  # Prepare results containers
  submodels <- list()
  lrt_results <- data.frame(
    model1 = character(),
    model2 = character(),
    statistic = numeric(),
    df = numeric(),
    p.value = numeric(),
    stringsAsFactors = FALSE
  )

  # Fixed parameters for the main model
  original_fixed <- result$fixed

  # Fit each submodel
  for (subfam in subchain) {
    if (!silent) message("Fitting submodel: ", subfam)

    # Get default fixed parameters for this subfamily
    default_fixed <- .get_family_fixed_defaults(subfam)

    # Combine with user's original fixed parameters (already included in result$fixed)
    submodel_fixed <- original_fixed
    for (param in names(default_fixed)) {
      if (!param %in% names(submodel_fixed)) {
        submodel_fixed[[param]] <- default_fixed[[param]]
      }
    }

    # Use existing parameter estimates as starting values when possible
    start_from_parent <- result$coefficients
    start_for_sub <- list()
    for (param in names(start_from_parent)) {
      if (!param %in% names(submodel_fixed)) {
        start_for_sub[[param]] <- start_from_parent[[param]]
      }
    }

    # Fit the submodel
    sub_result <- tryCatch(
      {
        gkwfit(
          data = data,
          family = subfam,
          start = start_for_sub,
          fixed = submodel_fixed,
          method = method,
          hessian = hessian,
          profile = FALSE,
          plot = FALSE,
          optimizer.control = optimizer.control,
          silent = silent
        )
      },
      error = function(e) {
        warning("Failed to fit submodel ", subfam, ": ", e$message)
        NULL
      }
    )

    if (!is.null(sub_result)) {
      submodels[[subfam]] <- sub_result

      # Calculate LRT between parent and this submodel
      lr_stat <- 2 * (result$loglik - sub_result$loglik)
      df_diff <- result$df - sub_result$df
      p_val <- stats::pchisq(lr_stat, df = df_diff, lower.tail = FALSE)

      lrt_results <- rbind(lrt_results, data.frame(
        model1 = result$family,
        model2 = subfam,
        statistic = lr_stat,
        df = df_diff,
        p.value = p_val,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Combine results
  out <- list(
    submodels = submodels,
    lrt = lrt_results
  )

  return(out)
}



#' @title Fit Generalized Kumaraswamy Distribution via Maximum Likelihood Estimation using TMB
#'
#' @description
#' Fits any distribution from the Generalized Kumaraswamy (GKw) family to data using maximum
#' likelihood estimation through Template Model Builder (TMB). The function supports several
#' optimization methods including R's nlminb and various optim algorithms.
#'
#' @param data A numeric vector with values strictly between 0 and 1. Values at the boundaries
#'   (0 or 1) may cause issues; consider slight adjustments if necessary (see Details).
#' @param family A character string specifying the distribution family. One of: \code{"gkw"} (default),
#'   \code{"bkw"}, \code{"kkw"}, \code{"ekw"}, \code{"mc"}, \code{"kw"}, or \code{"beta"}. See Details for parameter specifications.
#' @param start Optional list with initial parameter values (using natural parameter names like `alpha`, `beta`, etc.).
#'   If \code{NULL}, reasonable starting values will be determined, potentially using the method of moments if \code{use_moments = TRUE}.
#' @param fixed Optional list of parameters to be held fixed at specific values during estimation (e.g., \code{list(lambda = 1)}).
#' @param method Optimization method to use. One of: \code{"nlminb"} (default), \code{"Nelder-Mead"}, \code{"BFGS"},
#'   \code{"CG"}, \code{"L-BFGS-B"} or \code{"SANN"}. If \code{"nlminb"} is selected, R's \code{\link[stats]{nlminb}} function is used;
#'   otherwise, R's \code{\link[stats]{optim}} function is used with the specified method.
#' @param use_moments Logical; if \code{TRUE} and \code{start = NULL}, attempts to use method of moments estimates
#'   (via `gkwgetstartvalues`) as initial values. Default: \code{FALSE}.
#' @param hessian Logical; if \code{TRUE}, attempts to compute the Hessian matrix at the MLE to estimate
#'   standard errors and the variance-covariance matrix using TMB's `sdreport`. Default: \code{TRUE}.
#' @param profile Logical; if \code{TRUE}, computes likelihood profiles for parameters using TMB's profiling capabilities. Default: \code{FALSE}.
#' @param npoints Integer; number of points to use in profile likelihood calculations (minimum 5). Only relevant if \code{profile = TRUE}. Default: 20.
#' @param plot Logical; if \code{TRUE}, generates diagnostic plots (histogram with fitted density, QQ-plot) using `ggplot2` and `patchwork`. Default: \code{TRUE}.
#' @param conf.level Numeric, the confidence level for confidence intervals calculated from standard errors (requires \code{hessian = TRUE}). Default: 0.95.
#' @param optimizer.control List of control parameters passed to the chosen optimizer.
#'   The valid parameters depend on the `method` chosen. See Details.
#' @param submodels Logical; if \code{TRUE}, fits relevant nested submodels for comparison via likelihood ratio tests. Default: \code{FALSE}.
#' @param silent Logical; if \code{TRUE}, suppresses messages during fitting. Default: \code{FALSE}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{"gkwfit"} (inheriting from \code{"list"}) containing the fitted model results. Key components include:
#' \item{coefficients}{Named vector of estimated parameters (on their natural scale).}
#' \item{std.errors}{Named vector of estimated standard errors (if \code{hessian = TRUE}).}
#' \item{coef_summary}{Data frame summarizing estimates, SEs, z-values, and p-values.}
#' \item{vcov}{Variance-covariance matrix of the estimates (if \code{hessian = TRUE}).}
#' \item{loglik}{Log-likelihood value at the maximum.}
#' \item{AIC}{Akaike Information Criterion.}
#' \item{BIC}{Bayesian Information Criterion.}
#' \item{AICc}{Corrected Akaike Information Criterion.}
#' \item{data}{The input data vector used for fitting.}
#' \item{nobs}{Number of observations used.}
#' \item{df}{Number of estimated parameters.}
#' \item{convergence}{Logical indicating successful convergence.}
#' \item{message}{Convergence message from the optimizer.}
#' \item{family}{The specified distribution family.}
#' \item{method}{The specific optimization method used.}
#' \item{conf.int}{Data frame with confidence intervals (if \code{hessian = TRUE}).}
#' \item{conf.level}{The confidence level used.}
#' \item{optimizer}{The raw output object from the optimizer function.}
#' \item{obj}{The TMB object used for fitting (if available).}
#' \item{fixed}{The list of fixed parameters used.}
#' \item{profile}{A list containing likelihood profile results (if \code{profile = TRUE}).}
#' \item{submodels}{A list of fitted submodels (if \code{submodels = TRUE}).}
#' \item{lrt}{A list of likelihood ratio test results comparing nested models (if \code{submodels = TRUE}).}
#' \item{gof}{Goodness-of-fit statistics (e.g., AD, CvM, KS).}
#' \item{diagnostics}{Diagnostic information related to GOF tests.}
#' \item{plots}{A list or `patchwork` object containing ggplot objects for diagnostics (if \code{plot = TRUE}).}
#' \item{call}{The matched function call.}
#'
#' @details
#' The \code{gkwfit} function provides a unified interface for fitting the seven distributions in the Generalized Kumaraswamy family:
#' \itemize{
#'   \item \strong{GKw}: 5 parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) - All positive.
#'   \item \strong{BKw}: 4 parameters (\eqn{\alpha, \beta, \gamma, \delta}), \eqn{\lambda = 1} fixed - All positive.
#'   \item \strong{KKw}: 4 parameters (\eqn{\alpha, \beta, \delta, \lambda}), \eqn{\gamma = 1} fixed - All positive.
#'   \item \strong{EKw}: 3 parameters (\eqn{\alpha, \beta, \lambda}), \eqn{\gamma = 1, \delta = 0} fixed - All positive.
#'   \item \strong{Mc} (McDonald / Beta Power): 3 parameters (\eqn{\gamma, \delta, \lambda}), \eqn{\alpha = 1, \beta = 1} fixed - All positive.
#'   \item \strong{Kw} (Kumaraswamy): 2 parameters (\eqn{\alpha, \beta}), \eqn{\gamma = 1, \delta = 0, \lambda = 1} fixed - All positive.
#'   \item \strong{Beta}: 2 parameters (\eqn{\gamma, \delta}), \eqn{\alpha = 1, \beta = 1, \lambda = 1} fixed - All positive. (\eqn{\gamma, \delta} correspond to standard Beta shape1, shape2).
#' }
#'
#' This function uses Template Model Builder (TMB) for parameter estimation, which provides accurate and efficient automatic differentiation.
#'
#' **Optimizer Method (`method` argument):**
#' \itemize{
#'   \item \code{"nlminb"}: Uses R's built-in `stats::nlminb` optimizer. Good for problems with box constraints. Default option.
#'   \item \code{"Nelder-Mead"}: Uses R's `stats::optim` with the Nelder-Mead simplex algorithm, which doesn't require derivatives.
#'   \item \code{"BFGS"}: Uses R's `stats::optim` with the BFGS quasi-Newton method for unconstrained optimization.
#'   \item \code{"CG"}: Uses R's `stats::optim` with conjugate gradients method for unconstrained optimization.
#'   \item \code{"L-BFGS-B"}: Uses R's `stats::optim` with the limited-memory BFGS method with box constraints.
#'   \item \code{"SANN"}: Uses R's stats::optim with simulated annealing, a global optimization method useful for problems with multiple local minima.
#' }
#'
#' **Optimizer Control (`optimizer.control`):**
#' Pass a list with parameters specific to the chosen optimizer:
#' \itemize{
#'   \item For \code{method = "nlminb"}: Controls are passed to `stats::nlminb`. See `?nlminb` for options like `eval.max`, `iter.max`, `trace`, `rel.tol`, etc.
#'   \item For other methods: Controls are passed to `stats::optim`. See `?optim` for options like `maxit`, `trace`, `factr`, `pgtol`, etc.
#' }
#' If `optimizer.control` is empty, reasonable defaults are used for each method.
#'
#' **Data Preprocessing:** The function includes basic validation to ensure data is numeric and within (0, 1). It attempts to adjust values exactly at 0 or 1 by a small epsilon (`.Machine$double.eps^0.5`) with a warning, but stops if more than 10% of data needs adjustment. It's generally recommended to handle boundary data appropriately *before* calling `gkwfit`.
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(ggplot2)
#' library(patchwork)
#' library(betareg)
#'
#' # ============================================================================
#' # EXAMPLE 1: Basic Usage with Simulated Data for Different Distributions
#' # ============================================================================
#' # Set seed for reproducibility
#' set.seed(2203)
#' n <- 1000
#'
#' # Generate random samples from various distributions in the GKw family
#' y_gkw <- rgkw(n, alpha = 2, beta = 3, gamma = 1.5, delta = 0.5, lambda = 1.2)
#' y_bkw <- rbkw(n, alpha = 2, beta = 3, gamma = 1.5, delta = 0.5) # BKw has lambda fixed at 1
#' y_kw <- rkw(n, alpha = 2, beta = 3) # Standard Kumaraswamy (gamma=1, delta=0, lambda=1)
#' y_beta <- rbeta_(n, gamma = 2, delta = 3) # Beta with shape1=2, shape2=3
#'
#' # Calculate densities for the first 5 observations
#' head(dgkw(y_gkw[1:5], alpha = 2, beta = 3, gamma = 1.5, delta = 0.5, lambda = 1.2))
#'
#' # Compare with beta density (note different parameterization)
#' head(dbeta_(y_beta[1:5], gamma = 2, delta = 3))
#'
#' # Compute log-likelihood using parameter vector format
#' # Parameter order: alpha, beta, gamma, delta, lambda
#' par_gkw <- c(2, 3, 1.5, 0.5, 1.2)
#' ll_gkw <- llgkw(par_gkw, y_gkw)
#'
#' par_kw <- c(2, 3) # Kumaraswamy parameters
#' ll_kw <- llkw(par_kw, y_kw)
#'
#' cat("Log-likelihood GKw:", ll_gkw, "\nLog-likelihood Kw:", ll_kw, "\n")
#'
#' # ============================================================================
#' # EXAMPLE 2: Visualization and Distribution Comparisons
#' # ============================================================================
#' # Generate data for plotting
#' x_vals <- seq(0.001, 0.999, length.out = 500)
#'
#' # Calculate densities for different distributions
#' dens_gkw <- dgkw(x_vals, alpha = 2, beta = 3, gamma = 1.5, delta = 0.5, lambda = 1.2)
#' dens_bkw <- dbkw(x_vals, alpha = 2, beta = 3, gamma = 1.5, delta = 0.5)
#' dens_kw <- dkw(x_vals, alpha = 2, beta = 3)
#' dens_beta <- dbeta_(x_vals, gamma = 2, delta = 3)
#'
#' # Create and display plot
#' df <- data.frame(
#'   x = rep(x_vals, 4),
#'   density = c(dens_gkw, dens_bkw, dens_kw, dens_beta),
#'   Distribution = rep(c("GKw", "BKw", "Kw", "Beta"), each = length(x_vals))
#' )
#'
#' p <- ggplot(df, aes(x = x, y = density, color = Distribution)) +
#'   geom_line(linewidth = 1) +
#'   theme_minimal() +
#'   labs(
#'     title = "Density Comparison of GKw Distribution Family",
#'     x = "x", y = "Density"
#'   )
#'
#' print(p)
#'
#' # Examine quantile functions
#' # Calculate 0.25, 0.5, and 0.75 quantiles for each distribution
#' probs <- c(0.25, 0.5, 0.75)
#' qgkw_values <- qgkw(probs, alpha = 2, beta = 3, gamma = 1.5, delta = 0.5, lambda = 1.2)
#' qkw_values <- qkw(probs, alpha = 2, beta = 3)
#' qbeta_values <- qbeta_(probs, gamma = 2, delta = 3)
#'
#' # Display the quantiles
#' quantile_df <- data.frame(
#'   Probability = probs,
#'   GKw = qgkw_values,
#'   Kw = qkw_values,
#'   Beta = qbeta_values
#' )
#' print(quantile_df)
#'
#' # ============================================================================
#' # EXAMPLE 3: Model Fitting with Simulated Data
#' # ============================================================================
#' # Simulate data from GKw
#' set.seed(2203)
#' true_params <- list(alpha = 2.5, beta = 1.8, gamma = 1.3, delta = 0.4, lambda = 1.5)
#' y <- rgkw(n,
#'   alpha = true_params$alpha,
#'   beta = true_params$beta,
#'   gamma = true_params$gamma,
#'   delta = true_params$delta,
#'   lambda = true_params$lambda
#' )
#'
#' # Fit full GKw model
#' fit_gkw <- gkwfit(data = y, family = "gkw")
#' summary(fit_gkw)
#'
#' # Fit restricted models for comparison
#' fit_bkw <- gkwfit(data = y, family = "bkw")
#' fit_kkw <- gkwfit(data = y, family = "kkw")
#' fit_kw <- gkwfit(data = y, family = "kw")
#'
#' # Compare models using AIC
#' models <- c("GKw", "BKw", "KKw", "Kw")
#' AIC_values <- c(fit_gkw$AIC, fit_bkw$AIC, fit_kkw$AIC, fit_kw$AIC)
#' model_comparison <- data.frame(Model = models, AIC = AIC_values)
#' model_comparison <- model_comparison[order(model_comparison$AIC), ]
#' print(model_comparison)
#'
#' # ============================================================================
#' # EXAMPLE 4: Fixed Parameter Estimation and Likelihood Ratio Tests
#' # ============================================================================
#' # Simulate data where delta = 0
#' set.seed(2203)
#' true_params <- list(alpha = 1.8, beta = 2.2, gamma = 0.9, delta = 0, lambda = 1.2)
#' y <- rgkw(n,
#'   alpha = true_params$alpha,
#'   beta = true_params$beta,
#'   gamma = true_params$gamma,
#'   delta = true_params$delta,
#'   lambda = true_params$lambda
#' )
#'
#' # Fit model with delta fixed at 0
#' fit_fixed <- gkwfit(data = y, family = "gkw", method = "L-BFGS-B", fixed = list(delta = 0.5))
#' summary(fit_fixed)
#'
#' # Fit model without fixing delta (should estimate close to 0)
#' fit_free <- gkwfit(data = y, family = "gkw")
#' summary(fit_free)
#'
#' # Compare models via likelihood ratio test
#' # H0: delta = 0 vs H1: delta != 0
#' LR_stat <- -2 * (fit_fixed$loglik - fit_free$loglik)
#' p_value <- 1 - pchisq(LR_stat, df = 1)
#' cat("LR test statistic:", LR_stat, "\np-value:", p_value, "\n")
#'
#' # ============================================================================
#' # EXAMPLE 5: Profile Likelihood Analysis
#' # ============================================================================
#' set.seed(2203)
#' y <- rgkw(n, alpha = 2, beta = 3, gamma = 1.5, delta = 0, lambda = 1.2)
#'
#' # Fit with profile likelihood
#' fit_profile <- gkwfit(
#'   data = y,
#'   family = "gkw",
#'   profile = TRUE,
#'   npoints = 15
#' )
#'
#' # Examine profile objects
#' str(fit_profile$profile)
#' fit_profile$plots
#'
#' # ============================================================================
#' # EXAMPLE 6: Real Data Application with Beta Regression Datasets
#' # ============================================================================
#' # Example 1: Reading Skills data
#' data("ReadingSkills", package = "betareg")
#' y <- ReadingSkills$accuracy
#'
#' # Summary statistics of the response variable
#' summary(y)
#'
#' # Fit different distributions to this data
#' fit_rs_beta <- gkwfit(data = y, family = "beta")
#' fit_rs_kw <- gkwfit(data = y, family = "kw")
#' fit_rs_gkw <- gkwfit(data = y, family = "gkw")
#'
#' # Model comparison
#' rs_models <- c("Beta", "Kumaraswamy", "GKw")
#' rs_AICs <- c(fit_rs_beta$AIC, fit_rs_kw$AIC, fit_rs_gkw$AIC)
#' rs_BICs <- c(fit_rs_beta$BIC, fit_rs_kw$BIC, fit_rs_gkw$BIC)
#' rs_comparison <- data.frame(
#'   Model = rs_models,
#'   AIC = rs_AICs,
#'   BIC = rs_BICs,
#'   Parameters = c(
#'     length(coef(fit_rs_beta)),
#'     length(coef(fit_rs_kw)),
#'     length(coef(fit_rs_gkw))
#'   )
#' )
#' print(rs_comparison[order(rs_comparison$AIC), ])
#'
#' # Example 2: Gasoline Yield data
#' data("GasolineYield", package = "betareg")
#' y <- GasolineYield$yield
#'
#' # Check range and make adjustments if needed (data must be in (0,1))
#' if (min(y) <= 0 || max(y) >= 1) {
#'   # Apply common transformation for proportions that include 0 or 1
#'   n_obs <- length(y)
#'   y <- (y * (n_obs - 1) + 0.5) / n_obs
#' }
#'
#' # Fit best model based on previous comparison
#' best_family <- rs_comparison$Model[1]
#' fit_gas <- gkwfit(data = y, family = tolower(best_family))
#' summary(fit_gas)
#'
#' # Plot fitted density over histogram of data
#' # Get parameters from fitted model
#' params <- coef(fit_gas)
#'
#' # Generate x values and calculate density
#' x_seq <- seq(min(y), max(y), length.out = 100)
#' fitted_density <- switch(tolower(best_family),
#'   "beta" = dbeta_(x_seq, gamma = params["gamma"], delta = params["delta"]),
#'   "kumaraswamy" = dkw(x_seq, alpha = params["alpha"], beta = params["beta"]),
#'   "gkw" = dgkw(x_seq,
#'     alpha = params["alpha"], beta = params["beta"],
#'     gamma = params["gamma"], delta = params["delta"],
#'     lambda = params["lambda"]
#'   )
#' )
#'
#' # Create data frame for plotting
#' df_plot <- data.frame(x = x_seq, density = fitted_density)
#'
#' # Create plot
#' p <- ggplot() +
#'   geom_histogram(
#'     data = data.frame(y = y), aes(x = y, y = after_stat(density)),
#'     bins = 30, fill = "lightblue", color = "black", alpha = 0.7
#'   ) +
#'   geom_line(data = df_plot, aes(x = x, y = density), color = "red", size = 1) +
#'   labs(
#'     title = paste("Fitted", best_family, "Distribution to Gasoline Yield Data"),
#'     x = "Yield",
#'     y = "Density"
#'   ) +
#'   theme_minimal()
#'
#' print(p)
#'
#' # ============================================================================
#' # EXAMPLE 7: Beta Distribution Variants and Special Functions
#' # ============================================================================
#' # Generate samples from beta distribution
#' set.seed(2203)
#' y_beta <- rbeta_(n, gamma = 2, delta = 3)
#'
#' # Calculate density and log-likelihood
#' beta_dens <- dbeta_(y_beta[1:5], gamma = 2, delta = 3)
#'
#' # Using parameter vector format for log-likelihood (gamma, delta)
#' par_beta <- c(2, 3)
#' beta_ll <- llbeta(par_beta, y_beta)
#' cat("Beta density (first 5):", beta_dens, "\n")
#' cat("Beta log-likelihood:", beta_ll, "\n")
#'
#' # Gradient of log-likelihood with respect to parameters
#' beta_grad <- grbeta(par_beta, y_beta)
#' cat("Gradient of Beta log-likelihood:\n")
#' print(beta_grad)
#'
#' # Hessian of log-likelihood with respect to parameters
#' beta_hess <- hsbeta(par_beta, y_beta)
#' cat("Hessian of Beta log-likelihood:\n")
#' print(beta_hess)
#'
#' # ============================================================================
#' # EXAMPLE 8: Gradient and Hessian Functions for GKw Distribution
#' # ============================================================================
#' # Set seed and generate data
#' set.seed(2203)
#'
#' # Define parameters
#' alpha <- 2
#' beta <- 1.5
#' gamma <- 1.2
#' delta <- 0.3
#' lambda <- 1.1
#' par_gkw <- c(alpha, beta, gamma, delta, lambda)
#'
#' # Generate random sample
#' y <- rgkw(n, alpha, beta, gamma, delta, lambda)
#'
#' # Calculate log-likelihood of the sample using parameter vector format
#' ll <- llgkw(par_gkw, y)
#' cat("GKw log-likelihood:", ll, "\n")
#'
#' # Calculate gradient of log-likelihood
#' gr <- grgkw(par_gkw, y)
#' cat("GKw log-likelihood gradient:\n")
#' print(gr)
#'
#' # Calculate Hessian matrix of log-likelihood
#' hs <- hsgkw(par_gkw, y)
#' cat("GKw log-likelihood Hessian:\n")
#' print(hs)
#'
#' # ============================================================================
#' # EXAMPLE 9: Optimization with Custom Gradient and Hessian
#' # ============================================================================
#' # Manual optimization demonstration
#' set.seed(2203)
#'
#' # Generate data from a known distribution
#' true_par <- c(alpha = 1.8, beta = 2.5, gamma = 1.3, delta = 0.2, lambda = 1.1)
#' y <- rgkw(n,
#'   alpha = true_par["alpha"],
#'   beta = true_par["beta"],
#'   gamma = true_par["gamma"],
#'   delta = true_par["delta"],
#'   lambda = true_par["lambda"]
#' )
#'
#' # Define the negative log-likelihood function (for minimization)
#' nll <- function(log_par) {
#'   # Transform from log-scale to natural scale (ensures positivity)
#'   par <- exp(log_par)
#'
#'   # Return negative log-likelihood
#'   -llgkw(par, y)
#' }
#'
#' # Define the gradient function using analytical gradient
#' gr_func <- function(log_par) {
#'   # Transform parameters
#'   par <- exp(log_par)
#'
#'   # Get the gradient with respect to the original parameters
#'   gradient <- grgkw(par, y)
#'
#'   # Apply chain rule for the log transformation
#'   gradient <- gradient * par
#'
#'   # Return negative gradient for minimization
#'   gradient
#' }
#'
#' # Starting values (on log scale to ensure positivity)
#' start_log_par <- log(c(1, 1, 1, 0.1, 1))
#'
#' # Optimize using L-BFGS-B method with analytic gradient
#' opt_result <- optim(
#'   par = start_log_par,
#'   fn = nll,
#'   gr = gr_func,
#'   method = "BFGS",
#'   control = list(trace = 1, maxit = 100)
#' )
#'
#' # Transform parameters back to original scale
#' estimated_par <- exp(opt_result$par)
#' names(estimated_par) <- c("alpha", "beta", "gamma", "delta", "lambda")
#'
#' # Compare with true parameters
#' params_comparison <- data.frame(
#'   True = true_par,
#'   Estimated = estimated_par,
#'   Absolute_Error = abs(true_par - estimated_par),
#'   Relative_Error = abs((true_par - estimated_par) / true_par)
#' )
#' print(params_comparison)
#'
#' # ============================================================================
#' # EXAMPLE 10: Third Betareg Dataset (ImpreciseTask) and McDonald Distribution
#' # ============================================================================
#' data("ImpreciseTask", package = "betareg")
#' y <- ImpreciseTask$location
#'
#' # Make sure data is within (0, 1)
#' if (min(y) <= 0 || max(y) >= 1) {
#'   # Apply common transformation for proportions
#'   n_obs <- length(y)
#'   y <- (y * (n_obs - 1) + 0.5) / n_obs
#' }
#'
#' # Fit models from the GKw family
#' fit_beta <- gkwfit(data = y, family = "beta")
#' fit_kw <- gkwfit(data = y, family = "kw")
#' fit_mc <- gkwfit(data = y, family = "mc") # McDonald distribution
#'
#' # Compare information criteria
#' ic_comparison <- data.frame(
#'   Model = c("Beta", "Kumaraswamy", "McDonald"),
#'   AIC = c(fit_beta$AIC, fit_kw$AIC, fit_mc$AIC),
#'   BIC = c(fit_beta$BIC, fit_kw$BIC, fit_mc$BIC),
#'   LogLik = c(fit_beta$loglik, fit_kw$loglik, fit_mc$loglik)
#' )
#' print(ic_comparison[order(ic_comparison$AIC), ])
#'
#' # Get best model
#' best_model <- ic_comparison$Model[which.min(ic_comparison$AIC)]
#' best_fit <- switch(tolower(best_model),
#'   "beta" = fit_beta,
#'   "kumaraswamy" = fit_kw,
#'   "mcdonald" = fit_mc
#' )
#'
#' # Goodness of fit tests
#' print(paste("Best model:", best_model))
#' print(best_fit$gof)
#'
#' # Generate values from fitted McDonald distribution (if it's the best model)
#' if (best_model == "McDonald") {
#'   # McDonald's parameters: gamma, delta, lambda (alpha=1, beta=1 fixed)
#'   gamma_mc <- coef(fit_mc)["gamma"]
#'   delta_mc <- coef(fit_mc)["delta"]
#'   lambda_mc <- coef(fit_mc)["lambda"]
#'
#'   # Generate new sample using fitted parameters
#'   set.seed(2203)
#'   y_mc_simulated <- rmc(n, gamma = gamma_mc, delta = delta_mc, lambda = lambda_mc)
#'
#'   # Compare histograms of original and simulated data
#'   df_orig <- data.frame(value = y, type = "Original")
#'   df_sim <- data.frame(value = y_mc_simulated, type = "Simulated")
#'   df_combined <- rbind(df_orig, df_sim)
#'
#'   # Create comparative histogram
#'   p <- ggplot(df_combined, aes(x = value, fill = type)) +
#'     geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
#'     labs(
#'       title = "Comparison of Original Data and Simulated McDonald Distribution",
#'       x = "Value",
#'       y = "Count"
#'     ) +
#'     theme_minimal()
#'
#'   print(p)
#' }
#'
#' # ============================================================================
#' # EXAMPLE 11: Working with Alternative Distributions in the GKw Family
#' # ============================================================================
#' # Explore EKw - Exponential Kumaraswamy distribution (alpha, beta, lambda)
#' set.seed(2203)
#'
#' # Generate EKw sample
#' y_ekw <- rekw(n, alpha = 2.5, beta = 1.5, lambda = 2.0)
#'
#' # Calculate density and distribution function
#' ekw_density <- dekw(y_ekw[1:5], alpha = 2.5, beta = 1.5, lambda = 2.0)
#' ekw_cdf <- pekw(y_ekw[1:5], alpha = 2.5, beta = 1.5, lambda = 2.0)
#'
#' # Calculate log-likelihood
#' par_ekw <- c(2.5, 1.5, 2.0) # alpha, beta, lambda for EKw
#' ll_ekw <- llekw(par_ekw, y_ekw)
#'
#' cat("EKw density (first 5):", ekw_density, "\n")
#' cat("EKw CDF (first 5):", ekw_cdf, "\n")
#' cat("EKw log-likelihood:", ll_ekw, "\n")
#'
#' # Calculate gradient and Hessian
#' gr_ekw <- grekw(par_ekw, y_ekw)
#' hs_ekw <- hsekw(par_ekw, y_ekw)
#'
#' cat("EKw gradient:\n")
#' print(gr_ekw)
#'
#' cat("EKw Hessian (first 2x2):\n")
#' print(hs_ekw)
#'
#' # Fit EKw model to data
#' fit_ekw <- gkwfit(data = y_ekw, family = "ekw")
#' summary(fit_ekw)
#'
#' # Compare with true parameters
#' cat("True parameters: alpha=2.5, beta=1.5, lambda=2.0\n")
#' cat("Estimated parameters:\n")
#' print(coef(fit_ekw))
#'
#' # ============================================================================
#' # EXAMPLE 12: Comprehensive Parameter Recovery Simulation
#' # ============================================================================
#' # Function to simulate data and recover parameters
#' simulate_and_recover <- function(family, true_params, n = 1000) {
#'   set.seed(2203)
#'
#'   # Generate data based on family
#'   y <- switch(family,
#'     "gkw" = rgkw(n,
#'       alpha = true_params[1], beta = true_params[2],
#'       gamma = true_params[3], delta = true_params[4],
#'       lambda = true_params[5]
#'     ),
#'     "bkw" = rbkw(n,
#'       alpha = true_params[1], beta = true_params[2],
#'       gamma = true_params[3], delta = true_params[4]
#'     ),
#'     "kw" = rkw(n, alpha = true_params[1], beta = true_params[2]),
#'     "beta" = rbeta_(n, gamma = true_params[1], delta = true_params[2])
#'   )
#'
#'   # Fit model
#'   fit <- gkwfit(data = y, family = family, silent = TRUE)
#'
#'   # Return comparison
#'   list(
#'     family = family,
#'     true = true_params,
#'     estimated = coef(fit),
#'     loglik = fit$loglik,
#'     AIC = fit$AIC,
#'     converged = fit$convergence
#'   )
#' }
#'
#' # Define true parameters for each family
#' params_gkw <- c(alpha = 2.0, beta = 3.0, gamma = 1.5, delta = 0.5, lambda = 1.2)
#' params_bkw <- c(alpha = 2.5, beta = 1.8, gamma = 1.2, delta = 0.3)
#' params_kw <- c(alpha = 1.5, beta = 2.0)
#' params_beta <- c(gamma = 2.0, delta = 3.0)
#'
#' # Run simulations
#' result_gkw <- simulate_and_recover("gkw", params_gkw)
#' result_bkw <- simulate_and_recover("bkw", params_bkw)
#' result_kw <- simulate_and_recover("kw", params_kw)
#' result_beta <- simulate_and_recover("beta", params_beta)
#'
#' # Create summary table
#' create_comparison_df <- function(result) {
#'   param_names <- names(result$true)
#'   df <- data.frame(
#'     Parameter = param_names,
#'     True = result$true,
#'     Estimated = result$estimated[param_names],
#'     Abs_Error = abs(result$true - result$estimated[param_names]),
#'     Rel_Error = abs((result$true - result$estimated[param_names]) / result$true) * 100
#'   )
#'   return(df)
#' }
#'
#' # Print results
#' cat("===== GKw Parameter Recovery =====\n")
#' print(create_comparison_df(result_gkw))
#' cat("\n===== BKw Parameter Recovery =====\n")
#' print(create_comparison_df(result_bkw))
#' cat("\n===== Kw Parameter Recovery =====\n")
#' print(create_comparison_df(result_kw))
#' cat("\n===== Beta Parameter Recovery =====\n")
#' print(create_comparison_df(result_beta))
#' }
#'
#' @references
#' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded
#' random processes. *Journal of Hydrology*, 46(1-2), 79-88. \doi{10.1016/0022-1694(80)90036-0}
#'
#' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
#' *Journal of Statistical Computation and Simulation*, 81(7), 883-898. \doi{10.1080/00949650903530745}
#'
#' Kristensen, K., Nielsen, A., Berg, C. W., Skaug, H., & Bell, B. M. (2016). TMB: Automatic Differentiation and Laplace Approximation. *Journal of Statistical Software*, 70(5), 1–21. \doi{10.18637/jss.v070.i05}
#'
#' @seealso User-facing S3 methods: \code{\link{summary.gkwfit}}, \code{\link{print.gkwfit}}, \code{\link{plot.gkwfit}}, \code{\link{coef.gkwfit}}, \code{\link{vcov.gkwfit}}, \code{\link{logLik.gkwfit}}, \code{\link{confint.gkwfit}}. Density/distribution functions: \code{\link{dgkw}}, \code{\link{pgkw}}, \code{\link{qgkw}}, \code{\link{rgkw}}.
#' @keywords distribution models mle optimization
#' @author Lopes, J. E.
#' @export
gkwfit <- function(data,
                   family = "gkw",
                   start = NULL,
                   fixed = NULL,
                   method = "nlminb",
                   use_moments = FALSE,
                   hessian = TRUE,
                   profile = FALSE,
                   npoints = 20,
                   plot = TRUE,
                   conf.level = 0.95,
                   optimizer.control = list(),
                   submodels = FALSE,
                   silent = TRUE,
                   ...) {
  # --- Argument Matching and Validation ---
  call <- match.call()
  family <- match.arg(family, choices = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))

  # Validate optimization method
  method <- match.arg(method, choices = c("nlminb", "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"))

  # Determine if we're using nlminb or optim (with the specified method)
  use_nlminb <- method == "nlminb"

  # Package checks
  if (!requireNamespace("TMB", quietly = TRUE)) {
    stop("Package 'TMB' is required but not installed. Please install it with install.packages('TMB')")
  }

  if (profile && !requireNamespace("TMB", quietly = TRUE)) {
    warning("Package 'TMB' needed for profile likelihoods. Setting profile=FALSE.")
    profile <- FALSE
  }

  if (plot && (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("patchwork", quietly = TRUE))) {
    warning("Packages 'ggplot2' and 'patchwork' needed for plot=TRUE. Setting plot=FALSE.")
    plot <- FALSE
  }

  # Parameter validation
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a number between 0 and 1")
  }
  if (!is.numeric(npoints) || npoints < 5) {
    stop("npoints must be a positive integer >= 5")
  }
  npoints <- as.integer(npoints)

  # --- Data and Initial Parameter Setup ---
  param_info <- .get_family_param_info(family)
  n_params <- param_info$n
  param_names <- param_info$names

  data <- .validate_data(data, n_params) # Validate data early

  result <- list(call = call, family = family) # Initialize result list

  # Determine initial values and merge fixed parameters
  start_info <- .determine_start_values(data, start, fixed, family, use_moments, silent)
  start <- start_info$start # This is now a validated list for the specific family
  fixed <- start_info$fixed # This now includes user + family default fixed

  # --- Core Fitting Call ---
  if (!silent) message("Fitting model using TMB with method: ", method)

  # Check and compile TMB code
  tryCatch(
    {
      .check_and_compile_TMB_code("gkwmletmb", verbose = !silent)
    },
    error = function(e) {
      stop("Failed to compile TMB code: ", e$message)
    }
  )

  if (!silent) {
    message("Checking TMB model compilation...")
  }

  # Get the numeric code for family
  family_code <- .family_to_code(family)

  # Create full parameter list for TMB
  full_start_tmb <- list(
    log_alpha = log(1), # Default values, will be overwritten if needed
    log_beta = log(1),
    log_gamma = log(1),
    log_delta = log(0.0001), # Small value for delta to avoid log(0)
    log_lambda = log(1)
  )

  # Update with user-provided start values
  for (param in names(start)) {
    full_start_tmb[[paste0("log_", param)]] <- log(start[[param]])
  }

  # Apply fixed parameters for TMB
  map <- list()

  # Add family-specific fixed parameters to the map
  all_fixed_params <- c(names(fixed), setdiff(
    c("alpha", "beta", "gamma", "delta", "lambda"),
    c(param_names, names(fixed))
  ))

  for (param in all_fixed_params) {
    param_name <- paste0("log_", param)
    if (param %in% names(fixed)) {
      # User or family provided fixed value
      full_start_tmb[[param_name]] <- log(fixed[[param]])
    } else if (param == "alpha" || param == "beta") {
      # Default alpha, beta = 1 when not specified
      full_start_tmb[[param_name]] <- log(1)
    } else if (param == "gamma") {
      # Default gamma = 1 when not specified
      full_start_tmb[[param_name]] <- log(1)
    } else if (param == "delta") {
      # Default delta = 0 when not specified (use small value to avoid log(0))
      full_start_tmb[[param_name]] <- log(0.0001)
    } else if (param == "lambda") {
      # Default lambda = 1 when not specified
      full_start_tmb[[param_name]] <- log(1)
    }

    # Add to map to keep parameter fixed
    map[[param_name]] <- factor(NA)
  }

  # Prepare data for TMB
  tmb_data <- list(
    x = data,
    family = family_code
  )

  # Create TMB object
  obj <- tryCatch(
    {
      TMB::MakeADFun(
        data = tmb_data,
        parameters = full_start_tmb,
        map = if (length(map) > 0) map else NULL,
        DLL = "gkwmletmb",
        silent = silent
      )
    },
    error = function(e) {
      stop("Error creating TMB model: ", e$message)
    }
  )

  # Set up optimizer controls based on the chosen method
  if (use_nlminb) {
    control_defaults <- list(eval.max = 500, iter.max = 300, trace = ifelse(silent, 0, 1))
  } else { # optim methods
    control_defaults <- list(maxit = 500, trace = ifelse(silent, 0, 1))
  }

  # Merge user controls with defaults, with user controls taking precedence
  control <- utils::modifyList(control_defaults, optimizer.control)

  # Run optimization
  if (use_nlminb) {
    opt <- tryCatch(
      {
        stats::nlminb(
          start = obj$par,
          objective = obj$fn,
          gradient = obj$gr,
          control = control
        )
      },
      error = function(e) {
        stop("Optimization with nlminb failed: ", e$message)
      }
    )

    opt$convergence <- opt$convergence == 0
    opt$message <- opt$message
    opt$objective <- opt$objective
  } else {
    opt <- tryCatch(
      {
        stats::optim(
          par = obj$par,
          fn = obj$fn,
          gr = obj$gr,
          method = method,
          control = control
        )
      },
      error = function(e) {
        stop(paste("Optimization with optim method", method, "failed:", e$message))
      }
    )

    opt$objective <- opt$value
    opt$convergence <- opt$convergence == 0
    opt$message <- if (opt$convergence) "Successful convergence" else "Optimization failed to converge"
  }

  if (!opt$convergence) {
    warning("Model did not converge: ", opt$message)
  }

  # Get parameter estimates for all parameters
  all_params <- exp(opt$par)
  names(all_params) <- sub("log_", "", names(opt$par))

  # Filter parameters to include only those relevant for the family
  filtered_coefficients <- all_params[paste0("log_", param_names) %in% names(opt$par)]
  names(filtered_coefficients) <- param_names[param_names %in% sub("log_", "", names(opt$par))]

  # If some parameters are missing (fixed), add them from the fixed list
  for (param in param_names) {
    if (!param %in% names(filtered_coefficients)) {
      if (param %in% names(fixed)) {
        filtered_coefficients[param] <- fixed[[param]]
      } else {
        # Use default values for missing parameters
        if (param == "alpha" || param == "beta" || param == "gamma" || param == "lambda") {
          filtered_coefficients[param] <- 1.0
        } else if (param == "delta") {
          filtered_coefficients[param] <- 0.0
        }
      }
    }
  }

  # Ensure the order matches param_names
  filtered_coefficients <- filtered_coefficients[param_names]

  # Calculate standard errors and Hessian if requested
  filtered_std_errors <- rep(NA, length(param_names))
  names(filtered_std_errors) <- param_names

  cov_matrix <- NULL
  coef_summary <- NULL
  conf_int <- NULL

  if (hessian) {
    sd_report <- tryCatch(
      {
        TMB::sdreport(obj)
      },
      error = function(e) {
        warning("Error calculating standard errors: ", e$message)
        NULL
      }
    )

    if (!is.null(sd_report) && !is.character(sd_report)) {
      # Extract parameter estimates, SEs, and covariance matrix
      cov_matrix <- sd_report$cov.fixed
      std_errors_log <- sqrt(diag(cov_matrix))

      # Get the SEs only for non-fixed parameters
      for (i in seq_along(param_names)) {
        param <- param_names[i]
        log_param <- paste0("log_", param)

        if (log_param %in% names(std_errors_log)) {
          # Use the Delta method to transform SEs to original scale
          filtered_std_errors[i] <- std_errors_log[log_param] * filtered_coefficients[i]
        }
      }

      # Create coefficient summary
      coef_summary <- data.frame(
        Estimate = filtered_coefficients,
        `Std. Error` = filtered_std_errors,
        `z value` = filtered_coefficients / filtered_std_errors,
        `Pr(>|z|)` = 2 * stats::pnorm(abs(filtered_coefficients / filtered_std_errors), lower.tail = FALSE),
        row.names = param_names,
        check.names = FALSE
      )

      # Calculate confidence intervals
      z_value <- stats::qnorm(1 - (1 - conf.level) / 2)
      conf_int_params <- character()
      conf_int_estimates <- numeric()
      conf_int_se <- numeric()
      conf_int_lower <- numeric()
      conf_int_upper <- numeric()

      for (i in seq_along(param_names)) {
        if (!is.na(filtered_std_errors[i])) {
          param <- param_names[i]
          conf_int_params <- c(conf_int_params, param)
          conf_int_estimates <- c(conf_int_estimates, filtered_coefficients[i])
          conf_int_se <- c(conf_int_se, filtered_std_errors[i])

          # For delta, lower bound is 0, for others it's machine epsilon
          if (param == "delta") {
            lower <- max(filtered_coefficients[i] - z_value * filtered_std_errors[i], 0)
          } else {
            lower <- max(filtered_coefficients[i] - z_value * filtered_std_errors[i], .Machine$double.eps)
          }

          upper <- filtered_coefficients[i] + z_value * filtered_std_errors[i]
          conf_int_lower <- c(conf_int_lower, lower)
          conf_int_upper <- c(conf_int_upper, upper)
        }
      }

      if (length(conf_int_params) > 0) {
        conf_int <- data.frame(
          parameter = conf_int_params,
          estimate = conf_int_estimates,
          std.error = conf_int_se,
          lower = conf_int_lower,
          upper = conf_int_upper,
          row.names = NULL, check.names = FALSE
        )
      }
    } else {
      warning("Hessian calculation failed, standard errors not available")

      # Create coefficient summary without SEs
      coef_summary <- data.frame(
        Estimate = filtered_coefficients,
        `Std. Error` = filtered_std_errors,
        `z value` = rep(NA, length(param_names)),
        `Pr(>|z|)` = rep(NA, length(param_names)),
        row.names = param_names,
        check.names = FALSE
      )
    }
  } else {
    # If hessian = FALSE, only report parameter estimates
    coef_summary <- data.frame(
      Estimate = filtered_coefficients,
      `Std. Error` = filtered_std_errors,
      `z value` = rep(NA, length(param_names)),
      `Pr(>|z|)` = rep(NA, length(param_names)),
      row.names = param_names,
      check.names = FALSE
    )
  }

  # Calculate log likelihood, AIC, BIC
  loglik <- -opt$objective
  n <- length(data)
  k <- sum(!is.na(opt$par)) # Count non-fixed parameters
  aic <- -2 * loglik + 2 * k
  bic <- -2 * loglik + log(n) * k
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)

  # Compile core results
  fit_result <- list(
    coefficients = filtered_coefficients,
    std.errors = filtered_std_errors,
    coef_summary = coef_summary,
    vcov = cov_matrix,
    loglik = loglik,
    AIC = aic,
    BIC = bic,
    AICc = aicc,
    data = data,
    nobs = n,
    df = k,
    convergence = opt$convergence,
    message = opt$message,
    method = method,
    conf.int = conf_int,
    conf.level = conf.level,
    optimizer = opt,
    obj = obj,
    fixed = fixed
  )

  # Merge fit results into the main result object
  # Preserve call and family from the initial list
  result <- c(result, fit_result[!names(fit_result) %in% names(result)])

  # --- Post-fitting Analysis ---

  # Calculate profile likelihoods if requested
  if (profile) {
    # Check if sdreport ran successfully
    if (!is.null(result$obj)) {
      prof_list <- .calculate_profiles(result, data, family, fixed, "tmb", method, npoints, silent)
      result$profile <- prof_list
    } else {
      warning("Cannot calculate profiles as TMB object was not available.")
    }
  }

  # Fit submodels if requested
  if (submodels) {
    if (!silent) message("Fitting submodels...")
    submodel_results <- .fit_submodels_tmb(data, result, method, hessian, optimizer.control, silent)
    if (!is.null(submodel_results)) {
      result$submodels <- submodel_results$submodels
      result$lrt <- submodel_results$lrt
    }
  }

  # Calculate goodness of fit tests
  if (!silent) message("Calculating goodness-of-fit statistics...")
  gof_results <- .calculate_gof(result, data, family, silent)
  result$gof <- gof_results$gof
  result$diagnostics <- gof_results$diagnostics # Store any extra diagnostics

  # Generate diagnostic plots if requested
  if (plot) {
    plots <- .generate_plots(result, data, family, silent)

    if (!is.null(plots) && requireNamespace("patchwork", quietly = TRUE)) {
      plots <- patchwork::wrap_plots(plots) +
        patchwork::plot_annotation(
          title = paste("Diagnostic Plots for Fitted", toupper(family), "Model")
        )
      result$plots <- plots
    }
  }

  # Ensure class is set correctly
  class(result) <- "gkwfit"

  # Return the final result
  if (!silent) message("Fitting complete.")
  return(result)
}





#' @title Print Method for gkwfit Objects
#'
#' @description
#' Prints a concise summary of a model fitted by the \code{\link{gkwfit}} function,
#' displaying the call, estimated coefficients, log-likelihood, AIC, BIC,
#' number of observations, and a convergence warning if applicable.
#'
#' @param x An object of class \code{"gkwfit"}, typically the result of a call to \code{\link{gkwfit}}.
#' @param digits Integer; the minimum number of significant digits to be printed in values.
#'   Defaults to \code{max(3, getOption("digits") - 3)}.
#' @param ... Additional arguments passed to underlying print methods (currently unused).
#'
#' @return Invisibly returns the original input object \code{x}. Called for its side effect of printing to the console.
#'
#' @seealso \code{\link{gkwfit}}, \code{\link{summary.gkwfit}}
#'
#' @examples
#' \donttest{
#' # Generate a small sample from Kumaraswamy distribution
#' set.seed(2203)
#' y <- rkw(50, alpha = 2.5, beta = 1.5)
#'
#' # Fit the model with minimal options for speed
#' fit <- gkwfit(data = y, family = "kw", plot = FALSE, silent = TRUE)
#'
#' # Print method displays concise model summary
#' print(fit)
#'
#' # Alternative: object prints automatically when returned
#' fit
#' }
#'
#' @keywords methods internal
#' @author Lopes, J. E.
#' @export
print.gkwfit <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Generalized Kumaraswamy Distribution Fit\n\n")

  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print.default(format(x$coefficients, digits = digits),
    print.gap = 2,
    quote = FALSE
  )

  cat("\nLog-likelihood:", formatC(x$loglik, digits = digits, format = "f"))
  cat("\nAIC:", formatC(x$AIC, digits = digits, format = "f"))
  cat("\nBIC:", formatC(x$BIC, digits = digits, format = "f"))
  cat("\nNumber of observations:", x$nobs)

  if (!x$convergence) {
    cat("\n\nWarning: Model did not converge\n")
  }

  invisible(x)
}



# Ensure stats package is implicitly available or loaded for pnorm, cov2cor etc.

#' @title Summary Method for gkwfit Objects
#'
#' @description
#' Calculates and prepares a detailed summary of a model fitted by \code{\link{gkwfit}}.
#' This includes coefficients, standard errors, test statistics (z-values), p-values,
#' log-likelihood, information criteria (AIC, BIC, AICc), number of estimated
#' parameters, convergence status, and optimizer details.
#'
#' @param object An object of class \code{"gkwfit"}, typically the result of a call
#'   to \code{\link{gkwfit}}.
#' @param correlation Logical; if \code{TRUE}, the correlation matrix of the estimated
#'   parameters is computed from the \code{vcov} component and included in the
#'   summary. Defaults to \code{FALSE}.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' This function computes standard errors, z-values (\eqn{Estimate / SE}), and
#' p-values (two-tailed test based on the standard normal distribution) for the
#' estimated model parameters by utilizing the coefficient estimates (\code{coef})
#' and their variance-covariance matrix (\code{vcov}). This requires that the
#' variance-covariance matrix was successfully computed and is available in the
#' \code{object} (typically requires \code{hessian = TRUE} in the original
#' \code{\link{gkwfit}} call and successful Hessian inversion).
#'
#' If standard errors cannot be reliably calculated (e.g., \code{vcov} is missing,
#' invalid, or indicates non-positive variance), the corresponding columns in the
#' coefficient table will contain \code{NA} values, and the \code{se_available}
#' flag will be set to \code{FALSE}.
#'
#' The returned object is of class \code{"summary.gkwfit"}, and its printing is
#' handled by \code{\link{print.summary.gkwfit}}.
#'
#' @return An object of class \code{"summary.gkwfit"}, which is a list containing:
#' \item{call}{The original function call.}
#' \item{family}{The specified distribution family.}
#' \item{coefficients}{A matrix of estimates, standard errors, z-values, and p-values. Contains NAs if SEs could not be computed.}
#' \item{loglik}{The maximized log-likelihood value (numeric).}
#' \item{df}{The number of estimated parameters.}
#' \item{aic}{Akaike Information Criterion.}
#' \item{bic}{Bayesian Information Criterion.}
#' \item{aicc}{Corrected Akaike Information Criterion.}
#' \item{nobs}{Number of observations used in fitting (integer).}
#' \item{convergence}{The convergence code returned by the optimizer.}
#' \item{message}{The message returned by the optimizer.}
#' \item{se_available}{Logical indicating if standard errors could be computed.}
#' \item{correlation}{The correlation matrix of coefficients (if \code{correlation = TRUE} and calculable), otherwise \code{NULL}.}
#' \item{fixed}{A named list of parameters that were held fixed during estimation, or \code{NULL}.}
#' \item{fit_method}{The primary fitting method specified ('tmb' or 'nr').}
#' \item{optimizer_method}{The specific optimizer used (e.g., 'nlminb', 'optim', 'Newton-Raphson').}
#'
#' @seealso \code{\link{gkwfit}}, \code{\link{print.summary.gkwfit}}, \code{\link{coef.gkwfit}}, \code{\link{vcov.gkwfit}}, \code{\link{logLik.gkwfit}}, \code{\link{AIC.gkwfit}}
#'
#' @examples
#' \donttest{
#' # Generate data and fit model
#' set.seed(2203)
#' y <- rkw(50, alpha = 2, beta = 3)
#' fit <- gkwfit(data = y, family = "kw", plot = FALSE)
#'
#' # Display detailed summary with parameter estimates and standard errors
#' summary(fit)
#'
#' # Control digits in output
#' summary(fit, digits = 4)
#' }
#'
#' @keywords methods summary
#' @author Lopes, J. E. (with refinements)
#' @export
summary.gkwfit <- function(object, correlation = FALSE, ...) {
  if (!inherits(object, "gkwfit")) {
    stop("Input 'object' must be of class 'gkwfit'")
  }

  # --- Extract components (using accessors if defined, else direct) ---
  # Define helper to safely get value or NULL
  safe_get <- function(obj, name, accessor = NULL) {
    val <- NULL
    if (!is.null(accessor) && exists(accessor, mode = "function")) {
      # Use tryCatch in case accessor fails unexpectedly
      val <- tryCatch(get(accessor)(obj), error = function(e) {
        warning("Accessor function '", accessor, "' failed: ", e$message)
        NULL
      })
    }
    # Fallback or if no accessor specified
    if (is.null(val) && name %in% names(obj)) {
      val <- obj[[name]]
    }
    return(val)
  }

  coefs <- safe_get(object, "coefficients", "coef.gkwfit")
  vcov_matrix <- safe_get(object, "vcov", "vcov.gkwfit")
  logLik_val <- safe_get(object, "loglik", "logLik.gkwfit") # Note output name is loglik
  nobs_val <- safe_get(object, "nobs", "nobs.gkwfit")
  aic_val <- safe_get(object, "AIC", "AIC.gkwfit")
  bic_val <- safe_get(object, "BIC", "BIC.gkwfit")
  aicc_val <- safe_get(object, "AICc") # Assuming no standard AICc accessor
  df_val <- safe_get(object, "df") # Number of estimated parameters

  # Direct access for components less likely to have accessors
  conv_status <- object$convergence
  conv_message <- object$message
  fixed_params <- object$fixed
  family_val <- object$family
  call_val <- object$call
  fit_method_val <- object$method # From TMB fit
  optimizer_method_val <- object$optimizer_method # Specific optimizer

  # --- Prepare Coefficient Table ---
  se_available <- FALSE
  coef_table <- NULL

  # Ensure coefficients exist before proceeding
  if (is.null(coefs) || length(coefs) == 0) {
    warning("No coefficients found in the 'gkwfit' object.")
    # Create an empty matrix with standard columns for consistency downstream
    coef_table <- matrix(NA_real_,
      nrow = 0, ncol = 4,
      dimnames = list(NULL, c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
    )
  } else {
    # Ensure coefficients are named
    if (is.null(names(coefs))) {
      # Try getting names from vcov if possible
      if (!is.null(vcov_matrix) && is.matrix(vcov_matrix) && nrow(vcov_matrix) == length(coefs) && !is.null(rownames(vcov_matrix))) {
        names(coefs) <- rownames(vcov_matrix)
      } else {
        warning("Coefficient names are missing; using generic names.")
        names(coefs) <- paste0("param", seq_along(coefs))
      }
    }

    # se <- sqrt(diag(solve(hskkw(object$coefficients, data = object$data))))

    # std_err <- sqrt(diag(solve(vcov_matrix)))
    std_err <- object$std.errors
    z_vals <- coefs / std_err
    # Handle division by zero or non-finite results safely
    z_vals[!is.finite(z_vals)] <- NA
    p_vals <- 2 * stats::pnorm(abs(z_vals), lower.tail = FALSE)

    coef_table <- cbind(
      Estimate = coefs,
      `Std. Error` = std_err,
      `z value` = z_vals,
      `Pr(>|z|)` = p_vals
    )
    se_available <- TRUE
    rownames(coef_table) <- names(coefs) # Ensure row names are set
    # Row names should be correct due to checks above
  }

  # --- Calculate Correlation Matrix (optional) ---
  cor_matrix <- NULL
  if (correlation && se_available) {
    # Further check for near-zero variances before cov2cor
    if (all(diag(vcov_matrix) > sqrt(.Machine$double.eps))) {
      cor_matrix <- tryCatch(stats::cov2cor(vcov_matrix), error = function(e) {
        warning("Could not compute correlation matrix from 'vcov': ", e$message)
        NULL # Return NULL if cov2cor fails
      })
      # Ensure dimnames match coefficient names, should be correct if vcov check passed
      if (!is.null(cor_matrix)) dimnames(cor_matrix) <- list(names(coefs), names(coefs))
    } else {
      warning("Cannot compute correlations: near-zero or zero variance estimates found.")
    }
  }


  # --- Assemble Summary List ---
  summary_list <- list(
    call = call_val,
    family = family_val,
    coefficients = coef_table,
    loglik = if (is.numeric(logLik_val)) as.numeric(logLik_val) else NA_real_,
    df = if (is.numeric(df_val)) as.integer(df_val) else NA_integer_,
    aic = if (is.numeric(aic_val)) aic_val else NA_real_,
    bic = if (is.numeric(bic_val)) bic_val else NA_real_,
    aicc = if (is.numeric(aicc_val)) aicc_val else NA_real_,
    nobs = if (is.numeric(nobs_val)) as.integer(nobs_val) else NA_integer_,
    convergence = if (is.numeric(conv_status)) conv_status else NA_integer_,
    message = if (is.character(conv_message)) conv_message else NA_character_,
    se_available = se_available,
    correlation = cor_matrix,
    fixed = fixed_params, # Can be NULL if none were fixed
    fit_method = fit_method_val, # Method used in TMB call (nlminb/optim) or null if NR? Need gkwfit logic check. Let's assume it stores 'tmb' or 'nr' maybe? Check object$method vs object$optimizer_method
    optimizer_method = optimizer_method_val # Specific optimizer used
  )

  # Assign class
  class(summary_list) <- "summary.gkwfit"
  return(summary_list)
}


# Ensure stats package is implicitly available or loaded for printCoefmat

#' @title Print Method for summary.gkwfit Objects
#'
#' @description
#' Prints the formatted summary of a \code{gkwfit} model object, generated by
#' \code{summary.gkwfit}. It displays the call, family, coefficient table
#' (with estimates, standard errors, z-values, p-values, and significance stars*),
#' fit statistics (LogLik, AIC, BIC, AICc), number of parameters and observations,
#' fixed parameters (if any), fit method, convergence status including optimizer
#' message, and optionally the correlation matrix of coefficients.
#'
#' * Significance stars are shown next to p-values by default if standard errors
#'   were available.
#'
#' @param x An object of class \code{"summary.gkwfit"}, usually the result of
#'   \code{summary(gkwfit_object)}.
#' @param digits Integer; the minimum number of significant digits to display
#'   for numeric values. Defaults to \code{max(3L, getOption("digits") - 3L)}.
#' @param signif.stars Logical; if \code{TRUE}, p-values are additionally encoded
#'   visually using "significance stars". Defaults to
#'   \code{getOption("show.signif.stars", TRUE)}.
#' @param ... Additional arguments passed to \code{\link[stats]{printCoefmat}}.
#'
#' @return Invisibly returns the original input object \code{x}. Called primarily
#'   for its side effect of printing the summary to the console.
#'
#' @seealso \code{\link{summary.gkwfit}}, \code{\link{gkwfit}}, \code{\link[stats]{printCoefmat}}
#'
#' @keywords methods internal print
#' @author Lopes, J. E. (with refinements)
#' @export
#' @method print summary.gkwfit
print.summary.gkwfit <- function(x, digits = max(3L, getOption("digits") - 3L),
                                 signif.stars = getOption("show.signif.stars", TRUE),
                                 ...) {
  # --- Print Call ---
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  # --- Print Family ---
  cat("Family:", ifelse(is.null(x$family), "Not specified", x$family), "\n")

  # --- Print Fixed Parameters ---
  if (!is.null(x$fixed) && length(x$fixed) > 0) {
    cat("Fixed Parameters:\n")
    fixed_vals <- sapply(x$fixed, function(val) format(val, digits = digits, nsmall = 0))
    fixed_str <- paste(names(x$fixed), "=", fixed_vals, collapse = ", ")
    cat(" ", fixed_str, "\n")
  }

  # --- Print Coefficients ---
  cat("\nCoefficients:\n")
  if (!is.null(x$coefficients) && nrow(x$coefficients) > 0) {
    # Determine if p-values exist (last column usually) and SEs were available
    has_pvalue <- x$se_available && (ncol(x$coefficients) == 4)

    stats::printCoefmat(x$coefficients,
      digits = digits,
      signif.stars = signif.stars, # Ensures stars are used based on option/arg
      na.print = "NA",
      has.Pvalue = has_pvalue, # Correctly indicate if P-values are present and valid
      cs.ind = 1:2, # Column indices for Estimate and Std. Error
      tst.ind = 3, # Column index for z value (test statistic)
      # zap.ind = integer(), # Optional: prevent zapping small numbers, default is usually fine
      ...
    )
    if (!x$se_available) {
      cat("\n(Standard Errors & Tests unavailable; requires valid Hessian/vcov)\n")
    }
  } else {
    cat(" (No coefficients estimated or available to display)\n")
  }


  # --- Print Fit Statistics ---
  cat("\n--- Fit Statistics ---\n")
  cat("Log-likelihood:", formatC(x$loglik, format = "f", digits = digits))
  cat("  Parameters (est.):", ifelse(is.na(x$df), "NA", x$df), "\n") # Added df
  cat(
    "AIC:", formatC(x$aic, format = "f", digits = digits),
    "  BIC:", formatC(x$bic, format = "f", digits = digits),
    "  AICc:", formatC(x$aicc, format = "f", digits = digits), "\n"
  ) # Added AICc
  cat("Number of observations:", ifelse(is.na(x$nobs), "NA", x$nobs), "\n")


  # --- Print Fit Method and Convergence ---
  cat("Fit Method:", ifelse(is.null(x$fit_method), "Unknown", x$fit_method))
  if (!is.null(x$optimizer_method) && !is.null(x$fit_method) &&
    x$optimizer_method != x$fit_method) {
    cat(" (Optimizer: ", x$optimizer_method, ")", sep = "")
  }
  cat("\n") # Newline before convergence

  # Convergence Status Text
  conv_code <- x$convergence
  if (is.null(conv_code) || is.na(conv_code)) {
    conv_status_text <- "Unknown"
  } else if (conv_code == 0) {
    conv_status_text <- "Successful convergence"
  } else {
    conv_status_text <- paste0("Potential issues (code ", conv_code, ")")
  }
  # cat("Convergence:", conv_status_text)

  # Optimizer Message
  conv_msg <- if (!is.null(x$message) && nzchar(trimws(x$message))) trimws(x$message) else "None reported"
  # Print message only if it's not trivial (like the standard NLMINB message for success) or if convergence failed
  # Basic heuristic: print if code != 0 or if message is not standard success message
  print_msg <- (!is.na(conv_code) && conv_code != 0) ||
    (conv_msg != "None reported" && !grepl("relative convergence|objective function values|both X and GR tolerances met", conv_msg, ignore.case = TRUE))

  if (print_msg) {
    cat("\nOptimizer Message:", conv_msg, "\n")
  } else {
    cat("\n") # Ensure newline even if message isn't printed
  }


  # --- Print Correlation Matrix (Optional) ---
  if (!is.null(x$correlation)) {
    cat("\nCorrelation of Coefficients:\n")
    cor_mat <- x$correlation
    # Use stats::print.default or similar; printCoefmat is not for cor matrices
    # Ensure symmetric is FALSE if you only want lower/upper triangle
    stats::printCoefmat(cor_mat,
      digits = digits, signif.stars = FALSE, # No stars for correlations
      na.print = "NA", P.values = FALSE, has.Pvalue = FALSE, ...
    )
  }

  cat("\n") # Final newline for clean separation
  invisible(x) # Return the summary object invisibly
}

#' @title Plot Diagnostics for a gkwfit Object
#'
#' @description
#' Creates a panel of diagnostic plots for assessing the fit of a model estimated
#' by \code{\link{gkwfit}}. It displays a histogram of the data overlaid with the
#' fitted density, a Probability-Probability (P-P) plot, a Quantile-Quantile (Q-Q)
#' plot, and profile likelihood plots for each parameter if they were computed
#' during the fit (i.e., if \code{profile = TRUE} was used in \code{\link{gkwfit}}).
#'
#' @details
#' This function utilizes \code{ggplot2} for creating the plots and \code{patchwork}
#' for arranging them into a single figure. All plots use \code{ggplot2::theme_minimal()}.
#'
#' If the plots were already generated during the original \code{\link{gkwfit}} call
#' (because \code{plot = TRUE}), they are retrieved from the fitted object.
#' Otherwise, this function will attempt to generate the plots on the fly,
#' which requires the \code{ggplot2} package and the necessary distribution
#' functions (like \code{dgkw}, \code{pgkw}, \code{qgkw}, etc.) for the specific
#' \code{family} to be available.
#'
#' The arrangement of plots is handled automatically by \code{patchwork::wrap_plots}.
#' No user interaction (like menu selection) is required.
#'
#' @param x An object of class \code{"gkwfit"}, typically the result of a call to \code{\link{gkwfit}}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the original input object \code{x}. This function is called for its side effect of producing a plot.
#'
#' @seealso \code{\link{gkwfit}}, \code{\link{summary.gkwfit}}
#'
#' @examples
#' \donttest{
#' # Load required package
#' library(ggplot2)
#'
#' # Generate data and fit model
#' set.seed(2203)
#' y <- rbeta_(50, gamma = 2, delta = 3)
#' fit <- gkwfit(data = y, family = "beta", plot = FALSE)
#'
#' # Generate standard diagnostic plots
#' plot(fit)
#'
#' # Generate data and fit model with profile = TRUE
#' fit <- gkwfit(data = y, family = "gkw", profile = TRUE, npoints = 15)
#'
#' # Standard diagnostic plots
#' plot(fit)
#' }
#'
#' @keywords hplot methods
#' @author Lopes, J. E.
#' @export
plot.gkwfit <- function(x, ...) {
  if (!inherits(x, "gkwfit")) {
    stop("Input 'x' must be of class 'gkwfit'")
  }

  plots_list <- gkwgof(x, simulate_p_values = FALSE, plot = TRUE, print_summary = FALSE, ...)

  invisible(plots_list)
}


#' @title Extract Model Coefficients from a gkwfit Object
#'
#' @description
#' Extracts the estimated coefficients for the parameters of a model fitted by
#' \code{\link{gkwfit}}. This is an S3 method for the generic \code{\link[stats]{coef}} function.
#'
#' @param object An object of class \code{"gkwfit"}, typically the result of a call to \code{\link{gkwfit}}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A named numeric vector containing the estimated coefficients for the
#'   parameters of the specified GKw family distribution. The names correspond
#'   to the parameter names (e.g., \code{"alpha"}, \code{"beta"}, etc.).
#'
#' @seealso \code{\link{gkwfit}}, \code{\link[stats]{coef}}, \code{\link{vcov.gkwfit}}, \code{\link{logLik.gkwfit}}
#'
#' @examples
#' \donttest{
#' # Generate data and fit model
#' set.seed(2203)
#' y <- rgkw(50, alpha = 1.5, beta = 2.5, gamma = 1.2, delta = 0.3, lambda = 1.1)
#' fit <- gkwfit(data = y, family = "gkw", plot = FALSE)
#'
#' # Extract all coefficients
#' params <- coef(fit)
#' print(params)
#'
#' # Access specific parameters
#' alpha_est <- coef(fit)["alpha"]
#' lambda_est <- coef(fit)["lambda"]
#' cat("Estimated alpha:", alpha_est, "\n")
#' cat("Estimated lambda:", lambda_est, "\n")
#' }
#'
#' @keywords methods models
#' @author Lopes, J. E.
#' @export
coef.gkwfit <- function(object, ...) {
  # Basic check for existence
  if (is.null(object$coefficients)) {
    warning("Component 'coefficients' not found in the 'gkwfit' object.")
    return(NULL)
  }
  object$coefficients
}


#' @title Extract Variance-Covariance Matrix from a gkwfit Object
#'
#' @description
#' Extracts the variance-covariance matrix of the estimated parameters from a model
#' fitted by \code{\link{gkwfit}}. This matrix is typically derived from the
#' inverse of the observed Hessian matrix calculated during fitting (requires
#' \code{hessian = TRUE} in the \code{\link{gkwfit}} call). This is an S3 method
#' for the generic \code{\link[stats]{vcov}} function.
#'
#' @param object An object of class \code{"gkwfit"}, typically the result of a call to \code{\link{gkwfit}}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A numeric matrix representing the variance-covariance matrix of the
#'   estimated model parameters. Row and column names correspond to the parameter
#'   names (e.g., \code{"alpha"}, \code{"beta"}, etc.). Returns \code{NULL} or
#'   raises a warning/error if the matrix is not available (e.g., if \code{hessian=FALSE}
#'   was used or if the Hessian computation failed).
#'
#' @seealso \code{\link{gkwfit}}, \code{\link[stats]{vcov}}, \code{\link{coef.gkwfit}}, \code{\link{logLik.gkwfit}}
#'
#' @examples
#' \donttest{
#' # Generate data and fit model (ensure hessian = TRUE for vcov)
#' set.seed(2203)
#' y <- rbkw(50, alpha = 2, beta = 3, gamma = 1.5, delta = 0.5)
#' fit <- gkwfit(data = y, family = "bkw", plot = FALSE, hessian = TRUE)
#'
#' # Extract variance-covariance matrix
#' vcov_matrix <- vcov(fit)
#' print(vcov_matrix)
#'
#' # Extract standard errors from the diagonal
#' std_errors <- sqrt(diag(vcov_matrix))
#' print(std_errors)
#'
#' # Compare with standard errors from summary
#' summary_se <- summary(fit)$coefficients[, "Std. Error"]
#' all.equal(std_errors, summary_se)
#' }
#'
#' @keywords methods models
#' @author Lopes, J. E.
#' @export
vcov.gkwfit <- function(object, ...) {
  # Basic check for existence
  vcov_mat <- object$vcov
  if (is.null(vcov_mat)) {
    warning(
      "Component 'vcov' (variance-covariance matrix) not found in the 'gkwfit' object.",
      "\nWas the model fitted with hessian=TRUE?"
    )
    return(NULL)
  }
  # Optional: Further checks if vcov_mat is a valid matrix?
  if (!is.matrix(vcov_mat) || !is.numeric(vcov_mat)) {
    warning("'vcov' component is not a numeric matrix.")
    return(NULL)
  }
  # Ensure dimnames match coefficients if possible
  if (!is.null(object$coefficients)) {
    coef_names <- names(object$coefficients)
    if (length(coef_names) == nrow(vcov_mat) && length(coef_names) == ncol(vcov_mat)) {
      dimnames(vcov_mat) <- list(coef_names, coef_names)
    }
  }
  vcov_mat
}


#' @title Compute Confidence Intervals for gkwfit Parameters
#'
#' @description
#' Computes confidence intervals for one or more parameters in a model fitted by
#' \code{\link{gkwfit}}. It uses the Wald method based on the estimated coefficients
#' and their standard errors derived from the variance-covariance matrix.
#'
#' @param object An object of class \code{"gkwfit"}, typically the result of a call to \code{\link{gkwfit}}.
#'   The object must contain valid coefficient estimates and a corresponding
#'   variance-covariance matrix (usually requires fitting with \code{hessian = TRUE}).
#' @param parm A specification of which parameters are to be given confidence intervals,
#'   either a vector of numbers (indices) or a vector of names. If missing,
#'   confidence intervals are computed for all parameters that have a valid
#'   standard error available. Parameter indices refer to the order of parameters
#'   for which standard errors could be calculated.
#' @param level The confidence level required (default: 0.95).
#' @param ... Additional arguments (currently ignored).
#'
#' @details
#' This function calculates confidence intervals using the Wald method:
#' \eqn{Estimate \pm z \times SE}, where \eqn{z} is the appropriate quantile
#' from the standard normal distribution for the given confidence `level`.
#'
#' It relies on the results from \code{\link{coef.gkwfit}} and \code{\link{vcov.gkwfit}}
#' (or directly accesses \code{object$coefficients} and \code{object$vcov} if those
#' methods aren't defined). It checks for the validity of the variance-covariance
#' matrix before proceeding.
#'
#' Since all parameters of the GKw family distributions are constrained to be positive,
#' the lower bound of the confidence interval is truncated at a small positive value
#' (\code{.Machine$double.eps^0.5}) if the calculated lower bound is non-positive.
#'
#' If `parm` is specified, it selects the parameters for which to compute intervals.
#' Numeric indices in `parm` refer to the parameters that have calculable standard
#' errors, not necessarily all parameters in the model (if some were fixed or had
#' estimation issues).
#'
#' @return A matrix with columns giving lower and upper confidence limits for each
#'   parameter specified in `parm`. The columns are labeled with quantile percentages
#'   (e.g., \code{"2.5 %"} and \code{"97.5 %"} for \code{level = 0.95}). Row names
#'   are taken from the parameter names. Returns \code{NULL} or stops with an error
#'   if coefficients or a valid variance-covariance matrix cannot be extracted.
#'
#' @seealso \code{\link{gkwfit}}, \code{\link[stats]{confint}}, \code{\link{coef.gkwfit}}, \code{\link{vcov.gkwfit}}
#'
#' @examples
#' \donttest{
#' # Generate data and fit model
#' set.seed(2203)
#' y <- rkw(50, alpha = 2, beta = 3)
#' fit <- gkwfit(data = y, family = "kw", plot = FALSE, hessian = TRUE)
#'
#' # Calculate confidence intervals for all parameters
#' ci <- confint(fit)
#' print(ci)
#'
#' # 90% confidence interval
#' ci_90 <- confint(fit, level = 0.90)
#' print(ci_90)
#'
#' # Confidence interval for specific parameter
#' ci_alpha <- confint(fit, parm = "alpha")
#' print(ci_alpha)
#' }
#'
#' @keywords methods models
#' @author Lopes, J. E.
#' @export
confint.gkwfit <- function(object, parm, level = 0.95, ...) {
  if (!inherits(object, "gkwfit")) {
    stop("Input 'object' must be of class 'gkwfit'.")
  }
  if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
    stop("'level' must be a single numeric value between 0 and 1.")
  }

  # Prefer methods if they exist and work
  cf <- tryCatch(stats::coef(object), error = function(e) object$coefficients)
  vc <- tryCatch(stats::vcov(object), error = function(e) object$vcov)

  if (is.null(cf)) {
    stop("Could not extract coefficients ('coefficients') from the object.")
  }
  if (is.null(vc)) {
    stop(
      "Could not extract variance-covariance matrix ('vcov') from the object.",
      "\nWas the model fitted with hessian=TRUE?"
    )
  }

  # --- Validate VCOV and Calculate SE ---
  coef_names <- names(cf)
  if (is.null(coef_names)) {
    stop("Coefficients must be named.")
  }
  if (!is.matrix(vc) || nrow(vc) != ncol(vc) || nrow(vc) != length(cf) ||
    is.null(rownames(vc)) || is.null(colnames(vc)) || # Check names exist
    !all(rownames(vc) == coef_names) || !all(colnames(vc) == coef_names)) {
    stop(
      "Variance-covariance matrix ('vcov') has unexpected dimensions or names",
      " relative to coefficients."
    )
  }
  variances <- diag(vc)
  names(variances) <- coef_names # Ensure names consistency
  if (anyNA(variances)) {
    warning(
      "NA values found in the diagonal of the vcov matrix.",
      " Parameters with NA variance will have NA intervals."
    )
  }
  if (any(variances[!is.na(variances)] < 0)) {
    warning(
      "Negative variances found in vcov matrix.",
      " Affected parameters will have NA intervals."
    )
    variances[variances < 0] <- NA # Set negative variances to NA
  }
  ses <- sqrt(variances) # Will produce NAs where variances were NA or negative

  # --- Determine Parameters for Intervals ---
  # Identify parameters with valid (non-NA) standard errors
  param_names_with_se <- coef_names[!is.na(ses)]

  if (length(param_names_with_se) == 0) {
    warning(
      "No parameters with valid standard errors available ",
      "for confidence interval calculation."
    )
    # Return an empty matrix with correct column names
    a <- (1 - level) / 2
    a <- c(a, 1 - a)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    return(matrix(NA_real_, nrow = 0, ncol = 2, dimnames = list(NULL, pct)))
  }

  # Select parameters based on 'parm' argument
  if (missing(parm)) {
    params_to_compute <- param_names_with_se # Default: all with valid SE
  } else {
    if (is.numeric(parm)) {
      # Check numeric indices against the list of parameters *with SEs*
      if (any(parm <= 0) || any(parm > length(param_names_with_se))) {
        stop(
          "Numeric 'parm' indices are out of bounds (1 to ",
          length(param_names_with_se), ") for parameters with available SEs."
        )
      }
      params_to_compute <- param_names_with_se[parm]
    } else if (is.character(parm)) {
      unknown_parm <- setdiff(parm, coef_names) # Check against all coef names
      if (length(unknown_parm) > 0) {
        stop(
          "Unknown parameter(s) requested in 'parm': ",
          paste(dQuote(unknown_parm), collapse = ", ")
        )
      }
      # Filter requested parameters to only those with valid SEs
      params_to_compute <- intersect(parm, param_names_with_se)
      if (length(params_to_compute) == 0) {
        # Check if the requested parameters existed but just lacked SEs
        requested_but_no_se <- intersect(parm, setdiff(coef_names, param_names_with_se))
        if (length(requested_but_no_se) > 0) {
          stop(
            "None of the requested parameters have available standard errors. ",
            "Problematic parameters might include: ",
            paste(dQuote(requested_but_no_se), collapse = ", ")
          )
        } else {
          # Should not happen given the unknown_parm check, but as fallback
          stop("No valid parameters selected for confidence intervals.")
        }
      }
    } else {
      stop("'parm' must be missing, a numeric vector, or a character vector.")
    }
  }

  # --- Calculate Confidence Intervals ---
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%") # Format column names

  # Z-critical value for normal distribution
  z_crit <- stats::qnorm(a[2])

  # Subset coefficients and SEs for the selected parameters
  cf_sub <- cf[params_to_compute]
  ses_sub <- ses[params_to_compute] # NAs handled by calculation below

  # Calculate intervals matrix: Estimate +/- z * SE
  ci <- cf_sub + ses_sub %o% c(-z_crit, z_crit)

  # Ensure parameters are positive (all GKw parameters should be)
  # Using small positive value instead of exact zero
  # Apply only where the original estimate was positive, otherwise result is NA anyway
  ci <- ifelse(!is.na(ci) & ci < .Machine$double.eps^0.5, .Machine$double.eps^0.5, ci)

  # --- Format Output Matrix ---
  dimnames(ci) <- list(params_to_compute, pct) # Assign row and column names

  return(ci)
}


#' @title Extract Log-Likelihood from a gkwfit Object
#'
#' @description
#' Extracts the maximized log-likelihood value from a model fitted by \code{\link{gkwfit}}.
#' It returns an object of class \code{"logLik"}, which includes attributes for the
#' degrees of freedom (\code{"df"}) and the number of observations (\code{"nobs"}) used in the fit.
#'
#' @param object An object of class \code{"gkwfit"}, typically the result of a call to \code{\link{gkwfit}}.
#' @param ... Additional arguments (currently ignored).
#'
#' @details
#' This method provides compatibility with standard R functions that operate on
#' log-likelihood values, such as \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}},
#' and likelihood ratio tests. It retrieves the log-likelihood stored during the
#' model fitting process (in \code{object$loglik}) and attaches the required
#' attributes (\code{object$df} for the number of estimated parameters and
#' \code{object$nobs} for the number of observations).
#'
#' @return An object of class \code{"logLik"}. This is the numeric log-likelihood value
#'   with the following attributes:
#'   \item{df}{The number of estimated parameters in the model (integer).}
#'   \item{nobs}{The number of observations used for fitting the model (integer).}
#'
#' @seealso \code{\link{gkwfit}}, \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}, \code{\link[stats]{logLik}}
#'
#' @examples
#' \donttest{
#' # Generate data and fit two models
#' set.seed(2203)
#' y <- rgkw(50, alpha = 2, beta = 3, gamma = 1, delta = 0, lambda = 1.5)
#'
#' fit1 <- gkwfit(data = y, family = "kkw", plot = FALSE) # KKw model
#' fit2 <- gkwfit(data = y, family = "ekw", plot = FALSE) # EKw model
#'
#' # Extract log-likelihood values
#' ll1 <- logLik(fit1)
#' ll2 <- logLik(fit2)
#'
#' print(ll1)
#' print(ll2)
#'
#' # Use for likelihood ratio test
#' lr_stat <- -2 * (as.numeric(ll1) - as.numeric(ll2))
#' df_diff <- attr(ll1, "df") - attr(ll2, "df")
#' p_value <- pchisq(lr_stat, df = abs(df_diff), lower.tail = FALSE)
#'
#' cat("LR statistic:", lr_stat, "\n")
#' cat("df:", df_diff, "\n")
#' cat("p-value:", p_value, "\n")
#' }
#'
#' @keywords methods models utility
#' @author Lopes, J. E.
#' @importFrom stats logLik
#' @method logLik gkwfit
#' @export
logLik.gkwfit <- function(object, ...) {
  # Ensure the input object is of the correct class
  if (!inherits(object, "gkwfit")) {
    stop("Input 'object' must be of class 'gkwfit'.")
  }

  # Check for the existence of required components
  required_comps <- c("loglik", "df", "nobs")
  missing_comps <- setdiff(required_comps, names(object))
  if (length(missing_comps) > 0) {
    stop(
      "The 'gkwfit' object provided to logLik.gkwfit is missing required component(s): ",
      paste(missing_comps, collapse = ", "), "."
    )
  }

  val <- object$loglik
  df_val <- object$df
  nobs_val <- object$nobs

  # Validate the types and values of the components
  if (!is.numeric(val) || length(val) != 1 || !is.finite(val)) {
    stop("Component 'loglik' must be a single finite numeric value in logLik.gkwfit.")
  }
  # df should be a non-negative integer count of estimated parameters
  if (!is.numeric(df_val) || length(df_val) != 1 || !is.finite(df_val) || df_val < 0 || (df_val %% 1 != 0)) {
    # Allow tolerance for floating point comparison, although df should be integer
    if (!isTRUE(all.equal(df_val, round(df_val))) || df_val < 0) {
      stop("Component 'df' (number of estimated parameters) must be a single non-negative integer in logLik.gkwfit.")
    }
    # Coerce potentially float-like integer (e.g. 3.0) to integer
    df_val <- as.integer(round(df_val))
  } else {
    df_val <- as.integer(df_val) # Ensure integer type
  }

  # nobs should be a positive integer count of observations
  if (!is.numeric(nobs_val) || length(nobs_val) != 1 || !is.finite(nobs_val) || nobs_val <= 0 || (nobs_val %% 1 != 0)) {
    if (!isTRUE(all.equal(nobs_val, round(nobs_val))) || nobs_val <= 0) {
      stop("Component 'nobs' (number of observations) must be a single positive integer in logLik.gkwfit.")
    }
    nobs_val <- as.integer(round(nobs_val))
  } else {
    nobs_val <- as.integer(nobs_val) # Ensure integer type
  }

  # Assign attributes and class
  attr(val, "df") <- df_val
  attr(val, "nobs") <- nobs_val
  class(val) <- "logLik"

  return(val)
}


#' @title Calculate AIC or BIC for gkwfit Objects
#'
#' @description
#' Computes the Akaike Information Criterion (AIC) or variants like the Bayesian
#' Information Criterion (BIC) for one or more fitted model objects of class \code{"gkwfit"}.
#'
#' @param object An object of class \code{"gkwfit"}, typically the result of a call to \code{\link{gkwfit}}.
#' @param ... Optionally, more fitted model objects of class \code{"gkwfit"}.
#' @param k Numeric scalar specifying the penalty per parameter. The default \code{k = 2}
#'   corresponds to the traditional AIC. Use \code{k = log(n)} (where n is the number
#'   of observations) for the BIC (Bayesian Information Criterion).
#'
#' @details
#' This function calculates an information criterion based on the formula
#' \eqn{-2 \times \log Likelihood + k \times df}, where \eqn{df} represents the
#' number of estimated parameters in the model (degrees of freedom).
#'
#' It relies on the \code{\link{logLik.gkwfit}} method to extract the log-likelihood
#' and the degrees of freedom for each model.
#'
#' When comparing multiple models fitted to the **same data**, the model with the
#' lower AIC or BIC value is generally preferred. The function returns a sorted
#' data frame to facilitate this comparison when multiple objects are provided.
#'
#' @return
#' \itemize{
#'   \item If only one \code{object} is provided: A single numeric value representing the calculated criterion (AIC or BIC).
#'   \item If multiple objects are provided: A \code{data.frame} with rows corresponding
#'     to the models and columns for the degrees of freedom (\code{df}) and the
#'     calculated criterion value (named \code{AIC}, regardless of the value of \code{k}).
#'     The data frame is sorted in ascending order based on the criterion values.
#'     Row names are derived from the deparsed calls of the fitted models.
#' }
#'
#' @seealso \code{\link{gkwfit}}, \code{\link[stats]{AIC}}, \code{\link{logLik.gkwfit}}, \code{\link{BIC.gkwfit}}
#'
#' @examples
#' \donttest{
#' set.seed(2203)
#' y <- rkw(1000, alpha = 2.5, beta = 1.5)
#'
#' # Fit different models to the same data
#' fit1_kw <- gkwfit(y, family = "kw", silent = TRUE)
#' fit2_bkw <- gkwfit(y, family = "bkw", silent = TRUE)
#' fit3_gkw <- gkwfit(y, family = "gkw", silent = TRUE)
#'
#' # Calculate AIC for a single model
#' aic1 <- AIC(fit1_kw)
#' print(aic1)
#'
#' # Compare AIC values for multiple models
#' aic_comparison <- c(AIC(fit1_kw), AIC(fit2_bkw), AIC(fit3_gkw))
#' print(aic_comparison)
#' }
#'
#' @keywords models methods
#' @author Lopes, J. E.
#' @importFrom stats AIC
#' @method AIC gkwfit
#' @export
AIC.gkwfit <- function(object, ..., k = 2) {
  # --- Input Validation ---
  if (!inherits(object, "gkwfit")) {
    stop("Input 'object' must be of class 'gkwfit'.")
  }
  objects <- list(object, ...)
  obj_classes <- sapply(objects, class)
  if (any(obj_classes != "gkwfit")) {
    warning("All objects passed to AIC should ideally be of class 'gkwfit'.")
  }
  if (!is.numeric(k) || length(k) != 1 || k < 0) {
    stop("'k' must be a single non-negative numeric value.")
  }


  # --- Use logLik method to get value and df attribute consistently ---
  lls <- lapply(objects, function(obj) {
    ll <- tryCatch(logLik(obj), error = function(e) NULL)
    if (is.null(ll) || !inherits(ll, "logLik")) {
      stop("Could not extract valid 'logLik' object for one of the models.")
    }
    if (is.null(attr(ll, "df")) || is.null(attr(ll, "nobs"))) {
      stop("The 'logLik' object is missing 'df' or 'nobs' attribute.")
    }
    ll
  })

  # --- Check if nobs are consistent for model comparison ---
  if (length(lls) > 1) {
    nobs_vals <- sapply(lls, attr, "nobs")
    if (length(unique(nobs_vals)) > 1) {
      warning(
        "Models were not all fitted to the same number of observations.",
        "\nAIC/BIC comparison might be problematic."
      )
    }
  }

  # --- Calculate Criterion ---
  vals <- sapply(lls, function(ll) -2 * as.numeric(ll) + k * attr(ll, "df"))
  dfs <- sapply(lls, function(ll) attr(ll, "df"))

  # --- Format Output ---
  if (length(objects) == 1) {
    return(vals)
  } else {
    # Try to get model names from call
    calls <- lapply(objects, function(obj) {
      # Attempt to retrieve the call safely
      obj_call <- tryCatch(obj$call, error = function(e) NULL)
      if (!is.call(obj_call)) obj_call <- NULL # Ensure it's a call or NULL
      obj_call
    })
    # Use deparse1 for concise name, provide default if call missing
    mnames <- sapply(calls, function(cc) {
      if (!is.null(cc)) deparse1(cc, collapse = " ") else paste0("Model", seq_along(calls))
    })
    # Handle potential duplicate names
    if (anyDuplicated(mnames)) mnames <- make.unique(mnames, sep = ".")

    result <- data.frame(
      df = dfs,
      AIC = vals # Column name remains "AIC" regardless of k, per stats::AIC convention
    )
    rownames(result) <- mnames

    # Sort by the criterion value
    result <- result[order(result$AIC), ]
    return(result)
  }
}


#' @title Calculate Bayesian Information Criterion (BIC) for gkwfit Objects
#'
#' @description
#' Computes the Bayesian Information Criterion (BIC), sometimes called the
#' Schwarz criterion (SIC), for one or more fitted model objects of class \code{"gkwfit"}.
#'
#' @param object An object of class \code{"gkwfit"}, typically the result of a call
#'   to \code{\link{gkwfit}}.
#' @param ... Optionally, more fitted model objects of class \code{"gkwfit"}.
#'
#' @details
#' This function calculates the BIC based on the formula
#' \eqn{-2 \times \log Likelihood + \log(n) \times df}, where \eqn{n} is the number
#' of observations and \eqn{df} represents the number of estimated parameters in the
#' model (degrees of freedom).
#'
#' It relies on the \code{\link{logLik.gkwfit}} method to extract the log-likelihood,
#' the degrees of freedom (\code{df}), and the number of observations (\code{nobs})
#' for each model. Ensure that \code{logLik.gkwfit} is defined and returns a valid
#' \code{"logLik"} object with appropriate attributes.
#'
#' When comparing multiple models fitted to the **same data**, the model with the
#' lower BIC value is generally preferred, as BIC tends to penalize model complexity
#' more heavily than AIC for larger sample sizes. The function returns a sorted
#' data frame to facilitate this comparison when multiple objects are provided. A
#' warning is issued if models were fitted to different numbers of observations.
#'
#' @return
#' \itemize{
#'   \item If only one \code{object} is provided: A single numeric value, the calculated BIC.
#'   \item If multiple objects are provided: A \code{data.frame} with rows corresponding
#'     to the models and columns for the degrees of freedom (\code{df}) and the
#'     calculated BIC value (named \code{BIC}). The data frame is sorted in
#'     ascending order based on the BIC values. Row names are generated from the
#'     deparsed calls or the names of the arguments passed to BIC.
#' }
#'
#' @seealso \code{\link{gkwfit}}, \code{\link[stats]{BIC}}, \code{\link{logLik.gkwfit}}, \code{\link{AIC.gkwfit}}
#'
#' @examples
#' \donttest{
#'
#' set.seed(2203)
#' y <- rkw(1000, alpha = 2.5, beta = 1.5)
#'
#' # Fit different models to the same data
#' fit1_kw <- gkwfit(y, family = "kw", silent = TRUE)
#' fit2_bkw <- gkwfit(y, family = "bkw", silent = TRUE)
#' fit3_gkw <- gkwfit(y, family = "gkw", silent = TRUE)
#'
#' # Calculate BIC for a single model
#' bic1 <- BIC(fit1_kw)
#' print(bic1)
#'
#' # Compare BIC values for multiple models
#' bic_comparison <- c(BIC(fit1_kw), BIC(fit2_bkw), BIC(fit3_gkw))
#' print(bic_comparison)
#' }
#'
#' @keywords models methods stats
#' @author Lopes, J. E. (with refinements)
#' @importFrom stats BIC logLik nobs
#' @method BIC gkwfit
#' @export
BIC.gkwfit <- function(object, ...) {
  # Combine the first object and any others passed via ...
  objects <- list(object, ...)
  # Store original argument names/expressions for potential use in rownames
  arg_names <- as.character(match.call(expand.dots = TRUE)[-1L])

  # --- Basic Input Validation ---
  obj_classes <- sapply(objects, function(o) class(o)[1]) # Get primary class
  if (!inherits(object, "gkwfit")) {
    stop("The first argument 'object' must be of class 'gkwfit'.")
  }
  if (any(!sapply(objects, inherits, "gkwfit"))) {
    # Find which ones don't inherit
    bad_obj_indices <- which(!sapply(objects, inherits, "gkwfit"))
    warning(
      "Argument(s) at position(s) ", paste(bad_obj_indices, collapse = ", "),
      " do not inherit from 'gkwfit'. Ensure all models are comparable."
    )
  }

  # --- Use logLik method to get value, df, and nobs attribute consistently ---
  lls <- vector("list", length(objects))
  for (i in seq_along(objects)) {
    current_obj <- objects[[i]]
    ll <- tryCatch(stats::logLik(current_obj), error = function(e) {
      # Provide context if logLik fails
      list(error = TRUE, message = paste("Failed to extract logLik for model #", i, ": ", e$message))
    })

    # Check for failure during logLik extraction
    if (is.list(ll) && isTRUE(ll$error)) {
      stop(ll$message)
    }

    # Validate the returned logLik object
    if (is.null(ll) || !inherits(ll, "logLik")) {
      stop(
        "Could not extract a valid 'logLik' object for model #", i,
        " (argument '", arg_names[i], "')."
      )
    }
    req_attrs <- c("df", "nobs")
    if (!all(req_attrs %in% names(attributes(ll)))) {
      stop(
        "The 'logLik' object is missing 'df' or 'nobs' attribute for model #", i,
        " (argument '", arg_names[i], "')."
      )
    }
    n_obs_val <- attr(ll, "nobs")
    if (is.null(n_obs_val) || !is.numeric(n_obs_val) || length(n_obs_val) != 1 || n_obs_val <= 0 || !is.finite(n_obs_val)) {
      stop(
        "Number of observations ('nobs' attribute of logLik) must be a single positive finite value ",
        "for BIC calculation for model #", i, " (argument '", arg_names[i], "')."
      )
    }
    # Store the validated logLik object
    lls[[i]] <- ll
  } # End loop through objects

  # --- Check if nobs are consistent for model comparison ---
  nobs_vals <- sapply(lls, attr, "nobs")
  if (length(unique(nobs_vals)) > 1) {
    warning(
      "Models were not all fitted to the same number of observations.",
      "\nBIC comparison across models with different 'nobs' may be problematic."
    )
  }

  # --- Calculate BIC ---
  # BIC = -2*logLik + log(nobs) * df
  vals <- sapply(lls, function(ll) -2 * as.numeric(ll) + log(attr(ll, "nobs")) * attr(ll, "df"))
  dfs <- sapply(lls, function(ll) attr(ll, "df"))

  # --- Format Output ---
  if (length(objects) == 1) {
    # Return single numeric value for single object
    return(vals)
  } else {
    # Create data frame for multiple objects
    # Try to get model names from call stored in the object, fall back to arg names
    mnames <- character(length(objects))
    for (i in seq_along(objects)) {
      obj_call <- tryCatch(objects[[i]]$call, error = function(e) NULL)
      if (!is.null(obj_call) && is.call(obj_call)) {
        # Use deparse1 for concise name from the object's call
        mnames[i] <- deparse1(obj_call, collapse = " ")
      } else {
        # Fallback to the name/expression used in the BIC() call
        mnames[i] <- arg_names[i]
      }
    }

    # Ensure unique names if needed (e.g., if default calls were used)
    if (anyDuplicated(mnames)) mnames <- make.unique(mnames, sep = ".")

    result <- data.frame(
      df = dfs,
      BIC = vals # Column name is BIC (standard)
    )
    rownames(result) <- mnames

    # Sort by the criterion value (ascending)
    result <- result[order(result$BIC), ]
    return(result)
  }
}


#' @title Compare Fitted gkwfit Models using Likelihood Ratio Tests
#'
#' @description
#' Computes Likelihood Ratio Tests (LRT) to compare two or more nested models
#' fitted using \code{\link{gkwfit}}. It produces a table summarizing the models
#' and the test statistics.
#'
#' @param object An object of class \code{"gkwfit"}, representing the first fitted model.
#' @param ... One or more additional objects of class \code{"gkwfit"}, representing
#'   subsequent fitted models, assumed to be nested within each other or the first model.
#'
#' @details
#' This function performs pairwise likelihood ratio tests between consecutively ordered
#' models (ordered by their degrees of freedom). It assumes the models are nested
#' and are fitted to the same dataset. A warning is issued if the number of
#' observations differs between models.
#'
#' The Likelihood Ratio statistic is calculated as \eqn{LR = 2 \times (\log L_{complex} - \log L_{simple})}.
#' This statistic is compared to a Chi-squared distribution with degrees of freedom
#' equal to the difference in the number of parameters between the two models
#' (\eqn{\Delta df = df_{complex} - df_{simple}}).
#'
#' The output table includes the number of parameters (`N.Par`), AIC, BIC, log-likelihood (`LogLik`),
#' the test description (`Test`), the LR statistic (`LR stat.`), and the p-value (`Pr(>Chi)`).
#' Models are ordered by increasing complexity (number of parameters).
#'
#' Warnings are issued if models do not appear correctly nested based on degrees of
#' freedom or if the log-likelihood decreases for a more complex model, as the LRT
#' results may not be meaningful in such cases.
#'
#' The function relies on a working \code{\link{logLik.gkwfit}} method to extract
#' necessary information (log-likelihood, df, nobs).
#'
#' @return An object of class \code{c("anova.gkwfit", "anova", "data.frame")}.
#'   This data frame contains rows for each model and columns summarizing the fit
#'   and the pairwise likelihood ratio tests. It includes:
#'   \item{N.Par}{Number of estimated parameters (degrees of freedom).}
#'   \item{AIC}{Akaike Information Criterion.}
#'   \item{BIC}{Bayesian Information Criterion.}
#'   \item{LogLik}{Log-likelihood value.}
#'   \item{Test}{Description of the pairwise comparison (e.g., "1 vs 2").}
#'   \item{LR stat.}{Likelihood Ratio test statistic.}
#'   \item{Pr(>Chi)}{P-value from the Chi-squared test.}
#'   The table is printed using a method that mimics \code{print.anova}.
#'
#' @seealso \code{\link{gkwfit}}, \code{\link{logLik.gkwfit}}, \code{\link{AIC.gkwfit}}, \code{\link{BIC.gkwfit}}, \code{\link[stats]{anova}}
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(ggplot2)
#' library(patchwork)
#' library(betareg)
#' # Generate data from GKw distribution
#' set.seed(2203)
#' n <- 1000
#' y <- rgkw(n, alpha = 2, beta = 3, gamma = 1.5, delta = 0.2, lambda = 1.2)
#'
#' # Fit models from GKw family respecting their parameter structures
#' # Full GKw model: 5 parameters (alpha, beta, gamma, delta, lambda)
#' fit_gkw <- gkwfit(data = y, family = "gkw", plot = FALSE)
#'
#' # BKw model: 4 parameters (alpha, beta, gamma, delta)
#' fit_bkw <- gkwfit(data = y, family = "bkw", plot = FALSE)
#'
#' # KKw model: 4 parameters (alpha, beta, delta, lambda)
#' fit_kkw <- gkwfit(data = y, family = "kkw", plot = FALSE)
#'
#' # EKw model: 3 parameters (alpha, beta, lambda)
#' fit_ekw <- gkwfit(data = y, family = "ekw", plot = FALSE)
#'
#' # Mc model: 3 parameters (gamma, delta, lambda)
#' fit_mc <- gkwfit(data = y, family = "mc", plot = FALSE)
#'
#' # Kw model: 2 parameters (alpha, beta)
#' fit_kw <- gkwfit(data = y, family = "kw", plot = FALSE)
#'
#' # Beta model: 2 parameters (gamma, delta)
#' fit_beta <- gkwfit(data = y, family = "beta", plot = FALSE)
#'
#' # Test 1: BKw vs GKw (testing lambda)
#' # H0: lambda=1 (BKw) vs H1: lambda!=1 (GKw)
#' cat("=== Testing BKw vs GKw (adding lambda parameter) ===\n")
#' test_bkw_gkw <- anova(fit_bkw, fit_gkw)
#' print(test_bkw_gkw)
#'
#' # Test 2: KKw vs GKw (testing gamma)
#' # H0: gamma=1 (KKw) vs H1: gamma!=1 (GKw)
#' cat("\n=== Testing KKw vs GKw (adding gamma parameter) ===\n")
#' test_kkw_gkw <- anova(fit_kkw, fit_gkw)
#' print(test_kkw_gkw)
#'
#' # Test 3: Kw vs EKw (testing lambda)
#' # H0: lambda=1 (Kw) vs H1: lambda!=1 (EKw)
#' cat("\n=== Testing Kw vs EKw (adding lambda parameter) ===\n")
#' test_kw_ekw <- anova(fit_kw, fit_ekw)
#' print(test_kw_ekw)
#'
#' # Test 4: Beta vs Mc (testing lambda)
#' # H0: lambda=1 (Beta) vs H1: lambda!=1 (Mc)
#' cat("\n=== Testing Beta vs Mc (adding lambda parameter) ===\n")
#' test_beta_mc <- anova(fit_beta, fit_mc)
#' print(test_beta_mc)
#'
#' # Visualize model comparison
#' # Create dataframe summarizing all models
#' models_df <- data.frame(
#'   Model = c("GKw", "BKw", "KKw", "EKw", "Mc", "Kw", "Beta"),
#'   Parameters = c(
#'     paste("alpha,beta,gamma,delta,lambda"),
#'     paste("alpha,beta,gamma,delta"),
#'     paste("alpha,beta,delta,lambda"),
#'     paste("alpha,beta,lambda"),
#'     paste("gamma,delta,lambda"),
#'     paste("alpha,beta"),
#'     paste("gamma,delta")
#'   ),
#'   Param_count = c(5, 4, 4, 3, 3, 2, 2),
#'   LogLik = c(
#'     as.numeric(logLik(fit_gkw)),
#'     as.numeric(logLik(fit_bkw)),
#'     as.numeric(logLik(fit_kkw)),
#'     as.numeric(logLik(fit_ekw)),
#'     as.numeric(logLik(fit_mc)),
#'     as.numeric(logLik(fit_kw)),
#'     as.numeric(logLik(fit_beta))
#'   ),
#'   AIC = c(
#'     fit_gkw$AIC,
#'     fit_bkw$AIC,
#'     fit_kkw$AIC,
#'     fit_ekw$AIC,
#'     fit_mc$AIC,
#'     fit_kw$AIC,
#'     fit_beta$AIC
#'   ),
#'   BIC = c(
#'     fit_gkw$BIC,
#'     fit_bkw$BIC,
#'     fit_kkw$BIC,
#'     fit_ekw$BIC,
#'     fit_mc$BIC,
#'     fit_kw$BIC,
#'     fit_beta$BIC
#'   )
#' )
#'
#' # Sort by AIC
#' models_df <- models_df[order(models_df$AIC), ]
#' print(models_df)
#'
#' # Create comprehensive visualization
#' # Plot showing model hierarchy and information criteria
#' p1 <- ggplot(models_df, aes(x = Param_count, y = LogLik, label = Model)) +
#'   geom_point(size = 3) +
#'   geom_text(vjust = -0.8) +
#'   labs(
#'     title = "Log-likelihood vs Model Complexity",
#'     x = "Number of Parameters",
#'     y = "Log-likelihood"
#'   ) +
#'   theme_minimal()
#'
#' # Create information criteria comparison
#' models_df_long <- tidyr::pivot_longer(
#'   models_df,
#'   cols = c("AIC", "BIC"),
#'   names_to = "Criterion",
#'   values_to = "Value"
#' )
#'
#' p2 <- ggplot(models_df_long, aes(x = reorder(Model, -Value), y = Value, fill = Criterion)) +
#'   geom_bar(stat = "identity", position = "dodge") +
#'   labs(
#'     title = "Information Criteria Comparison",
#'     x = "Model",
#'     y = "Value (lower is better)"
#'   ) +
#'   theme_minimal() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#'
#' # Print plots
#' print(p1 + p2)
#'
#' # ============================================================================
#' # Manual LR tests to demonstrate underlying calculations
#' # ============================================================================
#' # Function to perform manual likelihood ratio test
#' manual_lr_test <- function(model_restricted, model_full, alpha = 0.05) {
#'   # Extract log-likelihoods
#'   ll_restricted <- as.numeric(logLik(model_restricted))
#'   ll_full <- as.numeric(logLik(model_full))
#'
#'   # Calculate test statistic
#'   lr_stat <- -2 * (ll_restricted - ll_full)
#'
#'   # Calculate degrees of freedom (parameter difference)
#'   df <- length(coef(model_full)) - length(coef(model_restricted))
#'
#'   # Calculate p-value
#'   p_value <- pchisq(lr_stat, df = df, lower.tail = FALSE)
#'
#'   # Return results
#'   list(
#'     lr_statistic = lr_stat,
#'     df = df,
#'     p_value = p_value,
#'     significant = p_value < alpha,
#'     critical_value = qchisq(1 - alpha, df = df)
#'   )
#' }
#'
#' # Example: Manual LR test for BKw vs GKw (testing lambda parameter)
#' cat("\n=== Manual LR test: BKw vs GKw ===\n")
#' lr_bkw_gkw <- manual_lr_test(fit_bkw, fit_gkw)
#' cat("LR statistic:", lr_bkw_gkw$lr_statistic, "\n")
#' cat("Degrees of freedom:", lr_bkw_gkw$df, "\n")
#' cat("P-value:", lr_bkw_gkw$p_value, "\n")
#' cat("Critical value (alpha=0.05):", lr_bkw_gkw$critical_value, "\n")
#' cat("Decision:", ifelse(lr_bkw_gkw$significant,
#'   "Reject H0: Lambda is significantly different from 1",
#'   "Fail to reject H0: Lambda is not significantly different from 1"
#' ), "\n")
#'
#' # Example: Manual LR test for Kw vs EKw (testing lambda parameter)
#' cat("\n=== Manual LR test: Kw vs EKw ===\n")
#' lr_kw_ekw <- manual_lr_test(fit_kw, fit_ekw)
#' cat("LR statistic:", lr_kw_ekw$lr_statistic, "\n")
#' cat("Degrees of freedom:", lr_kw_ekw$df, "\n")
#' cat("P-value:", lr_kw_ekw$p_value, "\n")
#' cat("Critical value (alpha=0.05):", lr_kw_ekw$critical_value, "\n")
#' cat("Decision:", ifelse(lr_kw_ekw$significant,
#'   "Reject H0: Lambda is significantly different from 1",
#'   "Fail to reject H0: Lambda is not significantly different from 1"
#' ), "\n")
#'
#' # ============================================================================
#' # Real data application with correct model nesting
#' # ============================================================================
#' if (requireNamespace("betareg", quietly = TRUE)) {
#'   data("ReadingSkills", package = "betareg")
#'   y <- ReadingSkills$accuracy
#'
#'   # Fit models
#'   rs_gkw <- gkwfit(data = y, family = "gkw", plot = FALSE)
#'   rs_bkw <- gkwfit(data = y, family = "bkw", plot = FALSE)
#'   rs_kkw <- gkwfit(data = y, family = "kkw", plot = FALSE)
#'   rs_kw <- gkwfit(data = y, family = "kw", plot = FALSE)
#'   rs_beta <- gkwfit(data = y, family = "beta", plot = FALSE)
#'
#'   # Test nested models
#'   cat("\n=== Real data: Testing BKw vs GKw (adding lambda) ===\n")
#'   rs_test_bkw_gkw <- anova(rs_bkw, rs_gkw)
#'   print(rs_test_bkw_gkw)
#'
#'   cat("\n=== Real data: Testing Kw vs KKw (adding delta and lambda) ===\n")
#'   rs_test_kw_kkw <- anova(rs_kw, rs_kkw)
#'   print(rs_test_kw_kkw)
#'
#'   # Compare non-nested models with information criteria
#'   cat("\n=== Real data: Comparing non-nested Beta vs Kw ===\n")
#'   rs_compare_beta_kw <- anova(rs_beta, rs_kw)
#'   print(rs_compare_beta_kw)
#'
#'   # Summarize all models
#'   cat("\n=== Real data: Model comparison summary ===\n")
#'   models_rs <- c("GKw", "BKw", "KKw", "Kw", "Beta")
#'   aic_values <- c(rs_gkw$AIC, rs_bkw$AIC, rs_kkw$AIC, rs_kw$AIC, rs_beta$AIC)
#'   bic_values <- c(rs_gkw$BIC, rs_bkw$BIC, rs_kkw$BIC, rs_kw$BIC, rs_beta$BIC)
#'   loglik_values <- c(
#'     as.numeric(logLik(rs_gkw)),
#'     as.numeric(logLik(rs_bkw)),
#'     as.numeric(logLik(rs_kkw)),
#'     as.numeric(logLik(rs_kw)),
#'     as.numeric(logLik(rs_beta))
#'   )
#'
#'   df_rs <- data.frame(
#'     Model = models_rs,
#'     LogLik = loglik_values,
#'     AIC = aic_values,
#'     BIC = bic_values
#'   )
#'   df_rs <- df_rs[order(df_rs$AIC), ]
#'   print(df_rs)
#'
#'   # Determine the best model for the data
#'   best_model <- df_rs$Model[which.min(df_rs$AIC)]
#'   cat("\nBest model based on AIC:", best_model, "\n")
#' }
#' }
#'
#' @keywords models methods regression htest
#' @author Lopes, J. E.
#' @export
anova.gkwfit <- function(object, ...) {
  # --- Gather objects and perform initial validation ---
  objects <- list(object, ...)
  is_gkwfit <- sapply(objects, inherits, "gkwfit")
  if (!all(is_gkwfit)) {
    stop("All objects provided must inherit from class 'gkwfit'.")
  }
  nmodels <- length(objects)
  if (nmodels < 2) {
    stop("Need at least two models to compare using anova().")
  }

  # Use substitute to capture original variable names if possible
  object_names_call <- match.call()
  object_names <- vapply(as.list(object_names_call[-1L])[seq_len(nmodels)], deparse1, "")

  lls <- vector("list", nmodels)
  for (i in seq_len(nmodels)) {
    lls[[i]] <- tryCatch(logLik(objects[[i]]), error = function(e) {
      stop("Could not extract valid 'logLik' object for model ", object_names[i], ": ", e$message)
    })
    if (!inherits(lls[[i]], "logLik") || is.null(attr(lls[[i]], "df")) || is.null(attr(lls[[i]], "nobs"))) {
      stop(
        "Invalid 'logLik' object returned for model ", object_names[i],
        " (missing class or 'df'/'nobs' attribute)."
      )
    }
    if (attr(lls[[i]], "nobs") <= 0) {
      stop("Number of observations ('nobs') must be positive for model ", object_names[i])
    }
  }

  dfs <- sapply(lls, attr, "df")
  nobs_vals <- sapply(lls, attr, "nobs")
  loglik_vals <- sapply(lls, as.numeric)

  if (length(unique(nobs_vals)) > 1) {
    warning(
      "Models were not all fitted to the same number of observations.\n",
      "Likelihood ratio tests assume comparison on the same dataset."
    )
  }
  n_obs <- nobs_vals[1] # Use first for AIC/BIC calculation, assumes consistency or accepts warning

  mnames <- object_names
  if (anyDuplicated(mnames)) mnames <- make.unique(mnames, sep = ".")

  ord <- order(dfs)
  objects <- objects[ord]
  lls <- lls[ord]
  dfs <- dfs[ord]
  loglik_vals <- loglik_vals[ord]
  mnames <- mnames[ord]

  aics <- -2 * loglik_vals + 2 * dfs
  bics <- -2 * loglik_vals + log(n_obs) * dfs

  # --- Perform pairwise LRTs ---
  lr_stat <- rep(NA_real_, nmodels)
  pr_chi <- rep(NA_real_, nmodels)
  delta_df <- rep(NA_integer_, nmodels)
  test_desc <- rep("", nmodels)

  for (i in 2:nmodels) {
    df_diff <- dfs[i] - dfs[i - 1]

    # Check if degrees of freedom increased (necessary for meaningful LRT)
    if (df_diff <= 0) {
      warning("Model ", mnames[i], " (", dfs[i], " df)",
        " does not have more parameters than model ", mnames[i - 1], " (", dfs[i - 1], " df).",
        "\nLRT requires models to be nested with increasing complexity.",
        call. = FALSE
      )
      next # Skip LRT calculation for this pair
    }

    # Check if log-likelihood increased (as expected for nested models)
    loglik_diff <- loglik_vals[i] - loglik_vals[i - 1]
    if (loglik_diff < -1e-6) { # Allow for small numerical errors
      warning("Log-likelihood decreased unexpectedly for the more complex model (",
        mnames[i], " vs ", mnames[i - 1], ").\nCheck model convergence or nesting.",
        call. = FALSE
      )
      lr <- NA_real_ # LRT statistic is not meaningful
    } else {
      lr <- max(0, 2 * loglik_diff) # Ensure non-negative LR statistic
    }

    delta_df[i] <- df_diff
    lr_stat[i] <- lr
    # Calculate p-value only if LR is not NA
    pr_chi[i] <- if (!is.na(lr)) stats::pchisq(lr, df_diff, lower.tail = FALSE) else NA_real_
    test_desc[i] <- paste(i - 1, "vs", i)
  }

  result_table <- data.frame(
    `N.Par` = dfs,
    AIC = aics,
    BIC = bics,
    LogLik = loglik_vals,
    Test = test_desc,
    `LR stat.` = lr_stat,
    `Pr(>Chi)` = pr_chi,
    row.names = mnames,
    check.names = FALSE # Prevent conversion of Pr(>Chi) etc.
  )

  heading <- c("Likelihood Ratio Test Comparison\n")
  attr(result_table, "heading") <- heading
  class(result_table) <- c("anova.gkwfit", "anova", "data.frame")

  return(result_table)
}

#' @title S3 method for class 'anova.gkwfit'
#' @param x An object of class \code{"anova.gkwfit"}.
#' @param digits Minimum number of significant digits to print.
#' @param signif.stars Logical; if TRUE, add significance stars.
#' @param ... Other args passed to
#' @return An object of class \code{"anova.gkwfit"} and prints a summary.
#' @export
print.anova.gkwfit <- function(x, digits = max(getOption("digits") - 2L, 3L),
                               signif.stars = getOption("show.signif.stars", TRUE), ...) {
  if (!inherits(x, "anova")) stop("x not of class anova") # Basic check

  # Print header if it exists
  if (!is.null(heading <- attr(x, "heading"))) {
    cat(heading, "\n")
  }

  # Use printCoefmat for standard ANOVA table formatting
  # Identify the P-value column index
  pval_col_idx <- which(colnames(x) == "Pr(>Chi)")
  # Identify the test statistic column index
  test_col_idx <- which(colnames(x) == "LR stat.")
  if (length(pval_col_idx) == 0) pval_col_idx <- NULL # Handle if column name changes
  if (length(test_col_idx) == 0) test_col_idx <- NULL

  # stats:::printCoefmat is not exported, use stats::printCoefmat directly
  # We need to tell it which column contains p-values
  stats::printCoefmat(x,
    digits = digits, signif.stars = signif.stars,
    has.Pvalue = !is.null(pval_col_idx), # Does it have P-values?
    P.values = !is.null(pval_col_idx), # Are they P-values? (for formatting)
    cs.ind = integer(), # No specific estimate/SE cols
    tst.ind = test_col_idx, # Which column has test stats?
    zap.ind = integer(),
    na.print = "", ...
  ) # Don't print NA for test on first line
  cat("\n")
  invisible(x)
}
