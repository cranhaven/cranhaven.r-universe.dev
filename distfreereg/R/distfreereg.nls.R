distfreereg.nls <-
  function(test_mean,
           ordering = "simplex",
           group = FALSE,
           stat = c("KS", "CvM"),
           B = 1e4,
           control = NULL,
           override = NULL,
           verbose = TRUE,
           ...){
    cl <- match.call()
    
    extra_arg_list <- list(...)
    
    if(!is.null(test_mean[["na.action"]])){
      if(isTRUE(verbose)) message("'na.action' must be reset; refitting nls model...")
      m <- update(object = test_mean, na.action = na.fail)
    } else {
      m <- test_mean
    }
    
    n <- length(fitted(m))
    
    control <- validate_args_distfreereg_nls(test_mean = m, n = n,
                                             control = control,
                                             extra_arg_list = extra_arg_list,
                                             verbose = verbose)

    # Get the covariance.
    covariance <- get_cov(m, verbose = verbose)
    covariance <- fill_covariance_list(need = "SqrtSigma",
                                       covariance_list = covariance,
                                       matsqrt_tol = control[["matsqrt_tol"]],
                                       solve_tol = control[["solve_tol"]])
    
    # Get the estimated Jacobian and the fitted values.
    if(is.null(override[["J"]])){
      if(isTRUE(verbose)) message("Retrieving Jacobian from 'nls' object...")
      J <- m[["m"]][["gradient"]]()
      # The returned value is the normalized gradient; that is, the gradient of
      # the original function left multiplied by the inverse square root of the
      # covariance. For consistency with what comes later, this multiplication
      # must be undone.
      if(is.matrix(covariance[["SqrtSigma"]])){
        J <- covariance[["SqrtSigma"]] %*% J
      } else {
        J <- covariance[["SqrtSigma"]] * J
      }
    } else {
      if(isTRUE(verbose)) message("Using supplied Jacobian...")
      J <- override[["J"]]
      override[["J"]] <- NULL
    }
    
    if(is.null(override[["fitted_values"]])){
      if(isTRUE(verbose)) message("Retrieving fitted values from 'nls' object...")
      fitted_values <- fitted(m)
    } else {
      if(isTRUE(verbose)) message("Using supplied fitted values...")
      fitted_values <- override[["fitted_values"]]
      override[["fitted_values"]] <- NULL
    }
    
    data <- get_nls_data(m)
    X <- data
    X[[get_response(formula(m))]] <- NULL
    X <- as.matrix(X)
    Y <- m[["m"]][["lhs"]]()
    
    control <- define_data_for_res_order(test_mean = m, data = data,
                                         ordering = ordering,
                                         override = override, control = control)
    
    dfr_default_output <-
      distfreereg.default(Y = Y, X = X, covariance = covariance, ordering = ordering,
                          group = group, J = J, fitted_values = fitted_values,
                          stat = stat, B = B, control = control, override = override,
                          verbose = verbose)
    
    output <- list(
      call = cl,
      data = list(data = get_nls_data(m), Y = Y, X = X),
      test_mean = m,
      # test_mean_function = NULL,
      covariance = dfr_default_output[["covariance"]],
      theta_hat = m[["m"]][["getPars"]](),
      optimization_output = m,
      fitted_values = dfr_default_output[["fitted_values"]],
      J = dfr_default_output[["J"]],
      mu = dfr_default_output[["mu"]],
      r = dfr_default_output[["r"]],
      r_tilde = dfr_default_output[["r_tilde"]],
      residuals = list(raw = dfr_default_output[["residuals"]][["raw"]],
                       sphered = dfr_default_output[["residuals"]][["sphered"]],
                       transformed = dfr_default_output[["residuals"]][["transformed"]]),
      res_order = dfr_default_output[["res_order"]],
      epsp = dfr_default_output[["epsp"]],
      observed_stats = dfr_default_output[["observed_stats"]],
      mcsim_stats = dfr_default_output[["mcsim_stats"]],
      p = list(value = dfr_default_output[["p"]][["value"]], mcse = dfr_default_output[["p"]][["mcse"]])
    )
    
    class(output) <- "distfreereg"
    
    return(output)
  }
