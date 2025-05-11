distfreereg.lm <-
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
    
    validate_args_distfreereg_lm(test_mean = test_mean,
                                 extra_arg_list = extra_arg_list)
    
    if(is.null(test_mean[["x"]]) || is.null(test_mean[["y"]]) || !is.null(test_mean[["na.action"]])){
      if(isTRUE(verbose)) message("'x' and/or 'y' not found in lm object, or 'na.action' must be reset; refitting linear model...")
      m <- update(object = test_mean, x = TRUE, y = TRUE, na.action = na.fail)
    } else {
      m <- test_mean
    }
    
    X <- m[["x"]]; Y <- m[["y"]]
    n <- length(Y)

    # Get the covariance.
    covariance <- get_cov(test_mean, verbose = verbose)

    # Get the estimated Jacobian and the fitted values.
    if(is.null(override[["J"]])){
      if(isTRUE(verbose)) message("Calculating Jacobian from lm object...")
      J <- get_lm_jacobian(m)
    } else {
      if(isTRUE(verbose)) message("Using supplied Jacobian...")
      J <- override[["J"]]
      override[["J"]] <- NULL
    }
    
    if(is.null(override[["fitted_values"]])){
      if(isTRUE(verbose)) message("Retrieving fitted values from 'lm' object...")
      fitted_values <- fitted(m)
    } else {
      if(isTRUE(verbose)) message("Using supplied fitted values...")
      fitted_values <- override[["fitted_values"]]
      override[["fitted_values"]] <- NULL
    }
    
    
    control <- define_data_for_res_order(test_mean = test_mean, data = test_mean[["model"]],
                                         ordering = ordering,
                                         override = override, control = control)
    
    dfr_default_output <-
      distfreereg.default(Y = Y, X = X, covariance = covariance, ordering = ordering,
                          group = group, J = J, fitted_values = fitted_values,
                          stat = stat, B = B, control = control, override = override,
                          verbose = verbose)
    
    output <- list(
      call = cl,
      data = list(data = m[["model"]], Y = Y, X = X),
      test_mean = m,# makes sure that output includes x and y in lm object
      covariance = dfr_default_output[["covariance"]],
      theta_hat = coefficients(m),
      optimization_output = NULL,
      fitted_values = fitted_values,
      J = J,
      mu = dfr_default_output[["mu"]],
      r = dfr_default_output[["r"]],
      r_tilde = dfr_default_output[["r_tilde"]],
      residuals = dfr_default_output[["residuals"]],
      res_order = dfr_default_output[["res_order"]],
      epsp = dfr_default_output[["epsp"]],
      observed_stats = dfr_default_output[["observed_stats"]],
      mcsim_stats = dfr_default_output[["mcsim_stats"]],
      p = list(value = dfr_default_output[["p"]][["value"]], mcse = dfr_default_output[["p"]][["mcse"]])
    )
    
    class(output) <- "distfreereg"
    
    return(output)
  }
