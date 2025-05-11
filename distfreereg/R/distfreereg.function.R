distfreereg.function <-
  function(test_mean,
           ordering = "simplex",
           group = FALSE,
           stat = c("KS", "CvM"),
           B = 1e4,
           control = NULL,
           override = NULL,
           verbose = TRUE,
           ...,
           Y,
           X = NULL,
           covariance,
           theta_init){
    cl <- match.call()
    
    extra_arg_list <- list(...)
    
    vargs <-
      validate_args_distfreereg_function(Y = Y, X = X, test_mean = test_mean, covariance = covariance,
                                         theta_init = theta_init, ordering = ordering,
                                         stat = stat, B = B,
                                         control = control, override = override, verbose = verbose,
                                         extra_arg_list = extra_arg_list)
    Y <- vargs[["Y"]]; X <- vargs[["X"]]; n <- vargs[["n"]]; J <- vargs[["J"]]
    covariance = vargs[["covariance"]]; matsqrt_tol = vargs[["matsqrt_tol"]]
    solve_tol = vargs[["solve_tol"]]; qr_tol <- vargs[["qr_tol"]]
    orth_tol <- vargs[["orth_tol"]]; trans_tol <- vargs[["trans_tol"]]
    optimization_fun <- vargs[["optimization_fun"]]
    optimization_args <- vargs[["optimization_args"]]
    theta_hat_name <- vargs[["theta_hat_name"]]
    fun_to_optimize_arg <- vargs[["fun_to_optimize_arg"]]
    jacobian_args = vargs[["jacobian_args"]]; B <- vargs[["B"]]
    
    if(is.null(covariance[["Q"]])){
      if(isTRUE(verbose)) message("Calculating the inverse square root of the covariance matrix...")
      covariance <- fill_covariance_list(need = "Q", covariance_list = covariance,
                                         matsqrt_tol = matsqrt_tol, solve_tol = solve_tol)
    } else{
      if(isTRUE(verbose)) message("Using supplied inverse square root of the covariance matrix...")
    }
    Q <- covariance[["Q"]]
    
    ftheta <- f2ftheta(test_mean, X, n)
    if(is.null(override[["theta_hat"]])){
      if(isTRUE(verbose)) message("Estimating parameters...")
      optimization_output <- calc_theta_hat(ftheta = ftheta, Y = Y, Q = Q,
                                            optimization_fun = optimization_fun,
                                            optimization_args = optimization_args,
                                            fun_to_optimize_arg = fun_to_optimize_arg)
      theta_hat <- optimization_output[[theta_hat_name]]
      if(is.null(theta_hat))
        stop("No element with name \"", theta_hat_name, "\" found in optimization output")
    } else {
      if(isTRUE(verbose)) message("Using supplied parameter estimates...")
      theta_hat <- override[["theta_hat"]]
    }
    
    
    if(is.null(override[["fitted_values"]])){
      if(isTRUE(verbose)) message("Calculating fitted values...")
      fitted_values <- ftheta(theta_hat)
    } else {
      if(isTRUE(verbose)) message("Using supplied fitted values...")
      fitted_values <- override[["fitted_values"]]
      override[["fitted_values"]] <- NULL
    }
    
    if(is.null(override[["J"]])){
      if(isTRUE(verbose)) message("Calculating Jacobian...")
      J <- calc_jacobian(ftheta = ftheta, theta_hat = theta_hat,
                         jacobian_args = jacobian_args)
    } else {
      if(isTRUE(verbose)) message("Using supplied Jacobian...")
      J <- override[["J"]]
      override[["J"]] <- NULL
    }
    
    # Prevent verification of symmetry in further methods.
    control[["symmetric"]] <- FALSE
    
    # Remove elements of control and override that will not be used by default
    # method.
    for(var in c("jacobian_args", "optimization_fun", "fun_to_optimize_arg",
                 "optimization_args", "theta_hat_name", "theta_init_arg")){
      control[[var]] <- NULL
    }
    override[["theta_hat"]] <- NULL
    
    dfr_default_output <-
      distfreereg.default(test_mean = NULL, Y = Y, X = X, covariance = covariance,
                          ordering = ordering, group = group, J = J,
                          fitted_values = fitted_values, stat = stat, B = B,
                          control = control, override = override,
                          verbose = verbose)
    
    output <- list(
      call = cl,
      data = list(Y = Y, X = X),
      test_mean = test_mean,
      covariance = covariance,
      theta_hat = theta_hat,
      optimization_output = if(exists("optimization_output")) optimization_output else NULL,
      fitted_values = fitted_values,
      J = J,
      mu = dfr_default_output[["mu"]],
      r = dfr_default_output[["r"]],
      r_tilde = dfr_default_output[["r_tilde"]],
      residuals = list(raw = dfr_default_output[["residuals"]][["raw"]],
                       sphered = dfr_default_output[["residuals"]][["sphered"]],
                       transformed = dfr_default_output[["residuals"]][["transformed"]]),
      res_order = dfr_default_output[["res_order"]],
      aggregator_matrix = dfr_default_output[["aggregator_matrix"]],
      epsp = dfr_default_output[["epsp"]],
      observed_stats = dfr_default_output[["observed_stats"]],
      mcsim_stats = dfr_default_output[["mcsim_stats"]],
      p = list(value = dfr_default_output[["p"]][["value"]], mcse = dfr_default_output[["p"]][["mcse"]])
    )
    
    class(output) <- "distfreereg"
    
    return(output)
  }
