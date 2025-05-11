distfreereg.default <-
  function(test_mean = NULL,
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
           J,
           fitted_values){
    cl <- match.call()
    
    extra_arg_list <- list(...)
    
    vargs <-
      validate_args_distfreereg_default(Y = Y, X = X, covariance = covariance,
                                        ordering = ordering, J = J, fitted_values = fitted_values,
                                        stat = stat, B = B, control = control,
                                        override = override, verbose = verbose,
                                        extra_arg_list = extra_arg_list)
    Y <- vargs[["Y"]]; X <- vargs[["X"]]; n <- vargs[["n"]]; J <- vargs[["J"]]
    covariance = vargs[["covariance"]]; matsqrt_tol = vargs[["matsqrt_tol"]]
    solve_tol = vargs[["solve_tol"]]; qr_tol <- vargs[["qr_tol"]]
    orth_tol <- vargs[["orth_tol"]]; trans_tol <- vargs[["trans_tol"]]
    B <- vargs[["B"]]
    
    p <- ncol(J)
    
    if(is.null(covariance[["Q"]])){
      if(isTRUE(verbose)) message("Calculating the inverse square root of the covariance matrix...")
      covariance <- fill_covariance_list(need = "Q", covariance_list = covariance,
                                         matsqrt_tol = matsqrt_tol, solve_tol = solve_tol)
    } else{
      if(isTRUE(verbose)) message("Using supplied inverse square root of the covariance matrix...")
    }
    Q <- covariance[["Q"]]
    if(any(diag(as.matrix(Q)) == 0)) warning("At least one diagonal element of Q is indistinguishable from zero")
    
    if(is.matrix(Q)){
      J_for_mu <- Q %*% J
    } else {
      J_for_mu <- Q * J
    }
    
    if(isTRUE(verbose)) message("Calculating mu...")
    mu <- calc_mu(J = J_for_mu, matsqrt_tol = matsqrt_tol, solve_tol = solve_tol)
    
    if(is.null(override[["res_order"]])){
      X_for_ordering <- if(is.null(control[["data"]])) X else control[["data"]]
      res_order <- determine_order(X = X_for_ordering, ordering = ordering,
                                   n = n, verbose = verbose)
    } else {
      if(isTRUE(verbose)) message("Using supplied observation ordering...")
      res_order <- override[["res_order"]]
    }
    # At least one external function can be called by determine_order(), so one
    # final verification that the output is a permutation.
    if(!isTRUE(isPermutation(res_order))) stop("res_order not a permutation")
    if(length(res_order) != n) stop("res_order and Y must have the same length")
    
    # Define r    
    if(is.null(override[["r"]])){
      if(isTRUE(verbose)) message("Calculating transformation anchors...")
      r <- define_r(n = n, p = p, res_order = res_order, gs_tol = qr_tol)
    } else {
      if(isTRUE(verbose)) message("Using supplied transformation anchors...")
      r <- as.matrix(override[["r"]])
      if(any(dim(mu) != dim(r))) stop("Supplied r has wrong dimension")
    }
    
    # Validate r
    rtr <- crossprod(r)
    if(!isTRUE(all.equal(rtr, diag(nrow(rtr)), check.attributes = FALSE,
                         tolerance = orth_tol))){
      stop("Columns of r are not orthogonal")
    }
    
    r_tilde <- define_r_tilde(r = r, mu = mu, k2_tol = trans_tol)
    
    if(isTRUE(verbose)) message("Calculating residuals...")
    raw_residuals <- calc_resid(Y = Y, fitted_values = fitted_values)
    validate_numeric(raw_residuals, min_len = 1, func = warning,
                     message = "Raw residuals failed validation: ")
    
    sphered_residuals <- calc_sphered_resid(raw_residuals = raw_residuals, Q = Q)
    validate_numeric(sphered_residuals, min_len = 1, func = warning,
                     message = "Sphered residuals failed validation: ")
    
    transformed_residuals <- calc_k2_resid(x = sphered_residuals,
                                           r_tilde = r_tilde, mu = mu,
                                           k2_tol = trans_tol)
    validate_numeric(transformed_residuals, min_len = 1, func = warning,
                     message = "Transformed residuals failed validation: ")
    
    if(isTRUE(group)){
      # control[["data"]] should NOT have response in it here if "ordering" is
      # "natural".
      X_for_am <- if(is.null(control[["data"]])) X else as.matrix(control[["data"]])
      if(identical(ordering, "natural") || is.list(ordering)){
        if(identical(ordering, "natural")) ordering <- as.list(seq_len(ncol(X_for_am)))
        aggregator_matrix <- define_aggregator_matrix(X = X_for_am, ordering = ordering)
      } else {
        warning("grouping only available when 'ordering' is 'natural' or a list of column specifications; ",
                "no grouping done")
      }
    } else {
      aggregator_matrix <- NULL
    }
    
    epsp <- calc_epsp(transformed_residuals = transformed_residuals,
                      res_order = res_order,
                      aggregator_matrix = aggregator_matrix)
    validate_numeric(epsp, min_len = 1, func = warning,
                     message = "Partial sum process failed validation: ")
    
    if(isTRUE(verbose)) message("Calculating observed statistic(s)...")
    observed_stats <- as.list(calc_stats(epsp = epsp, stat = stat))
    names(observed_stats) <- stat
    
    if(is.null(override[["mcsim_stats"]])){
      if(isTRUE(verbose)) message("Running Monte Carlo simulation...")
      mcsim_stats <- mcsim(B = B, r = r, aggregator_matrix = aggregator_matrix,
                           stat = stat, verbose = verbose)
    } else {
      if(isTRUE(verbose)) message("Using supplied Monte Carlo simulation results...")
      mcsim_stats <- override[["mcsim_stats"]]
    }
    
    # Now redefine "p" to hold p values (not dimension of parameter space).
    p <- calc_p(observed_stats = observed_stats, mcsim_stats = mcsim_stats)
    mcse <- calc_mcse_binom(p = p, B = B)
    
    output <- list(
      call = cl,
      data = list(Y = Y, X = X),
      covariance = covariance,
      fitted_values = fitted_values,
      J = J,
      mu = mu,
      r = r,
      r_tilde = r_tilde,
      residuals = list(raw = raw_residuals, sphered = sphered_residuals,
                       transformed = transformed_residuals),
      res_order = res_order,
      aggregator_matrix = aggregator_matrix,
      epsp = epsp,
      observed_stats = observed_stats,
      mcsim_stats = mcsim_stats,
      p = list(value = p, mcse = mcse)
    )
    
    class(output) <- "distfreereg"
    
    return(output)
  }
