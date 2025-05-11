validate_args_distfreereg_formula <-
  function(test_mean, data, covariance, method, theta_init, override, control,
           verbose, extra_arg_list){
    validate_extra_arg_list(extra_arg_list, "distfreereg.formula()")

    control <- combine_lists(control, default_distfreereg_tol())
    validate_named_list(control)
    validate_control_tols(control)
    
    if(!is(test_mean, "formula")) stop("test_mean must be a formula")
    validate_single_response_term(form = test_mean)
    strict_match(arg = method, choices = c("lm", "nls"))
    
    if(identical(method, "nls") && !is.null(theta_init) && is.null(names(theta_init)))
      stop("theta_init must be a named vector when method is 'nls'")
    
    if(!is.data.frame(data)) stop("'data' must be a data frame")
    n <- nrow(data)
    if(n == 0) stop("'data' must have at least one row")
    
    if(!is.null(covariance) && !is.null(control[["method_args"]][["weights"]]))
      stop("At most one of 'covariance' and 'weights' may be specified")
    
    method_args <- control[["method_args"]]
    if(!is.null(method_args) && !is.list(method_args))
      stop("method_args must be a list")

    if(!is.null(override[["theta_hat"]]))
      stop("distfreereg.formula() argument 'override' cannot have 'theta_hat' element")

    if(!is.null(covariance)){
      if(isTRUE(verbose)) message("Converting covariance specification to weights...")
      # covariance <- eval_cov(covariance, data = data)
      validate_covariance_list(covariance, n = n, allow_function = FALSE,
                               require_diagonal = TRUE, symmetric = FALSE)
      covariance <- fill_covariance_list(need = "P", covariance,
                                         matsqrt_tol = control[["matsqrt_tol"]],
                                         solve_tol = control[["solve_tol"]])
      w <- covariance2weights(covariance[["P"]], n = n)
      control[["method_args"]][["weights"]] <- w
    }
    
    return(list(covariance = covariance, control = control))
  }
