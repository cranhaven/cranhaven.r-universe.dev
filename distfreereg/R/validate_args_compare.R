validate_args_compare <-
  function(true_mean,
           true_method = true_method,
           true_method_args = true_method_args,
           true_covariance,
           true_X,
           true_data,
           theta,
           n,
           reps,
           prog,
           err_dist_fun,
           err_dist_args,
           keep,
           manual,
           update_args,
           arg_list){
    if(is.function(true_mean) && !is.null(true_data)){
      stop("true_data must not be supplied when true_mean is a function")
    }
    
    if(is(true_mean, "formula")){
      strict_match(arg = true_method, choices = c("lm", "nls"))
      if(!is.null(true_method_args) && !is.list(true_method_args))
        stop("true_method_args must be NULL or a list")
      if(is.null(true_data)) stop("true_data must be supplied when true_mean is a formula")
      if(is(true_mean, "formula") && identical(true_method, "nls") && is.null(names(theta)))
        stop("theta must be a named vector when true_method is 'nls'")
    } else {
      if(!is.null(true_method)) stop("true_method must be NULL when true_mean is not a formula")
      if(!is.null(true_method_args)) stop("true_method_args must be NULL when true_mean is not a formula")
      if(!is.function(true_mean)){
        if(!is.null(true_data)) stop("true_data must be NULL when true_mean is a model object")
      }
      if(is(true_mean, "nls") && is.null(names(theta)))
        stop("theta must be a named vector when true_mean is an 'nls' object")
    }
    
    test_mean <- arg_list[["test_mean"]]
    if(is.null(test_mean)) stop("test_mean must be specified")
    data <- arg_list[["data"]]
    X <- arg_list[["X"]]
    
    if(!is.null(arg_list[["Y"]])){
      warning("Y supplied to compare() is ignored")
      arg_list[["Y"]] <- NULL
    }
    
    
    if(!is.null(data)){
      if(is.matrix(data)){
        data <- as.data.frame(data)
      } else {
        if(!is.data.frame(data)) stop("data must be a data frame or a matrix")
      }
      if(nrow(data) == 0) stop("data cannot be an empty matrix or data frame")
    }
    
    # Check for appropriate test covariate specification.
    if(!is.null(data)){
      if(is(test_mean, "lm")) stop("data must be NULL when test_mean is an lm object")
      if(is(test_mean, "nls")) stop("data must be NULL when test_mean is an nls object")
    }
    if(is.function(test_mean)){
      if(!is.null(data)) stop("'data' must be NULL when test_mean is a function")  
      if(isTRUE("x" %in% names(formals(test_mean))) && is.null(X))
        stop("X must be specified when test_mean is a function with an 'x' argument")
    }
    
    test_mean_formula <- tryCatch(formula(test_mean), error = function(e) NULL)
    if(is.null(test_mean_formula)){
      if(!is.null(data)) stop("data must be NULL when test_mean has no formula method")
    } else {
      if(!is.null(X)) stop("X must be NULL when test_mean has a formula method")
    }
    
    if(is(test_mean_formula, "formula")){
      if(!is.null(X)) stop("X must be NULL when test_mean has a formula method")
      test_Y <- tryCatch(get_response(test_mean),
                         error = function(e) warning("Error in extracting response variable name: ", e))
      if(is.null(test_Y)) stop("Response variable name not found in test_mean object's formula")
      if(!is.character(test_Y)) stop("Response variable name found in test_mean object is not a character!")
      if(length(test_Y) != 1) stop("Response in test_mean object must be exactly one term")
    } else {
      test_Y <- NULL
    }
    
    if(!is.null(X)){
      validate_numeric(x = X)
      X <- as.matrix(X)
    }

    if(is.function(true_mean)){
      if(is.null(true_X)){
        if(any(c("x", "X") %in% names(formals(true_mean))))
          stop("true_mean must have neither 'x' nor 'X' as an argument when true_X is NULL")
      } else {
        if(!(any(c("x", "X") %in% names(formals(true_mean)))))
          stop("true_mean must have 'x' or 'X' as an argument when true_X is not NULL")
        validate_numeric(x = true_X)
        true_X <- as.matrix(true_X)
      }
    } else {
      if(!is.null(true_X)) stop("true_X must be NULL if true_mean is not a function")
      if(!is(true_mean, "formula") && !is.null(true_data))
        stop("true_data must be NULL when true_mean is a model object")
    }

    # Determine n    
    if(!is.null(n)){
      validate_numeric(x = n, pos_int = TRUE, len = 1)
      n <- as.integer(n)
    }
    n <- get_n(n = n, true_X, X, true_data, data, true_mean, test_mean)

    # Validate format of covariance lists.
    symmetric <- arg_list[["control"]][["symmetric"]]
    if(!isFALSE(symmetric) && !is.null(symmetric) && !is_named_list(symmetric))
      stop("control element \"symmetric\" must be NULL, FALSE, or a named list")
    
    validate_covariance_list(true_covariance, n = n, allow_function = FALSE,
                             require_diagonal = FALSE, symmetric = symmetric)
    if(!is.null(arg_list[["covariance"]])){
      validate_covariance_list(arg_list[["covariance"]], n = n,
                               allow_function = FALSE, require_diagonal = FALSE,
                               symmetric = symmetric)
    }
    
    # Validate true mean function.
    if(is.function(true_mean)){
      validate_test_mean_function_arg_names(f = true_mean)
      if(!("theta" %in% names(formals(true_mean))))
        stop("true_mean must contain an argument named \"theta\"")
      
      # Verify that the true mean function can be evaluated at starting values.
      f_out <- tryCatch(f2ftheta(f = true_mean, X = true_X, n = n)(theta),
                        error = function(e) stop("Unable to evaluate true_mean(true_X, theta): ", e))
      validate_numeric(x = f_out, len = n)
    }
    
    
    # Validate other arguments.
    validate_numeric(x = theta, min_len = 1)
    validate_numeric(x = reps, min_val = 0, len = 1)
    reps <- as.integer(reps)
    
    # Note that the following call to validate_numeric() prog does not use
    # pos_int, since the default value reps/10 is 0 for reps < 10, so only
    # require positive and then convert 0 to 1.
    validate_numeric(x = prog, finite = FALSE, positive = TRUE, len = 1)
    if(is.finite(prog)) prog <- as.integer(prog)
    if(prog == 0) prog <- 1
    
    # Check for compatibility of err_dist_fun and err_dist_args.
    if(!isTRUE(is.function(err_dist_fun)))
      stop("No function found named '", deparse1(substitute(err_dist_fun)), "'")
    if(!is.null(err_dist_args) && !is.list(err_dist_args))
      stop("err_dist_args must be a list")
    
    if(!is.null(keep) && !identical("all", keep)){
      if(!is.numeric(keep)){
        stop("keep_all must be NULL, \"all\", or a vector of positive integers")
      } else {
        validate_numeric(keep, pos_int = TRUE, min_len = 1, max_len = reps,
                         min_val = 1, max_val = reps)
        keep <- sort(unique(keep))
      }
    }
    if(!is.null(manual) && !is.function(manual))
      stop("\"manual\" must be NULL or a function")
    
    # Define default matsqrt_tol value if NULL.
    if(is.null(arg_list[["control"]][["matsqrt_tol"]])){
      matsqrt_tol <- default_distfreereg_tol()[["matsqrt_tol"]]
    } else {
      matsqrt_tol <- arg_list[["control"]][["matsqrt_tol"]]
    }
    validate_numeric(matsqrt_tol, max_val = 0, len = 1)

    # Define default solve_tol value if NULL.
    if(is.null(arg_list[["control"]][["solve_tol"]])){
      solve_tol <- default_distfreereg_tol()[["solve_tol"]]
    } else {
      solve_tol <- arg_list[["control"]][["solve_tol"]]
    }
    validate_numeric(solve_tol, positive = TRUE, len = 1)
    
    return(list(reps = reps, prog = prog, true_X = true_X, test_mean = test_mean,
                X = X, test_Y = test_Y, data = data, n = n,
                keep = keep, arg_list = arg_list, solve_tol = solve_tol,
                matsqrt_tol = matsqrt_tol))
  }
