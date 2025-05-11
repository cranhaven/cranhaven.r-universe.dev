validate_args_distfreereg_function <-
  function(Y, X, test_mean, covariance, theta_init, ordering,
           stat, B, override, verbose, control, extra_arg_list){
    
    validate_extra_arg_list(extra_arg_list, "distfreereg.function()")
    
    if(!is.null(control))
      validate_named_list(control, valid_names = c("matsqrt_tol", "solve_tol",
                                                   "qr_tol", "orth_tol", "trans_tol",
                                                   "symmetric", "jacobian_args",
                                                   "optimization_fun",
                                                   "fun_to_optimize_arg",
                                                   "optimization_args",
                                                   "theta_hat_name", "theta_init_arg",
                                                   "data"))# "data" only here for ordering
    
    control <- combine_lists(control, default_distfreereg_tol())
    validate_control_tols(control)
    
    symmetric <- control[["symmetric"]]
    if(!isFALSE(symmetric) && !is.null(symmetric) && !is_named_list(symmetric))
      stop("control element \"symmetric\" must be NULL, FALSE, or a named list")
    
    # Verify that Y is numeric and has at least two entries.
    if(!is.vector(Y)){
      if(isTRUE(verbose)) message("Coercing Y to vector...")
      Y <- tryCatch(as.vector(Y), error = function(e) stop("Error converting Y to a vector: ", e))
    }
    validate_numeric(x = Y, min_len = 2, message = "Y failed numeric validation: ")
    n <- length(Y)
    
    # Verify that X is numeric, and convert to a matrix in case it is just a
    # vector.
    if(!is.null(X)){
      if(!is.matrix(X)){
        if(isTRUE(verbose)) message("Coercing X to matrix...")
        X <- tryCatch(as.matrix(X), error = function(e) stop("Error converting X to a matrix: ", e))
      }
      validate_numeric(x = X, message = "X failed numeric validation: ")
      column_value_counts <- apply(X, MARGIN = 2, function(x) length(unique(x)))
      if(!identical(colnames(X)[1], "(Intercept)") && isTRUE(any(column_value_counts == 1)))
        warning("At least one column of X contains only one value. Verify that the model is identifiable.")
    }
    
    
    
    # Verify that override is a named list.
    if(!is.null(override)){
      validate_named_list(override, valid_names = c("J", "fitted_values",
                                                    "res_order", "theta_hat",
                                                    "r", "mcsim_stats"))
      if(!is.null(override[["res_order"]])){
        if(!isPermutation(override[["res_order"]])) stop("override[[\"res_order\"]] must be a permutation")
        if(length(override[["res_order"]]) != n) stop("override[[\"res_order\"]] and Y must have the same length")
      }
      
      if(!is.null(override[["theta_hat"]]))
        validate_numeric(override[["theta_hat"]],
                         message = "Override theta_hat value failed numeric validation: ")
      
      if(!is.null(override[["r"]])){
        validate_numeric(override[["r"]], message = "Override r failed numeric validation: ")
        if(nrow(as.matrix(override[["r"]])) != n) stop("nrow(as.matrix(override[[\"r\"]])) must be equal to length(Y)") 
      }
      
      if(!is.null(override[["J"]]) && !is.null(override[["r"]]) &&
         !all(dim(as.matrix(override[["J"]])) == dim(as.matrix(override[["r"]]))))
        stop("as.matrix(override[[\"J\"]]) and as.matrix(override[[\"r\"]]) must have the same dimensions")
    }
    
    if(!is.null(override[["fitted_values"]])){
      validate_numeric(override[["fitted_values"]], len = n,
                       message = "override[[\"fitted_values\"]] failed numeric validation: ")
    }
    
    if(!is.null(override[["J"]])){
      validate_numeric(override[["J"]], message = "override[[\"J\"]] failed numeric validation: ")
      override[["J"]] <- tryCatch(as.matrix(override[["J"]]),
                                  error = function(e) stop("Error converting override[[\"J\"]] to a matrix: ", e))
      if(nrow(as.matrix(override[["J"]])) != n) stop("nrow(as.matrix(override[[\"J\"]])) must be equal to length(Y)") 
    }
    
    if(is.null(override[["theta_hat"]])){
      validate_numeric(x = theta_init, min_len = 1, message = "theta_init failed numeric validation: ")
    } else {
      # This is just so function output is verified at theta_hat below.
      theta_init <- override[["theta_hat"]]
    }
    if(is.null(names(theta_init))) names(theta_init) <- paste0("theta", seq_along(theta_init))
    
    if(!is.function(test_mean)) stop("test_mean must be a function: class(test_mean) is '",
                                     class(test_mean), "'")
    validate_test_mean_function_arg_names(f = test_mean)
    if(!("theta" %in% names(formals(test_mean))))
      stop("test_mean must contain an argument named \"theta\"")
    if(is.null(X)){
      if(any(c("x", "X") %in% names(formals(test_mean))))
        stop("X must be supplied if 'x' or 'X' is an argument of test_mean")
      if(length(theta_init) != 1) stop("theta_init must have length 1 when X is NULL")
    } else {
      if(!any(c("x", "X") %in% names(formals(test_mean))))
        stop("'x' or 'X' must be an argument of test_mean when X is not NULL")
    }
    f_out <- tryCatch(f2ftheta(f = test_mean, X = X[,, drop = FALSE], n = n)(theta_init),
                      error = function(e) stop("Unable to evaluate test_mean(X, theta_init): ", e))
    validate_numeric(x = f_out, len = n, message = "Test function output failed numeric validation: ")
    
    if(!is.null(override[["theta_hat"]])) theta_init <- NULL

    
    # Verify that X and Y dimensions match.
    if(!is.null(X) && nrow(X) != n)
      stop("The number of rows in X must be the length of Y: nrow(X) is ",
           nrow(X), ", but length(Y) is ", n)
    
    # Evaluate any function elements of covariance list, then validate.
    # covariance <- eval_cov(covariance_list = covariance, Y = Y, X = X)
    validate_covariance_list(covariance, n = n, allow_function = FALSE,
                             require_diagonal = FALSE, symmetric = symmetric)
    
    # Deal with optimization
    optimization_args <- control[["optimization_args"]]
    if(!is.null(optimization_args) && !is.list(optimization_args))
      stop("control list element \"optimization_args\" must be a list")
    if(is.null(control[["optimization_fun"]])){
      if(any(!is.null(control[["fun_to_optimize_arg"]]),
             !is.null(control[["theta_init_arg"]]),
             !is.null(control[["theta_hat_name"]])))
        warning("No alternative optimization function specified; using optim()",
                " by default, ignoring \"fun_to_optimize_arg\", \"theta_init_arg\", and \"theta_hat_name\"")
      optimization_fun <- optim
      optimization_args <- combine_lists(optimization_args, list(method = "BFGS"))
      theta_init_arg <- "par"
      theta_hat_name <- "par"
      fun_to_optimize_arg <- "fn"
    } else {
      optimization_fun <- control[["optimization_fun"]]
      theta_init_arg <- control[["theta_init_arg"]]
      theta_hat_name <- control[["theta_hat_name"]]
      fun_to_optimize_arg <- control[["fun_to_optimize_arg"]]
      if(!is.function(optimization_fun))
        stop("control list element \"optimization_fun\" must be a function")
    }
    if(is.character(theta_init_arg)){
      if(length(theta_init_arg) != 1)
        stop("control list element \"theta_init_arg\" must have length 1")
      if(!(theta_init_arg %in% names(formals(optimization_fun))))
        stop("control list element \"theta_init_arg\" must be the name of an argument of optimization_fun")
    } else {
      stop("control list element \"theta_init_arg\" must be a character string")
    }
    if(is.character(fun_to_optimize_arg)){
      if(length(fun_to_optimize_arg) != 1)
        stop("control list element \"fun_to_optimize_arg\" must have length 1")
      if(!(fun_to_optimize_arg %in% names(formals(optimization_fun))))
        stop("control list element \"fun_to_optimize_arg\" must be the name of an argument of optimization_fun")
    } else {
      stop("control list element \"fun_to_optimize_arg\" must be a character string")
    }
    if(!is.character(theta_hat_name)){
      stop("control list element \"theta_hat_name\" must be a character vector of length 1")
    } else {
      if(length(theta_hat_name) != 1) stop("control list element \"theta_hat_name\" must have length 1")
    }
    
    optimization_args[[theta_init_arg]] <- theta_init
    
    # Straight-forward verification of a few more arguments.
    jacobian_args <- control[["jacobian_args"]]
    if(!is.null(jacobian_args) && !is.list(jacobian_args))
      stop("jacobian_args must be a list")
    validate_numeric(x = B, pos_int = TRUE, len = 1)
    B <- as.integer(B)
    
    
    # Validate the observation ordering specification.
    if(is.null(override[["res_order"]])){
      if(is.list(ordering)){
        validate_order_columns(X, ordering)
      } else {
        if(length(ordering) > 1) stop("ordering method specification must have length one, ",
                                      "or be a list of column specifications")
        strict_match(ordering, c("asis", "optimal", "simplex", "natural"))
      }
    }
    
    # # Check stat functions for egregious errors.
    # v <- vector(mode = "list", length = length(stat))
    # for(i in seq_along(stat)){
    #   v[[i]] <- tryCatch(get(stat[i])(rnorm(100)),
    #                      error = function(e) stop("Unable to evaluate ",
    #                                               deparse1(substitute(stat[i])),
    #                                               "(rnorm(100)): ", e))
    # }
    # bad_ind <- which(sapply(v, function(x) isFALSE(is.numeric(x)) ||
    #                           isFALSE(length(x) == 1)))
    # if(length(bad_ind) > 0){
    #   stop("The following stat functions are invalid: ",
    #        paste(stat[bad_ind], collapse = ", "), ".\nEach failed either ",
    #        "because it did not return a (non-NA) numeric value or because its ",
    #        "output is not a single number.")
    # }
    return(list(Y = Y, X = X, n = n, covariance = covariance,
                matsqrt_tol = control[["matsqrt_tol"]], solve_tol = control[["solve_tol"]],
                qr_tol = control[["qr_tol"]], orth_tol = control[["orth_tol"]],
                trans_tol = control[["trans_tol"]],
                optimization_fun = optimization_fun, optimization_args = optimization_args,
                theta_hat_name = theta_hat_name, fun_to_optimize_arg = fun_to_optimize_arg,
                jacobian_args = jacobian_args, B = B))
  }
