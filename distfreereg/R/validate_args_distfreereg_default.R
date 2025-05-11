validate_args_distfreereg_default <-
  function(Y, X, covariance, ordering, J, fitted_values, stat, B,
           override, verbose, control, extra_arg_list){
    
    validate_extra_arg_list(extra_arg_list, "distfreereg.default()")
    
    if(!is.null(control))
      validate_named_list(control, valid_names = c("matsqrt_tol", "solve_tol",
                                                   "qr_tol", "orth_tol",
                                                   "trans_tol", "symmetric",
                                                   "data"))

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
      validate_named_list(override, valid_names = c("res_order", "r", "mcsim_stats"))
      if(!is.null(override[["dfr"]]) && !is(override[["dfr"]], "distfreereg"))
        stop("override[[\"dfr\"]] must be an object of class 'distfreereg'")
      
      if(!is.null(override[["res_order"]])){
        if(!isPermutation(override[["res_order"]])) stop("override[[\"res_order\"]] must be a permutation")
        if(length(override[["res_order"]]) != n) stop("override[[\"res_order\"]] and Y must have the same length")
      }
      
      if(!is.null(override[["r"]])){
        validate_numeric(override[["r"]], message = "Override r failed numeric validation: ")
        if(nrow(as.matrix(override[["r"]])) != n) stop("nrow(as.matrix(override[[\"r\"]])) must be equal to length(Y)") 
      }
      
      if(!is.null(J) && !is.null(override[["r"]]) &&
         !all(dim(as.matrix(J)) == dim(as.matrix(override[["r"]]))))
        stop("as.matrix(override[[\"J\"]]) and as.matrix(override[[\"r\"]]) must have the same dimensions")
    }
    
    validate_numeric(J, message = "J failed numeric validation: ")
    J <- as.matrix(J)
    if(nrow(as.matrix(J)) != n) stop("nrow(as.matrix(J)) must be equal to length(Y)") 

    validate_numeric(fitted_values, len = n,
                     message = "Fitted values failed numeric validation: ")
    

    # Verify that X and Y dimensions match.
    if(!is.null(X) && nrow(X) != n)
      stop("The number of rows in X must be the length of Y: nrow(X) is ",
           nrow(X), ", but length(Y) is ", n)
    
    # Verify covariance list.
    # covariance <- eval_cov(covariance_list = covariance, Y = Y, X = X)
    validate_covariance_list(covariance, n = n, allow_function = FALSE,
                             require_diagonal = FALSE, symmetric = symmetric)
    

    # Validate B
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
    
    # Check stat functions for egregious errors.
    v <- vector(mode = "list", length = length(stat))
    for(i in seq_along(stat)){
      v[[i]] <- tryCatch(get(stat[i])(rnorm(100)),
                         error = function(e) stop("Unable to evaluate ",
                                                  deparse1(substitute(stat[i])),
                                                  "(rnorm(100)): ", e))
    }
    bad_ind <- which(sapply(v, function(x) isFALSE(is.numeric(x)) ||
                              isFALSE(length(x) == 1)))
    if(length(bad_ind) > 0){
      stop("The following stat functions are invalid: ",
           paste(stat[bad_ind], collapse = ", "), ".\nEach failed either ",
           "because it did not return a (non-NA) numeric value or because its ",
           "output is not a single number.")
    }
    return(list(Y = Y, X = X, n = n, J = J, covariance = covariance,
                matsqrt_tol = control[["matsqrt_tol"]], solve_tol = control[["solve_tol"]],
                qr_tol = control[["qr_tol"]], orth_tol = control[["orth_tol"]],
                trans_tol = control[["trans_tol"]], B = B))
  }
