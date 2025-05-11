distfreereg.formula <-
  function(test_mean,
           ordering = "simplex",
           group = FALSE,
           stat = c("KS", "CvM"),
           B = 1e4,
           control = NULL,
           override = NULL,
           verbose = TRUE,
           ...,
           data,
           covariance = NULL,
           method = "lm",
           theta_init = NULL){
    cl <- match.call()
    
    # To make sure the formula sent to lm is in this environment.
    environment(test_mean) <- environment()
    
    extra_arg_list <- list(...)
    
    vargs <- validate_args_distfreereg_formula(test_mean = test_mean,
                                               data = data,
                                               covariance = covariance, 
                                               method = method,
                                               theta_init = theta_init,
                                               override = override,
                                               control = control,
                                               verbose = verbose,
                                               extra_arg_list = extra_arg_list)
    # If covariance is supplied, then vargs[["control"]] contains a "weight"
    # element in the "method_args" element.
    control <- vargs[["control"]]
    
    n <- nrow(data)
    
    default_method_args <- list(formula = test_mean, data = data,
                                na.action = na.fail)
    
    # If theta_init supplied, send that, too, to nls().
    if(!is.null(theta_init)){
      if(identical(method, "lm")){
        stop("theta_init must be NULL when method is 'lm'")
      } else {
        default_method_args[["start"]] <- theta_init
      }
    }

    if(identical(method, "lm")){
      control[["method_args"]][["x"]] <- TRUE
      control[["method_args"]][["y"]] <- TRUE
    }
    
    m <- do.call(get(method), args = combine_lists(control[["method_args"]],
                                                   default_method_args))

    # Prevent passing method_args to following methods.
    control[["method_args"]] <- NULL
    
    output <- distfreereg(test_mean = m,
                          ordering = ordering,
                          group = group,
                          stat = stat,
                          B = B,
                          control = control,
                          override = override,
                          verbose = verbose)
    
    output[["test_mean"]] <- test_mean
    output[["model"]] <- m
    output[["call"]] <- cl
    
    return(output)
  }
