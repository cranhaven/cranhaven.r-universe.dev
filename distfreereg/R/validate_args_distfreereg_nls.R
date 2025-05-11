validate_args_distfreereg_nls <-
  function(test_mean, n, control, extra_arg_list, verbose){
    validate_extra_arg_list(extra_arg_list, "distfreereg.nls()")
    if(!is(test_mean, "nls")) stop("test_mean must be an object of class \"nls\"")
    validate_single_response_term(form = test_mean)
    if(!is.null(control) && !is.list(control)) stop("'control' must be NULL or a list")
    if(!is.null(control))
      validate_named_list(control, valid_names = c("matsqrt_tol", "solve_tol",
                                                   "qr_tol", "orth_tol", "trans_tol",
                                                   "symmetric", "data"))
    control <- combine_lists(control, default_distfreereg_tol())
    validate_control_tols(control)
    
    if(!is.null(test_mean[["weights"]]))
      validate_numeric(x = test_mean[["weights"]], positive = TRUE,
                       message = "nls object weights failed validation: ")
    return(control)
  }
