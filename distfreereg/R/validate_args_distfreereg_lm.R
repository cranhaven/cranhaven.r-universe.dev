validate_args_distfreereg_lm <-
  function(test_mean, extra_arg_list){
    validate_extra_arg_list(extra_arg_list, "distfreereg.lm()")
    if(!is(test_mean, "lm")) stop("test_mean must be an object of class \"lm\"")
    validate_single_response_term(form = test_mean)
    if(!is.null(test_mean[["weights"]]))
      validate_numeric(x = test_mean[["weights"]], positive = TRUE,
                       message = "Linear model weights failed validation: ")
  }
