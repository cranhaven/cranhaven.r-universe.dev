update.distfreereg <-
  function(object, ..., smart = TRUE, envir = parent.frame()){
    if (is.null(call <- getCall(object))) 
      stop("Need an object with call component")
    func_name <- as.list(call)[1]
    old_arg_list <- as.list(call)[-1]
    new_arg_list <- list(...)
    override_list <- new_arg_list[["override"]]
    # old_arg_list[["override"]] <- new_arg_list[["override"]] <- NULL
    
    # If smart is TRUE, then use existing entries in object as override values.
    if(isTRUE(smart)){
      
      # First prepare for dealing with "override".
      
      n_old <- nrow(object[["J"]])
      p_old <- ncol(object[["J"]])
      
      n_new <- as.numeric(sapply(new_arg_list[c("Y", "X", "data", "J")], NROW))
      n_new <- setdiff(unique(n_new), 0)
      p_new <- as.numeric(c(sapply(new_arg_list[c("J")], NCOL),
                            sapply(new_arg_list[c("theta_init")], length)))
      p_new <- setdiff(unique(p_new), 0)
      
      if(length(n_new) == 0) n_new <- NULL
      if(length(p_new) == 0) p_new <- NULL
      if(length(n_new) > 1) stop("Sample size mismatch in arguments to update()")
      if(length(p_new) > 1) stop("Parameter space dimension mismatch in arguments to update()")
      
      # Create a list of user inputs that should trigger recomputation of
      # values. For example, if user calls "update(object, X = X1)", then
      # object[["res_order"]] should not be used as an override. If none of X,
      # data, and ordering are specified in the call to update(), then it
      # should. The argument "verbose" is omitted from consideration here. Note
      # that the handling of "data" is not optimal, since "data" might change
      # without changing covariate values, which would not require updating
      # res_order.
      var_list <- list(
        res_order   = c("data", "ordering", "X"),
        theta_hat   = c("covariance", "data", "test_mean", "theta_init", "X", "Y"),
        r           = c("data", "ordering", "test_mean", "X"),
        mcsim_stats = c("B", "group", "ordering", "stat")
      )
      # Since some values are hidden within the control list, there is a
      # separate list analogous to var_list that contains the names of the
      # control list elements that should trigger recomputation.
      control_list <- list(
        theta_hat = c("symmetric", "solve_tol", "optimization_fun", "optimization_args"),
        r = c("qr_tol", "orth_tol")
      )
      # Next, since some values are hidden within the override list, there is a
      # separate list analogous to control_list. Note that it is not named
      # "override_list", since that name is used elsewhere.
      or_list <- list(
        r = c("res_order"),
        mcsim_stats = c("res_order", "r")
      )
      
      # Finally, define a list of custom tests for updating each variable. This
      # is important, for example, to avoid updating r when Y changes but
      # length(Y) does not. Each expression should evaluate to a single logical
      # value, and should be TRUE when var needs to be recalculated (that is,
      # not included in the override argument).
      custom_list <- list(
        res_order =
          isTRUE(!is.null(n_new) && n_new != n_old) ||
          isTRUE(is(new_arg_list[["test_mean"]], "lm") || is(new_arg_list[["test_mean"]], "nls")),
        theta_hat = 
          isTRUE(is(object[["test_mean"]], "formula") || is(object[["test_mean"]], "lm") ||
                   is(object[["test_mean"]], "nls")),
        r =
          isTRUE(!is.null(n_new) && n_new != n_old) ||
          isTRUE(!is.null(p_new) && p_new != p_old),
        mcsim_stats =
          isTRUE(!is.null(n_new) && n_new != n_old) ||
          isTRUE(!is.null(p_new) && p_new != p_old) ||
          isTRUE(class(object[["test_mean"]]) %in% c("lm", "formula") &&
                   !is.null(new_arg_list[["test_mean"]])) ||
          isTRUE(isTRUE(old_arg_list[["group"]]) && !is.null(new_arg_list[["ordering"]])) ||
          isTRUE(isTRUE(old_arg_list[["group"]]) && !is.null(new_arg_list[["X"]])) ||
          isTRUE(isTRUE(old_arg_list[["group"]]) && !is.null(new_arg_list[["data"]]))
      )
      
      # Now, for each variable in any of the lists above, set override_list[[var]]
      # to appropriate value from object unless a new argument should prevent it.
      for(var in unique(c(names(var_list), names(control_list), names(or_list), names(custom_list)))){
        if(is.null(override_list[[var]]) &&
           !isTRUE(any(any(var_list[[var]] %in% names(new_arg_list)),
                       any(control_list[[var]] %in% names(new_arg_list[["control"]])),
                       any(or_list[[var]] %in% names(new_arg_list[["override"]])),
                       custom_list[[var]]))){
          override_list[[var]] <- object[[var]]
        }
      }
      
      # Second, deal with covariance, which is not part of the "override"
      # paradigm.
      
      if(is.null(new_arg_list[["covariance"]]) &&
         (is(object[["test_mean"]], "function") || is.null(object[["test_mean"]]))){
        new_arg_list[["covariance"]] <- object[["covariance"]]
      }
    }
    
    updated_args <- combine_lists(new_arg_list, old_arg_list)
    updated_args[["override"]] <- override_list
    updated_call <- as.call(c(func_name, updated_args))
    updated_object <- eval(updated_call, envir = envir)
    return(updated_object)
  }
