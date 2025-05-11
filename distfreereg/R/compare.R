compare <-
  function(true_mean,
           true_method = NULL,
           true_method_args = NULL,
           true_covariance,
           true_X = NULL,
           true_data = NULL,
           theta,
           n = NULL,
           reps = 1e3,
           prog = reps/10,
           err_dist_fun = rmvnorm,
           err_dist_args = NULL,
           keep = NULL,
           manual = NULL,
           update_args = NULL,
           global_override = NULL,
           ...){
    cl <- match.call()
    arg_list <- list(...)
    
    vargs <- validate_args_compare(true_mean = true_mean,
                                   true_method = true_method,
                                   true_method_args = true_method_args,
                                   true_covariance = true_covariance,
                                   true_X = true_X,
                                   true_data = true_data,
                                   theta = theta,
                                   n = n,
                                   reps = reps,
                                   prog = prog,
                                   err_dist_fun = err_dist_fun,
                                   err_dist_args = err_dist_args,
                                   keep = keep,
                                   manual = manual,
                                   update_args = update_args,
                                   arg_list = arg_list)
    
    test_mean <- vargs[["test_mean"]]
    true_X <- vargs[["true_X"]];  X <- vargs[["X"]];
    test_Y <- vargs[["test_Y"]]; data <- vargs[["data"]];
    n <- vargs[["n"]]; reps <- vargs[["reps"]]; prog <- vargs[["prog"]]
    keep <- vargs[["keep"]]; arg_list <- vargs[["arg_list"]]
    matsqrt_tol <- vargs[["matsqrt_tol"]]; solve_tol <- vargs[["solve_tol"]]
    covariance <- vargs[["covariance"]]
    
    Y_mean <- generate_Y_mean(true_mean = true_mean, true_X = true_X,
                              true_data = true_data, n = n, theta = theta,
                              true_method = true_method,
                              true_method_args = true_method_args)
    
    message("Calculating required true covariance specification matrices...")
    err_dist_fun_formals <- names(formals(err_dist_fun))
    possible_cov_args <- c("Sigma", "SqrtSigma", "P", "Q")
    cov_args <- possible_cov_args[which(possible_cov_args %in% err_dist_fun_formals)]
    true_covariance <- fill_covariance_list(need = intersect(cov_args, possible_cov_args),
                                            covariance_list = true_covariance,
                                            matsqrt_tol = matsqrt_tol,
                                            solve_tol = solve_tol)
    
    error_matrix <- generate_error_matrix(err_dist_fun = err_dist_fun,
                                          err_dist_args = err_dist_args,
                                          err_dist_fun_formals = err_dist_fun_formals,
                                          true_covariance = true_covariance,
                                          cov_args = cov_args,
                                          solve_tol = solve_tol,
                                          reps = reps, n = n)
    
    if(!is.null(manual)) manual_output <- vector(mode = "list", length = reps)
    if(identical(keep, "all")) keep <- 1:reps
    if(!is.null(keep)) dfrs <- vector("list", length = 0L)
    
    mcsim_stats <- obs_stats <- p_values <- NULL
    
    message("Running simulation...")
    for(i in seq_len(reps)){
      if(i %% prog == 0) message("Repetition ", i, " of ", reps)
      
      dfr_args <- define_dfr_args(Y_mean = Y_mean, error_matrix = error_matrix,
                                  test_mean = test_mean, data = data,
                                  X = X, test_Y = test_Y, i = i,
                                  global_override = global_override)
      if(i == 1){
        dfr_args <- combine_dfr_arg_lists(dfr_args, arg_list,
                                          list(test_mean = test_mean,
                                               verbose = FALSE))
        dfr <- do.call(distfreereg, args = dfr_args)
        output_covariance <- dfr[["covariance"]]
        mcsim_stats <- dfr[["mcsim_stats"]]
        
        # On first repetition, create some output data frames with the correct
        # dimensions. Doing it here guarantees that the data frames have the
        # correct dimensions and, in particular, the correct column names when no
        # value of "stat" is specified in "...".
        obs_stats <- p_values <- as.data.frame(matrix(NA, nrow = reps,
                                                      ncol = length(dfr[["observed_stats"]])))
        colnames(obs_stats) <- colnames(p_values) <- names(dfr[["observed_stats"]])
      } else {
        # Include res_order, r, and mcsim_stats in override list to prevent
        # updating those because "X" or "data" is always in dfr_args.
        dfr_args <- combine_lists(dfr_args, list(object = dfr), update_args,
                                  list(override = list(res_order = dfr[["res_order"]],
                                                       r = dfr[["r"]],
                                                       mcsim_stats = dfr[["mcsim_stats"]])))
        dfr <- do.call(update.distfreereg, args = dfr_args)
      }
      obs_stats[i,] <- unlist(dfr[["observed_stats"]])
      p_values[i,] <- unlist(dfr[["p"]][["value"]])
      if(isTRUE(i %in% keep)) dfrs[[paste0("dfr_", i)]] <- dfr
      if(!is.null(manual)) manual_output[[i]] <- manual(dfr)
    }
    
    
    output <- list(call = cl,
                   Y_mean = Y_mean,
                   errors = error_matrix,
                   theta = theta,
                   true_mean = true_mean,
                   true_covariance = true_covariance,
                   true_X = true_X,
                   true_data = true_data,
                   test_mean = test_mean,
                   covariance = output_covariance,
                   X = X,
                   data = data,
                   observed_stats = obs_stats,
                   mcsim_stats = mcsim_stats,
                   p = p_values)
    
    if(!is.null(keep)) output[["dfrs"]] <- dfrs
    if(!is.null(manual)) output[["manual"]] <- manual_output
    class(output) <- "compare"
    return(output)
  }
