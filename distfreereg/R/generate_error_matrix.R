generate_error_matrix <- function(err_dist_fun, err_dist_args, err_dist_fun_formals,
                                  true_covariance, cov_args, solve_tol, reps, n){
  # Define error-generating function and argument list, then generate errors.
  # err_dist_fun <- get(err_dist_fun)
  if(reps > 0){
    err_dist_args_default <-
      create_err_dist_args_default(err_dist_fun_formals = err_dist_fun_formals,
                                   cov_args = cov_args, reps = reps, n = n,
                                   true_covariance = true_covariance)
    
    message("Generating errors...")
    error_matrix <- tryCatch(do.call(err_dist_fun,
                                     combine_lists(err_dist_args, err_dist_args_default)),
                             error = function(e) stop("Unable to generate error matrix: ", e))
    
    # Check error matrix.
    if(!is.matrix(error_matrix)) stop("class of error_matrix is '", class(error_matrix),
                                      "'; should be 'matrix'")
    validate_numeric(error_matrix, message = "Error matrix failed numeric validation: ")
    if(any(dim(error_matrix) != c(n, reps))) stop("dimension of error_matrix is ",
                                                  paste(dim(error_matrix), collapse = ", "),
                                                  "; should be ", n, ", ", reps)
  } else {
    error_matrix <- NULL
  }
  
  return(error_matrix)
}

