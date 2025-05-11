validate_covariance_list <- function(covariance_list, n, allow_function,
                                     require_diagonal, symmetric){
  stopifnot(isFALSE(allow_function))
  list_name <- deparse1(substitute(covariance_list))
  validate_named_list(covariance_list, valid_names = c("Sigma", "SqrtSigma", "P", "Q"))
  if(length(covariance_list) == 0) stop(list_name, " must have at least one element")
  
  # If functions are allowed, convert to NULL. Note that SqrtSigma should NOT be
  # allowed to be a function.
  if(isTRUE(allow_function)){
    covariance_list <-
      lapply(covariance_list, function(x) if(is.function(x) &&
                                             !identical(names(x), "SqrtSigma")) NULL)
  }
  
  cov_val <- function(x){
    if(!is.null(covariance_list[[x]])){
      if(!is.numeric(covariance_list[[x]]))
        stop("All specified elements of ", list_name, " element ", x, " must be numeric")
      if(is.matrix(covariance_list[[x]])){
        validate_sqmat(covariance_list[[x]], n, symmetric,
                       message = c("Error in validating ", list_name, " element ", x, " (length>1): "))
        if(isTRUE(require_diagonal)) validate_diag_mat(covariance_list[[x]])
      } else {
        if(length(covariance_list[[x]]) != 1 && length(covariance_list[[x]]) != n){
          stop("Invalid covariance vector length")
        } else {
          if(any(covariance_list[[x]] <= 0)) stop("Elements of a vector specification for covariance must be positive")
        }
      }
    } 
  }
  lapply(names(covariance_list), cov_val)
}