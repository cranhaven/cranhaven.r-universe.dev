validate_test_mean_function_arg_names <- function(f){
  arg_names <- names(formals(f))
  if(setequal(arg_names, c("x", "theta"))){
    # if(isTRUE(verbose)) message("Interpreting 'x' in test_mean as a vector argument...")
  } else {
    if(setequal(arg_names, c("X", "theta"))){
      # if(isTRUE(verbose)) message("Interpreting 'X' in test_mean as a matrix argument...")
    } else {
      if(!setequal(arg_names, c("theta"))){
        stop("Invalid argument specification of test_mean")
      }
    } 
  }
}
