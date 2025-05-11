define_dfr_args <- function(Y_mean, error_matrix, test_mean, data, X,
                            test_Y, i, global_override){
  Y <- Y_mean + error_matrix[,i]
  dfr_args <- list()
  if(is(test_mean, "lm")){
    # test_Y has been defined in validate_args_compare().
    new_data <- tryCatch(eval(getCall(test_mean)$data, envir = environment(formula(test_mean))),
                         error = function(e) stop("Unable to get lm object's data: ", e))
    new_data[,test_Y] <- Y
    dfr_args[["test_mean"]] <- update(test_mean, data = new_data, x = TRUE, y = TRUE)
  } else {
    if(is(test_mean, "nls")){
      new_data <- get_nls_data(test_mean)
      new_data[,test_Y] <- Y
      dfr_args[["test_mean"]] <- update(test_mean, data = new_data)
    } else {
      if(is.null(data)){
        dfr_args[["X"]] <- X
        dfr_args[["Y"]] <- Y
      } else {
        dfr_args[["data"]] <- data
        dfr_args[["data"]][,test_Y] <- Y
      }
    }
  }
  
  if(!is.null(global_override)) dfr_args[["override"]] <- global_override

  return(dfr_args)
}
