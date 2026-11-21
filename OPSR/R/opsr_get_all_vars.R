opsr_get_all_vars <- function(object, ...) {
  all_vars <- all.vars(object$formula)
  original_call <- object$call
  data_arg_passed <- "data" %in% names(original_call)
  if (data_arg_passed) {
    data_arg_name <- original_call$data
    data <- get(data_arg_name, envir = environment(object$formula))
    data <- data[all_vars]
  } else {
    data_list <- lapply(all_vars, function(x) get(x, envir = environment(object$formula)))
    data <- as.data.frame(data_list)
    colnames(data) <- all_vars
  }
  data
}
