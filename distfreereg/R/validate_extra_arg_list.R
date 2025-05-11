validate_extra_arg_list <- function(extra_arg_list, fun_name){
  if(length(extra_arg_list) > 0){
    if(is.null(names(extra_arg_list))){
      stop("Unused unnamed arguments passed to ", fun_name, ": ",
           extra_arg_list)
    } else {
      stop("Unused arguments passed to ", fun_name, " with name(s) ",
           paste(names(extra_arg_list), collapse = ", "))
    }
  }
}
