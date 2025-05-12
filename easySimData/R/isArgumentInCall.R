
# return TRUE if all desired arguments are found in call and no redundant arguments
isArgumentInCall <- function(args, call){

  call_name <- sub('\\s+', '', sub('^(.*?)\\(.*$', '\\1', deparse(call)))
  arg_is_found <- sapply(args, function(arg) arg %in% names(call))
  if(!all(arg_is_found)){
    msg <- str_glue("The following argument(s) are missing from {call_name}(): \n{paste0(args[!arg_is_found], collapse = ', ')}")
    stop(msg)
  }

  redundant_args <- setdiff(names(call), c(args, ''))
  if(length(redundant_args) > 0){
    msg <- str_glue("Redundant arguments in {call_name}(): \n{paste0(redundant_args, collapse = ', ')}")
    stop(msg)
  }

  return(TRUE)

}
