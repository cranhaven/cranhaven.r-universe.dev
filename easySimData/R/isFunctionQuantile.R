
## user specifies a quantile function, like the usage of simdata
isFunctionQuantile <- function(call, env){

  re <- try(
    call_class <-
      call %>%
      eval(envir = env) %>%
      class(),
    silent = TRUE
  )

  ## see possible errors in comments of isDataQuantile()
  if('try-error' %in% class(re)){
    return(FALSE)
  }

  if(!('function' %in% call_class)){
    return(FALSE)
  }

  func <- eval(call, envir = env)
  par_list <- formals(func)
  if(!('x' %in% names(par_list))){
    stop('x must be the first argument in user-defined quantile functions.')
  }

  return(TRUE)

}

