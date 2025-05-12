
isDataQuantile <- function(call, env){

  re <- try(
    call_class <-
      call %>%
      eval(envir = env) %>%
      class(),
    silent = TRUE
  )

  # possible reasons when catching an error from try():
  # 1. function quantile is specified. We allow non-existing function name,
  #    like norm for flexibility as a result, eval() will fail.
  #    In this case, isNormalQuantile will be called, so return with FALSE.
  #    Note that if users specify a valid function as a call which returns a
  #    data frame, no error will be thrown, TRUE should be returned if other
  #    checks below are also passed
  # 2. other issue/bugs we need to fix in the future. Return FALSE as well.
  #    Now we expect other error will be prompted elsewhere
  if('try-error' %in% class(re)){
    return(FALSE)
  }

  if('function' %in% call_class){
    ## a function is provided by user
    ## we assume that is a valid quantile
    ## we have no way to check if a function is a quantile or not
    return(FALSE) # not a data frame
  }

  ## once we arrive here, we assume that call is a data frame,
  ## or it is an expression with its value a data frame

  # is.symbol can return TRUE for x, df, etc.
  # is.symbol can return FALSE for df[, 1:3]
  # both can be valid data frame

  if('data.frame' %in% call_class){
    return(TRUE)
  }

  if(is.symbol(call)){
    ## a symbol (e.g. x, df, etc.) must be a data frame
    ## we stop executing the program and ask user to fix this input
    stop(str_glue("{call} should be a data frame if quantile function is created with data"))
  }

  return(FALSE)
}
