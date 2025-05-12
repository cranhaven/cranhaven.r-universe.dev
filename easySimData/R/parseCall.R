
parseCall <- function(call, env){

  quantile_function <- parseCallIntoQuantileFunction(call, env)
  if(!is.null(quantile_function)){
    return(quantile_function)
  }

  invisible(NULL)

}

