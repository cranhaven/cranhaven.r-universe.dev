fitted.distfreereg <-
  function(object, ...){
    stopifnot(!is.null(object[["fitted_values"]]))
    return(object[["fitted_values"]])
  }
