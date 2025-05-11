coef.distfreereg <-
  function(object, ...){
    stopifnot(is.list(object))
    return(object[["theta_hat"]])
  }
