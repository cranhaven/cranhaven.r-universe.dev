formula.distfreereg <-
  function(x, ...){
    stopifnot(is.list(x))
    if(is.null(x[["test_mean"]])){
      output <- NULL
    } else {
      output <- tryCatch(formula(x[["test_mean"]]), error = function(e) NULL)
    }
    return(output)
  }
