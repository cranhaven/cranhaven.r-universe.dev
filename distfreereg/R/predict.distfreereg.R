predict.distfreereg <-
  function(object, ..., newdata){
    stopifnot(!is.null(object[["test_mean"]]))
    test_mean <- object[["test_mean"]]
    if(missing(newdata)){
      output <- fitted(object)
    } else {
      if(is(test_mean, "lm") || is(test_mean, "nls") || is(test_mean, "formula")){
        if(is(test_mean, "formula")){
          m <- object[["model"]]
          stopifnot(!is.null(m))
        } else {
          m <- test_mean
        }
        output <- predict(m, ..., newdata = newdata)
      } else {
        if(is.function(test_mean)){
          stopifnot(!is.null(object[["theta_hat"]]))
          if(!is.numeric(newdata)) stop("'newdata' must be numeric when object$test_mean does not have a formula method")
          if(!is.matrix(newdata)){
            warning("Vector supplied as 'newdata' converted into single row of newdata matrix...")
            newdata <- t(as.matrix(newdata))
          }
          output <- f2ftheta(f = test_mean, X = newdata, n = ncol(newdata))(object[["theta_hat"]])
        }
      }
    }
    return(output)
  }
