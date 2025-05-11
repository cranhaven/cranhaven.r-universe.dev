residuals.distfreereg <-
  function(object, ..., type = "raw"){
    strict_match(arg = type, choices = c("raw", "sphered", "transformed"))
    stopifnot(!is.null(object[["residuals"]][[type]]))
    return(object[["residuals"]][[type]])
  }
