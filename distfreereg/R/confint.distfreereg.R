confint.distfreereg <-
  function(object, parm, level = 0.95, ..., digits = 3)
  {
    stopifnot(is(object, "distfreereg"))
    validate_numeric(x = digits, len = 1, pos_int = TRUE)
    digits <- as.integer(digits)
    validate_numeric(x = level, max_val_strict = 1, min_val_strict = 0)
    cf <- coef(object)
    pnames <- names(cf)
    if(missing(parm)){
      parm <- pnames
    } else {
      if(is.numeric(parm)){
        parm <- pnames[parm]
      }
    }
    vc <- vcov(object)
    ses <- sqrt(diag(vc))[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = digits), "%")
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
    ci[] <- cf[parm] + ses %o% fac
    
    if(is.function(object[["test_mean"]])){
      return(list(ci = ci, vcov = vc))
    } else {
      return(ci)
    }
  }
