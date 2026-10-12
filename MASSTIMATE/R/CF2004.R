CF2004 <- function(X, X2 = NULL, eqn, data = NULL) {
  if(!is.numeric(X)) stop("At least one number or numeric vector must be provided to estimate body mass")
  if(is.null(X2) & grepl("+", eqn, fixed = TRUE)) stop("Two variables need to be identified to use a multple regression equation")
  if(eqn == "FL") {
    log.CF2004 <- 3.222 * log10(X) - 6.288
    CF2004 <- round((10^(log.CF2004)) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "FC") {
    log.CF2004 <- 2.738 * log10(X) - 3.607
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "FAP") {
    log.CF2004 <- 2.641 * log10(X) - 2.028
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "FML") {
    log.CF2004 <- 2.377 * log10(X) - 2.284
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "TC") {
    log.CF2004 <- 2.611 * log10(X) - 3.135
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "TdistML") {
    log.CF2004 <- 2.337 * log10(X) - 2.099
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "FidistAP") {
    log.CF2004 <- 2.787 * log10(X) - 2.905
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "FC+FL") {
    log.CF2004 <- 1.030 * log10(X) + 2.012 * log10(X2) - 5.285 #X=FC, X2=FL
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "FML+FL") {
    log.CF2004 <- 0.431 * log10(X) + 2.714 * log10(X2) - 5.656 #X=FML, X2=FL
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }  
  if(eqn == "TC+FL") {
    log.CF2004 <- 0.868 * log10(X) + 2.159 * log10(X2) - 5.263 #X=TC, X2=FL
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }  
  if(eqn == "TL+FC") {
    log.CF2004 <- 0.715 * log10(X) + 2.179 * log10(X2) - 4.324 #X=TL, X2=FC
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
  if(eqn == "TC+FAP") {
    log.CF2004 <- 1.461 * log10(X) + 1.158 * log10(X2) - 2.638 #X=TC, X2=FAD
    CF2004 <- round((10^log.CF2004) * 1000, 2)
    return(cbind(data, log.CF2004, CF2004))
  }
}