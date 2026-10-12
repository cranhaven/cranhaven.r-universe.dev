MCF2004 <- function(X, eqn, data = NULL) {
  if(!is.numeric(X)) stop("At least one number or numeric vector must be provided to estimate body mass")
  if(eqn == "FL") {
    log.MCF2004 <- 3.195 * log10(X) - 5.983
    MCF2004 <- round((10^log.MCF2004) * 1000, 2)
    return(cbind(data, log.MCF2004, MCF2004))
  }
  if(eqn == "FL*") {
    log.MCF2004 <- 2.838 * log10(X) - 4.864
    MCF2004 <- round((10^log.MCF2004) * 1000, 2)
    return(cbind(data, log.MCF2004, MCF2004))
  }
  if(eqn == "FC") {
    log.MCF2004 <- 2.955 * log10(X) - 4.166
    MCF2004 <- round((10^log.MCF2004) * 1000, 2)
    return(cbind(data, log.MCF2004, MCF2004))
  }
  if(eqn == "TL") {
    log.MCF2004 <- 3.876 * log10(X) - 7.342
    MCF2004 <- round((10^log.MCF2004) * 1000, 2)
    return(cbind(data, log.MCF2004, MCF2004))
  }
  if(eqn == "TC") {
    log.MCF2004 <- 3.288 * log10(X) - 4.507
    MCF2004 <- round((10^log.MCF2004) * 1000, 2)
    return(cbind(data, log.MCF2004, MCF2004))
  }
}