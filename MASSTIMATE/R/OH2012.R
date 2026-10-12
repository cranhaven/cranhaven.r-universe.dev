OH2012 <- function(FL, eqn, data=NULL) {
  if(eqn == "theropods") {
    log.OH2012 <- 3.1854 * log10(FL) - 3.184
    OH2012 <- round((10^log.OH2012), 2)
    return(cbind(data, log.OH2012, OH2012))
  }
  if(eqn == "sauropods") {
    log.OH2012 <- 2.3459 * log10(FL) - 0.2935
    OH2012 <- round((10^log.OH2012), 2)
    return(cbind(data, log.OH2012, OH2012))
  }
  if(eqn == "ornithischians") {
    log.OH2012 <- 3.0587 * log10(FL) - 2.7042
    OH2012 <- round((10^log.OH2012), 2)
    return(cbind(data, log.OH2012, OH2012))
  }
}