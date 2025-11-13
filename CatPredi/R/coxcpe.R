coxcpe <- function(phfit, data) {
  xbeta <- na.omit(predict(phfit, data, type = "lp"))
  n <- length(xbeta)
  CPE <- 0
  for (i in c(1:(n-1))) {
    for (j in (i+1):n) {
      xbij <- xbeta[i] - xbeta[j]
      eij <- 1 + exp(xbij)
      eji <- 1 + exp(-xbij)
      if (xbij<= 0){
        CPE <-  CPE + 1/eij
      }else{
        CPE <-  CPE + 1/eji
      }
    }
  }
  CPE <- 2*CPE/(n*(n-1))
  as.numeric(CPE)
}