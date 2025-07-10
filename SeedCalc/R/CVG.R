# Coefficient of velocity of germination

CVG <- function(time, nger) {

  n <- nger[1]
  for (i in 2:length(nger)) {
    n <- c(n, nger[i]-nger[i-1])
  }

  num <- sum(n)
  den <- c()

  for (i in 1:length(n)) {
    den <- c(den,n[i]*time[i])
  }

  result <- num/sum(den)*100

  return(result)
}
