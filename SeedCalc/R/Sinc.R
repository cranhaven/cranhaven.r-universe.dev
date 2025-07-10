# sincrony - Z

Sinc <- function(time, nger) {

  n <- nger[1]
  for (i in 2:length(nger)) {
   n <- c(n, nger[i]-nger[i-1])
  }

  Ci2 <- 0
  for (i in 1:length(n)) {
  Ci2 <- Ci2 + choose(n[[i]],2)
  }

  N <- choose(sum(n),2)

  result <- Ci2/N

 return(result)
}

