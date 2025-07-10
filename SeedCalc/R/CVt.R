# Coeficiente de variacao


CVt <- function(time, nger) {

  #gerar dados nao acumulados
  n <- nger[1]
  for (i in 2:length(nger)) {
    n <- c(n, nger[i] - nger[i-1])
  }

  tm <- MGT(time,nger)

  result <- sqrt(VarGer(time,nger))/tm*100

  return(result)

}


