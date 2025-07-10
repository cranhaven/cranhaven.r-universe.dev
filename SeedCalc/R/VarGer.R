# Germination variance

VarGer <- function(time, nger) {

  #gerar dados nao acumulados
  n <- nger[1]
  for (i in 2:length(nger)) {
   n <- c(n, nger[i] - nger[i-1])
  }

  
  tm <- MGT(time,nger)


  num <- c()
  for (i in 1:length(n)) {
    num <- c(num, n[i]*(time[i] - mean(tm))^2)
  }

 result <- sum(num)/(sum(n) - 1)

 return(result)
}
