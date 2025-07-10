# Uncertainty

Unc <- function(time, nger) {
  
  n <- nger[1]
  for (i in 2:length(nger)) {
    n <- c(n, nger[i]-nger[i-1])
  }
  
  E <- c()
  f <- c()
  
  for (i in 1:length(n)) {
    f <- c(f,n[i]/sum(n))  
    
    if (f[i] > 0) {
      E <- c(E, f[i]*log2(f[i]))  
    }
    
  }
  
  result <- -sum(E)
  
  return(result)
}
