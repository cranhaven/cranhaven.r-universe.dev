geovolTest <- function(e)
{
  
  n <- nrow(e)
  m <- ncol(e)
  r <- (m*(m-1))/2
  stats <- matrix(NA, 1, 3)
  
  what <- rowSums(e^2, na.rm = TRUE)
  zhat <- rowSums(e, na.rm = TRUE)
  zhat2 <- zhat^2
  s1 <- mean(what) / m
  s2 <- mean(zhat2) / m
  test <- (s2 - s1) / (s1 * (m - 1))
  
  R = matrix(NA, m, m)
  for(i in 1:m){
    for(j in 1:m){
      if(i > j) R[i,j] = cor(na.omit(e[,c(i,j)]))[1,2]
    }
  }
  stats[1]  <- mean(R, na.rm = TRUE)
  stats[2]  <- test * sqrt(r * n)
  stats[3] <- pnorm(stats[2], lower.tail = FALSE)
      
  colnames(stats) <- c("Correlation", "Statistic", "p-value")
  rownames(stats) <- c("Value")
   
  return(round(stats, 4))
  
}



