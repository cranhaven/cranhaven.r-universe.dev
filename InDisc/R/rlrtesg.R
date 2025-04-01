rlrtesg <- function(DIF,RES,alpha,varc,vres,th0,th1,vari){

  n <- size(RES)[1]
  LR <- matrix(0,n,2)
  sum <- 0

  for (i in 1:n){
    OUT <- lrtest(DIF,RES[i,], alpha, varc, vres, th0[i], th1[i], vari[i])
    LR[i,1] <- as.numeric(OUT$LR)
    LR[i,2] <- as.numeric(OUT$chi)
    sum <- sum + LR[i,2]
  }

  return(LR)

}
