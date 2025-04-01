thresholds <- function(X,Xmin,Xmax){

  N<-size(X)[1]
  m<-size(X)[2]

  Ki <- (Xmax - Xmin + 1)
  Kmax <- max(Ki)

  THRES <- matrix(0,m,(max(Ki)-1))

  FR <- matrix(0,m,Kmax)

  for (i in 1:m){
    Xmi <- min(X[,i])
    for (k in 1:N){
      jth<-1
      for (j in 1:Kmax){
        if (X[k,i] == (Xmi-1+j)){
          jth <- j
        }
      }
      FR[i,jth] <- FR[i,jth] + 1
    }
    FR[i,] <- FR[i,]/N

    for (k in 1:Kmax-1){
      tmp1 <- sum(t(FR[i,1:k]))
      THRES[i,k] <- stats::qnorm(tmp1)
    }
  }

  return(THRES)

}
