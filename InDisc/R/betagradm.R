betagradm <- function(THRES, ALPHA){

  nite<-size(ALPHA)[1]
  nfac<-size(ALPHA)[2]

  m <- size(THRES)[2]

  nume <- matrix(0,m,1)
  deno <- matrix(0,m,1)
  BETA <- matrix(0,nite,nfac)
  MDIF <- matrix(0,m,1)

  for (j in 1:m){
    tmp1 <- THRES[,j]
    tmp2 <- ALPHA[j,]
    nume[j] <- (tmp1[2] + tmp1[3])/2
    deno[j] <- sqrt(tmp2 %*% transpose(tmp2))
    MDIF[j] <- nume[j] / deno[j]
  }

  for (j in 1:nite){
    for (k in 1:nfac){
      BETA[j,k] <- (ALPHA[j,k] %*% MDIF[j]) / deno[j]
    }
  }

  OUT<-list("BETA" = BETA, "MDIF" = MDIF)
  return(OUT)
}
