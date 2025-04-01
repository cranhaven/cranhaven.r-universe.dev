generbeta <- function(ALPHA, LAM, mu){

  nite<-size(LAM)[1]
  nfac<-size(LAM)[2]

  BETA <- matrix(0,nite,nfac)
  MDIF <- matrix(0,nite,1)
  TMP <- LAM * LAM
  deno <- TMP %*% matrix(1,nfac,1)

  for (j in 1:nite){
    for (k in 1:nfac){
      BETA[j,k] <- (-1) %*% (LAM[j,k] %*% mu[j])/deno[j]
    }
  }

  TMP1 <- t(LAM * LAM)
  TMP2 <- t(ALPHA * ALPHA)
  MDISC <- transpose(colSums(TMP2))
  MDISC <- sqrt(MDISC)
  denovec <- transpose(colSums(TMP1))
  denovec <- sqrt(denovec)

  for (j in 1:nite){
    MDIF[j] <- (-1) %*% (mu[j]/denovec[j])
  }


  OUT<-list("BETA" = BETA, "MDIF" = MDIF, "MDISC" = MDISC)
  return(OUT)
}
