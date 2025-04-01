eapth17c <- function (mu, RES, lam, vres, nodos1, cvar){

  RES<-as.matrix(RES)

  n<-size(RES)[1]
  m<-size(RES)[2]

  if (n>m){
    ni <- n
  }
  else {
    ni <- m
  }

  k <- dim(nodos1)[1]

  p <- matrix(0,ni)
  L1 <- matrix(0,k,k)
  L2 <- matrix(0,k,k)
  deno <- 0
  nume1 <- 0
  nume2 <- 0
  nume3 <- 0
  nume4 <- 0

  for (l in 1:k){


      L1[l] <- 1

      for (j in 1:ni){

        term0 <- lam[j] %*% sqrt(cvar+vres[j])
        term1 <- 0.399 / term0
        term2 <- RES[j] - mu[j] - (lam[j] %*% nodos1[l,1])
        term3 <- (term2 / term0) %*% (term2 / term0)
        term4 <- exp(-0.5 * term3)
        term5 <- term1 * term4
        L1[l] <- L1[l] * term5
      }

      p <- as.matrix(L1[l] * nodos1[l,2]) # evitar named num

      deno <- deno + p
      nume1 <- nume1 + p * nodos1[l,1]
      nume3 <- nume3 + p * nodos1[l,1] * nodos1[l,1]
  }


  th <- nume1 / deno

  st <- sqrt(nume3 / deno - (th*th))

  OUT <- list("th"=th, "st"=st)
  return(OUT)

}
