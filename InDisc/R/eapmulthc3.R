eapmulthc3 <- function (mu, RES, lam, vres, cvari, nodos1){

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

  th <- matrix(0,1,3)
  vi <- 0
  st <- matrix(0,1,3)
  sv <- 0

  p <- matrix(0,ni)
  L1 <- array(0,dim=c(k,k,k))
  deno <- 0
  nume1 <- 0
  nume2 <- 0
  nume3 <- 0
  nume4 <- 0
  nume5 <- 0
  nume6 <- 0
  nume7 <- 0
  nume8 <- 0


  for (l in 1:k){
    for (t in 1:k){
      for(u in 1:k){
        #for (i in 1:k){

          L1[l,t,u] <- 1

          for (j in 1:ni){

            lamtmp <- cbind(lam[j,1],lam[j,2],lam[j,3])
            rlamtmp <- sqrt(lamtmp %*% (transpose(lamtmp)))

            term0 <- rlamtmp %*% sqrt(cvari+vres[j])
            term1 <- 0.399 / term0
            term2 <- RES[j] - mu[j] - (lam[j,1] %*% nodos1[l,1]) - (lam[j,2] %*% nodos1[t,1]) - (lam[j,3] %*% nodos1[u,1])
            term3 <- (term2 / term0) %*% (term2 / term0)
            term4 <- exp(-0.5 * term3)
            term5 <- term1 * term4
            L1[l,t,u] <- L1[l,t,u] * term5
          }

          p <- as.matrix(L1[l,t,u] * nodos1[l,2] * nodos1[t,2] * nodos1[u,2])  # evitar named num

          deno <- deno + p
          nume1 <- nume1 + p * nodos1[l,1]
          nume2 <- nume2 + p * nodos1[t,1]
          nume3 <- nume3 + p * nodos1[u,1]

          nume5 <- nume5 + p * nodos1[l,1] * nodos1[l,1]
          nume6 <- nume6 + p * nodos1[t,1] * nodos1[t,1]
          nume7 <- nume7 + p * nodos1[u,1] * nodos1[u,1]
        }
      #}
    }
  }

  th[1] <- nume1 / deno
  th[2] <- nume2 / deno
  th[3] <- nume3 / deno

  st[1] <- sqrt(nume5 / deno - (th[1]*th[1]))
  st[2] <- sqrt(nume6 / deno - (th[2]*th[2]))
  st[3] <- sqrt(nume7 / deno - (th[3]*th[3]))


  OUT <- list("th"=th, "st"=st)

  return(OUT)

}
