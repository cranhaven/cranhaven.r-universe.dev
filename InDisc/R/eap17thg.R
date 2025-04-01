eap17gth <- function (DIF, RES, alpha, vres, nodos1, vari){

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

    for (i in 1:k){

      #

      for (j in 1:ni){

        if ((RES[j]-1) != 0){
          p1 <- pcong(DIF[RES[j]-1,j], alpha[j], nodos1[l,1], vari, vres[j])
        }
        p2 <- 0

        if (DIF[RES[j],j] != 0){
          p2 <- pcong(DIF[RES[j],j], alpha[j], nodos1[l,1], vari, vres[j])
        }

        if ((RES[j]-1) == 0){
          p[j] <- 1 - p2
        }
        else {
          p[j] <- p1 - p2
        }


      }

      L1[l,i] <- 1

      for (j in 1:ni){
        L1[l,i] <- L1[l,i] * p[j]
      }

      p <- as.matrix(L1[l,i] * nodos1[l,2] ) # no named num


      deno <- deno + p
      nume1 <- nume1 + p * nodos1[l,1]
      nume3 <- nume3 + p * nodos1[l,1] * nodos1[l,1]
    }
  }

  th <- nume1 / deno

  st <- sqrt(nume3 / deno - (th*th))

  OUT <- list("th"=th, "st"=st )
  return(OUT)

}
