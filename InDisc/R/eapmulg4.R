eapmulg4 <- function (DIF, RES, alpha, vres, nodos1, nodos2){

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

  th <- matrix(0,1,4)
  vi <- 0
  st <- matrix(0,1,4)
  sv <- 0

  p <- matrix(0,ni)
  L1 <- array(0,dim=c(k,k,k,k,k))
  deno <- 0
  nume1 <- 0
  nume2 <- 0
  nume3 <- 0
  nume4 <- 0
  nume5 <- 0
  nume6 <- 0
  nume7 <- 0
  nume8 <- 0
  nume9 <- 0
  nume10 <- 0

  for (l in 1:k){
    for (t in 1:k){
      for (u in 1:k){
        for (b in 1:k){
          for (i in 1:k){

            L1[l,t,u,b,i] <- 1

            for (j in 1:ni){

              alphatmp <- cbind(alpha[j,1],alpha[j,2],alpha[j,3])
              thettmp <- rbind(nodos1[l,1], nodos1[t,1],nodos1[u,1])

              if ((RES[j]-1) != 0){
                p1 <- pcongmul(DIF[RES[j]-1,j], alphatmp, thettmp, nodos2[i,1], vres[j])
              }
              p2 <- 0

              if (DIF[RES[j],j] != 0){
                p2 <- pcongmul(DIF[RES[j],j], alphatmp, thettmp, nodos2[i,1], vres[j])
              }

              if ((RES[j]-1) == 0){
                p[j] <- 1 - p2
              }
              else {
                p[j] <- p1 - p2
              }


            }

            L1[l,t,u,b,i] <- 1

            for (j in 1:ni){
              L1[l,t,u,b,i] <- L1[l,t,u,b,i] * p[j]
            }

            pp <- as.matrix(L1[l,t,u,b,i] * nodos1[l,2] * nodos1[t,2] * nodos1[u,2] * nodos1[b,2] * nodos2[i,2]) # no named num


            deno <- deno + pp
            nume1 <- nume1 + pp * nodos1[l,1]
            nume2 <- nume2 + pp * nodos1[t,1]
            nume3 <- nume3 + pp * nodos1[u,1]
            nume4 <- nume4 + pp * nodos1[b,1]
            nume5 <- nume5 + pp * nodos2[i,1]

            nume6 <- nume6 + pp * nodos1[l,1] * nodos1[l,1]
            nume7 <- nume7 + pp * nodos1[t,1] * nodos1[t,1]
            nume8 <- nume8 + pp * nodos1[u,1] * nodos1[u,1]
            nume9 <- nume9 + pp * nodos1[b,1] * nodos1[b,1]
            nume10 <- nume10 + pp * nodos2[i,1] * nodos2[i,1]
          }
        }
      }
    }
  }

  th[1] <- nume1 / deno
  th[2] <- nume2 / deno
  th[3] <- nume3 / deno
  th[4] <- nume4 / deno
  vi <- nume5 / deno

  st[1] <- sqrt(nume6 / deno - (th[1]*th[1]))
  st[2] <- sqrt(nume7 / deno - (th[2]*th[2]))
  st[3] <- sqrt(nume8 / deno - (th[3]*th[3]))
  st[4] <- sqrt(nume8 / deno - (th[4]*th[4]))
  sv <- sqrt(nume10 / deno - (vi*vi))

  # reli th and PDD
  reli_th <- 1 - (st^2)
  reli_PDD <- 0.4159/ (0.4159 + sv^2) # nodes variance

  OUT <- list("th"=th, "vi"=vi, "st"=st, "sv"=sv, "reli_th" = reli_th, "reli_PDD" = reli_PDD)
  return(OUT)

}
