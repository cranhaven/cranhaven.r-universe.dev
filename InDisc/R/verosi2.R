verosi2 <- function(DIF, RES, alpha, vres, th, vari){

  n <- dim(RES)
  m <- dim(RES)
  if (n>m){
    ni <- n
  }
  else {
    ni <- m
  }

  p <- matrix(0,ni)
  L1 <- matrix(0,1,1)

  for (j in 1:ni){
    if ((RES[j] - 1) != 0){
      p1 <- pcong(DIF[RES[j]-1,j], alpha[j], th, vari, vres[j])
    }
    p2 <- 0

    if (DIF[RES[j],j] != 0){
      p2 <- pcong(DIF[RES[j],j], alpha[j], th, vari, vres[j])
    }

    if ((RES[j]-1) == 0){
      p[j] <- 1- p2
    }
    else {
      p[j] <- p1 - p2
    }

  }

  L1 <- 1
  for (j in 1:ni){
    L1 <- L1 * p[j]
  }

}
