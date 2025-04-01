rthres <- function(X){
  m <- size(X)[2]
  ncat <- max(max(X) - min (X) + 1)

  B <- matrix(0,(ncat-1),m)

  for (i in 1:m){
    if (ncat == 2){
      OUT <- thresdic(X[,i])
    }
    if (ncat == 3){
      OUT <- thres3(X[,i])
    }
    if (ncat == 4){
      OUT <- thres4(X[,i])
    }
    if (ncat == 5){
      OUT <- thres5(X[,i])
    }

    B[,i] = OUT

  }

  return(B)

}
