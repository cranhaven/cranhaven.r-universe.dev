yX <- 
  function(response, treatments, blocks) {
    n <- length(response)
    treat <- unique(treatments)
    ntreat <- length(treat)
    block <- unique(blocks)
    nblock <- length(block)
    X <- NULL
    y <- NULL
    for (i in 1:ntreat) {
      for (j in 1:nblock) {
        yij <- response[treatments==treat[i]&blocks==block[j]]
        y <- c(y,yij)
        nij <- length(yij)
        xij <- c(1,rep(0,(ntreat + nblock)))
        xij[1+i] <- 1
        xij[1+ntreat+j] <- 1
        X <- rbind(X, rep(1, nij)%*%t(xij))
      }
	  }
	  return(list(y, X))
  }
