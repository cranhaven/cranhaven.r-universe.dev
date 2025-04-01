eap_grad_obli<-function(X, LAM, PHI, THRES, sigj,grid,index_nodos){

  ni   <- length(X)
  r    <- ncol(cbind(LAM)) # Works in case LAM is vector or matrix
  nnod <- length(grid)
  hnod <- nrow(index_nodos)


  d<-grid[2]-grid[1]

  nume_th <- matrix(0,r,1)
  nume_se <- matrix(0,r,1)

  # Computations vectorized from now on:
  zi.mat <- t(sapply(1:hnod, function(h) grid[index_nodos[h, ]]))

  # p1:
  p1.mat <- matrix(0, hnod, ni)
  ind    <-  which(X != 1)
  if (r==1){
    tmp1   <- t(zi.mat) %*% LAM[ind]
  }
  else {
    if ((size(ind)[2])==1){
      tmp1 <- zi.mat %*% (LAM[ind, ])
    }
    else {
      tmp1   <- zi.mat %*% t(LAM[ind, ])
    }
  }
  tmp2   <- tmp1 - matrix(rep(THRES[cbind(X[ind] - 1, ind)], hnod), nrow = hnod, byrow = TRUE)
  tmp3   <- tmp2 / matrix(rep(sigj[ind], hnod), nrow = hnod, byrow = TRUE)
  tmp4   <- exp(1.702 * tmp3)
  tmp5   <- tmp4 / (1+tmp4)
  tmp6   <- matrix(rep(X[ind]-1, hnod), nrow = hnod, byrow = TRUE) * log(tmp5)
  p1.mat[, ind] <- tmp6
  rm(list=ls(pattern = c("tmp")))

  # p2:
  p2.mat <- matrix(0, hnod, ni)
  ind    <-  which(THRES[cbind(X,1:ni)] != 0)
  if (r==1){
    tmp1   <- t(zi.mat) %*% LAM[ind]
  }
  else {
    if ((size(ind)[2])==1){
      tmp1 <- zi.mat %*% (LAM[ind, ])
    }
    else {
      tmp1   <- zi.mat %*% t(LAM[ind, ])
    }
  }
  tmp2   <- tmp1 - matrix(rep(THRES[cbind(X[ind], ind)], hnod), nrow = hnod, byrow = TRUE)
  tmp3   <- tmp2 / matrix(rep(sigj[ind], hnod), nrow = hnod, byrow = TRUE)
  tmp4   <- exp(1.702 * tmp3)
  tmp5   <- tmp4 / (1+tmp4)
  tmp6   <- matrix(rep(X[ind], hnod), nrow = hnod, byrow = TRUE) * log(1-tmp5)
  p2.mat[, ind] <- tmp6
  rm(list=ls(pattern = c("tmp")))

  # p, L1:
  p.mat  <- p1.mat + p2.mat
  L1.vec <- rowSums(p.mat)

  # term2:
  dospi  <-6.2832
  tmp1   <-(dospi)^(r/2) # JNT - 'n' in ordnormulti is r, here, right?
  if (r == 1) tmp2 <- sqrt(PHI) else tmp2 <- sqrt(det(PHI))
  tmp3   <- 1 / (tmp1*tmp2)
  PHIinv <- solve(PHI)
  tmp4   <- rep(NA, hnod)
  if (r==1){
    for (h in 1:hnod) tmp4[h] <- t(cbind(zi.mat[h])) %*% PHIinv %*% cbind(zi.mat[h])
  }
  else {
    for (h in 1:hnod) tmp4[h] <- t(cbind(zi.mat[h,])) %*% PHIinv %*% cbind(zi.mat[h,])
  }
  tmp5   <- as.vector(tmp3) * exp(-.5* tmp4) #tmp5 = term1
  term2.vec <- tmp5 * d * d
  rm(list=ls(pattern = c("tmp")))

  pp.vec  <- exp(L1.vec)*term2.vec
  deno    <- sum(pp.vec)
  if (r==1){
    nume_th <- sum(pp.vec*zi.mat)
    nume_se <- sum(pp.vec*zi.mat^2)
  }
  else {
    nume_th <- colSums(matrix(rep(pp.vec, r), nrow = hnod, byrow = FALSE) * zi.mat)
    nume_se <- colSums(matrix(rep(pp.vec, r), nrow = hnod, byrow = FALSE) * (zi.mat^2))
}

  TH   <- nume_th / deno
  #SE   <- sqrt(nume_se/deno - (TH^2))
  #RELI <- 1 - SE^2

  IPHI <- solve(PHI)
  VAR_E <- nume_se / deno - (TH^2)
  VAR_EE <- 1 - VAR_E
  SE <- sqrt(VAR_E / (1 - IPHI %*% VAR_E))
  RELI <- (VAR_EE - SE^2) / VAR_EE

  OUT  <- list("th_i"=TH, "se_i"=SE, "reli_i"=RELI)
  return(OUT)
}
