MStep.ecm <- function (Y, g, q, pivec, B, mu, D, sigma_type, D_type, tau, eta, ...){
  
  #Based on the function MStep.mfa from package EMMIXmfa. 
  #See package EMMIXmfa for more details. 
  
  p <- ncol(Y)
  n <- nrow(Y)
  if (p == 2) { qmax = 1 } else {
    qmax <- floor(p + (1-sqrt(1+8*p))/2)}
  n_i <- matrix(colSums(tau),nrow = 1)

  #CM-Step 1: Updating pi and mu_i
  pivec <- n_i/n
  for (i in 1:g) mu[, i] <- matrix(colSums(sweep(Y, MARGIN = 1,
                                                 tau[, i], "*")))/sum(tau[, i])

  #Calculate quantities for CM-Step 2
  Vtilde <- array(dim = c(p,p,g))
  Lambda <- array(dim = c(qmax,g))
  Lambda.new <- array(dim = c(qmax,g))
  eigenvecs <- vector(mode = 'list')
  for(i in 1:g){
    Ymu <- sweep(Y, 2, mu[, i, drop = FALSE], "-")
    Vi <- (t(Ymu) %*% sweep(Ymu, 1, tau[, i], "*"))/n_i[i]
    D.is <- diag(1/sqrt(diag(D[,,i])))
    Vtilde[,,i] <- D.is%*%Vi%*%D.is
    temp <- eigen(Vtilde[,,i])
    eigenvecs[[i]] <- Re(temp$vectors)
    Lambda[,i] <- Re(temp$values[1:qmax])
    if(any(Lambda[,i]<0)){Lambda[(Lambda[,i] < 0),i] <- 1e-20}
  }

  newB <- array(dim = c(p,q,g))

  #CM-Step 2: Updating B_i, then D_i in same loop.
  for(i in 1:g){
    qi <- sum(Lambda[,i]>1); if(qi > q){qi <- q}
    if(qi==0){
      model <- "bad"
      class(model) <- "error"
      break
    }
    Uqi <- try(matrix(eigenvecs[[i]][,1:qi],ncol = qi), silent = TRUE)

    if ((any(class(Uqi) %in% "try-error")) || (any(class(Uqi) %in%
                                                   "character"))) {
      model <- "bad"
      class(model) <- "error"
      break
    }

    Ri <- matrix(diag(q)[1:qi,],nrow=qi,ncol=q)
    if(qi == 1){mid = sqrt(Lambda[1:qi,i] - rep(1,qi))} else { mid = mydiag(sqrt(Lambda[1:qi,i] - rep(1,qi)))}
    newB[,,i] <- diag(sqrt(diag(D[,,i])))%*%Uqi%*%mid%*%Ri

    C <- diag(p); Us <- Uqi%*%(mydiag(1/Lambda[1:qi,i]) - mydiag(qi)); V <- t(Uqi)
    bvec <- matrix(0,nrow = p, ncol = 1)
    psitilde <- array(p,1)
    for(r in 1:p){
      c <- matrix(C[1:r,r],nrow = r); v <- matrix(V[,1:r],ncol = r)%*%c;
      bvec[r:p] <- matrix(Us[r:p,],nrow = p - r + 1) %*% v
      b <- bvec[r] + 1;
      a <- t(v)%*%mydiag(1/Lambda[1:qi,i] - Lambda[1:qi,i])%*%v + t(c) %*% matrix(Vtilde[1:r,1:r,i], nrow = r) %*% c
      psitilde[r] <- max( ((a-b)/b^2 + 1)*D[r,r,i], eta);
      omega <- psitilde[r]/D[r,r,i] - 1
      if (r < p && omega != 0){
        ratio <- omega/(1 + omega*b);
        C[1:r, (r+1):p] <- C[1:r, (r+1):p] -ratio*c%*%t(bvec[(r+1):p])
      }
    }
    D[,,i] <- diag(as.vector(psitilde))
  }

  model <- list(g = g, q = q, pivec = pivec, B = newB, mu = mu,
                D = D, sigma_type = sigma_type, D_type = D_type)
  class(model) <- "mfa"
  return(model)
}
