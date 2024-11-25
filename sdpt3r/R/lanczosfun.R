lanczosfun <- function(Xchol, dX, maxit=30, tol=1e-3){
  
  if(norm(dX, type="F") < 1e-13){
    return(list(lam=0, delta=0, res=0))
  }
  
  n <- max(dim(as.matrix(dX)))
  v <- randmat(n,1,0,"n")
  
  
  V <- matrix(0, n, maxit+1)
  v <- v/base::norm(v, type="2")
  V[,1] <- v
  H <- matrix(0, n, maxit+1)
  
  for(k in 1:maxit){
    w <- dX %*% as.matrix(mextriang(as.matrix(Xchol),v,1))
    w <- mextriang(as.matrix(Xchol),as.matrix(w),2)
    
    wold <- w
    if(k > 1){
      w <- w - H[k,k-1]*V[,k-1]
    }
    alp <- t(w) %*% matrix(V[,k], ncol=1)
    w <- w - alp*V[,k]
    H[k,k] <- alp
    ##
    ##
    ##
    if(base::norm(w, type="2") <= 0.8*base::norm(wold, type="2")){
      s <- t(t(w) %*% V[,1:k])
      w <- w - V[,1:k] %*% s
      H[1:k,k] <- H[1:k,k] + s
    }
    
    nrm <- base::norm(w, type="2")
    v <- w/nrm
    V[,k+1] <- v
    H[k+1,k] <- nrm
    H[k,k+1] <- nrm
    ##
    ##
    ##
    if(k %% 5 == 0 | k == maxit){
      Hk <- H[1:k,1:k]
      Hk <- .5*(Hk + t(Hk))
      out <- eigen(Hk)
      D <- out$values
      Y <- out$vectors
      eigH <- D
      idx <- order(eigH)
      res_est <- abs(H[k+1,k] * Y[k, idx[k]])
      if(res_est < 0.1*tol | k == maxit){
        lam <- eigH[idx[k]]
        lam2 <- eigH[idx[k-1]]
        z <- V[,1:k] %*% Y[,idx[k]]
        z2 <- V[,1:k] %*% Y[,idx[k-1]]
        
        tmp <- dX %*% mextriang(as.matrix(Xchol),z,1)
        res <- base::norm(mextriang(as.matrix(Xchol),as.matrix(tmp),2) - lam*z, type="2")
        tmp <- dX %*% mextriang(as.matrix(Xchol), z2, 1)
        res2 <- base::norm(mextriang(as.matrix(Xchol), as.matrix(tmp), 2) - lam*z2, type="2")
        
        tmp <- lam - lam2 - res2
        if(tmp > 0){
          beta <- tmp
        }else{
          beta <- .Machine$double.eps
        }
        delta <- min(res, res^2/beta)
        if(delta <= tol){
          break
        }
      }
    }
  }
  
  return(list(lam=lam, delta=delta, res=res))
  
}