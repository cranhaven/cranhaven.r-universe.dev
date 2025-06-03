func.input <- function(x, formula, p, q, f=NULL, eps=1e-5, off.diag=TRUE){
  n <- dim(x)[1]; T <- dim(x)[2]; r <- 2*max(p, q)
  denom <- res <- tx <- matrix(0, nrow=n, ncol=T-r); h <- x*0
  c.mat <- c()
  for(i in 1:n){
    gf <- garchFit(formula = formula, data = x[i, ], trace=FALSE, include.mean=FALSE)
    c <- coef(gf)
    #Sanity check to examine if a+b<1. If not then run a simple GARCH(0,1) model which is a EWMA equivalent.
    if (sum(c[-1]) >= 1)  {
      print(paste("Component series",i,"returns a+b > 1: Re-estimating assuming GARCH(0,1)"))
      c <- c*0
      gf <- garchFit(formula = ~garch(1, 0), data = x[i, ], trace=FALSE, include.mean=FALSE)
      c[1:2] <- coef(gf)
    }
    c.mat <- rbind(c.mat, c)
    h[i, ] <- gf@h.t
    res[i, ] <- x[i, -(1:r)]/sqrt(h[i, -(1:r)])
  }
  if(is.null(f)) f <- apply(c.mat, 1, function(z){max(1, min(.99, sum(z[-1]))/max(.01, 1-sum(z[-1])))})
  for(t in (r+1):T){
    denom[, t-r] <- c.mat[, 1] # apply(c.mat, 1, max)
    if(p > 0) denom[, t-r] <- denom[, t-r] + apply(c.mat[, 1+1:p, drop=FALSE]*x[, (t-1):(t-p), drop=FALSE]^2, 1, sum)/f
    if(q > 0) denom[, t-r] <- denom[, t-r] + apply(c.mat[, 1+p+1:q, drop=FALSE]*h[, (t-1):(t-q), drop=FALSE], 1, sum)/f
  }
  denom <- denom + eps*x[, -(1:r)]^2
  tx <- x[, -(1:r)]/sqrt(denom)
  ttx <- t(apply(tx^2, 1, function(z){z/mean(z)}))
  if(off.diag){ sgn <- sign(tx%*%t(tx)); ttx <- rbind(ttx, t(func_input_off(tx, sgn, 0, 0)$input)) } else sgn <- c()
  list(denom=denom, tx=tx, ttx=ttx, h=h, res=res, c.mat=c.mat, sgn=sgn, f=f)
}


make.tree <- function(y, op, gam, dw, rule=NULL){
  len <- ncol(y)
  if(is.null(rule)) rule <- round(log(len, 2)/2)
  tree <- list(matrix(0, 6, 1))
  mat <- c()
  
  fd <- func_density(y, gam)
  stat <- fd$res[, op]
  test.stat <- max(stat[-c((1:dw), (len-dw):len)])
  hat.chp <- min(which(stat==test.stat))
  
  tree[[1]][1, 1] <- 1
  tree[[1]][2, 1] <- 1
  tree[[1]][3, 1] <- hat.chp
  tree[[1]][4, 1] <- len
  tree[[1]][5, 1] <- 0
  tree[[1]][6, 1] <- test.stat
  mat <- cbind(mat, c(tree[[1]][-5, ], 1, 1))
  
  j <- 1
  while(length(tree)==j & j < rule){
    npc <- dim(tree[[j]])[2]
    if(sum(tree[[j]][4,]-tree[[j]][2,]-rep(4*dw, npc)>0)){
      ncc <- 0; i <- 1
      while(i <= npc){
        if(tree[[j]][3, i]-tree[[j]][2, i]+1>4*dw){
          s <- tree[[j]][2, i]; e <- tree[[j]][3, i]
          fd <- func_density(y[, s:e], gam)
          stat <- fd$res[, op]
          test.stat <- max(stat[-c((1:dw), (e-s+1-dw):(e-s+1))])
          hat.chp <- s+min(which(stat==test.stat))-1
          
          if(length(tree)==j) tree <- c(tree, list(matrix(0, 6, 0)))
          ncc <- ncc+1
          tree[[j+1]] <- matrix(c(tree[[j+1]], matrix(0, 6, 1)), 6, ncc)
          tree[[j+1]][1, ncc] <- 2*tree[[j]][1, i]-1
          tree[[j+1]][2, ncc] <- s
          tree[[j+1]][3, ncc] <- hat.chp
          tree[[j+1]][4, ncc] <- e
          tree[[j+1]][5, ncc] <- 0
          tree[[j+1]][6, ncc] <- test.stat
          mat <- cbind(mat, c(tree[[j+1]][-5, ncc], j+1, ncc))
        }
        if(tree[[j]][4, i]-tree[[j]][3, i]>4*dw){
          s <- tree[[j]][3, i]+1; e <- tree[[j]][4, i]
          fd <- func_density(y[, s:e], gam)
          stat <- fd$res[, op]
          test.stat <- max(stat[-c((1:dw), (e-s+1-dw):(e-s+1))])
          hat.chp <- s+min(which(stat==test.stat))-1
          
          if(length(tree)==j) tree <- c(tree, list(matrix(0, 6, 0)))
          ncc <- ncc+1
          tree[[j+1]] <- matrix(c(tree[[j+1]], matrix(0, 6, 1)), 6, ncc)
          tree[[j+1]][1, ncc] <- 2*tree[[j]][1, i]
          tree[[j+1]][2, ncc] <- s
          tree[[j+1]][3, ncc] <- hat.chp
          tree[[j+1]][4, ncc] <- e
          tree[[j+1]][5, ncc] <- 0
          tree[[j+1]][6, ncc] <- test.stat
          mat <- cbind(mat, c(tree[[j+1]][-5, ncc], j+1, ncc))
        }
        i <- i+1
      }
      j <- j+1
    } else{ 
      break
    }
  }
  list(tree=tree, mat=mat)
}

tri.kern <- function(len, h){
  filter <- rep(0, h+1)
  i <- 0
  while (i <= h) {
    u <- i/h
    if (u < 1/2) 
      filter[i+1] <- 1
    if (u >= 1/2 & u < 1) 
      filter[i+1] <- 2 * (1 - u)
    if (u > 1) 
      break
    i <- i + 1
  }
  filter
}

get.gg <- function(z, M=NULL, C=2, max.K=5){
  len <- length(z)
  max.K <- max(max.K, sqrt(log(len)))
  acv <- acf(z, type="covariance", lag.max=len-1, plot=FALSE)$acf[,,1]
  if(is.null(M)){
    l <- 1; ind <- 0
    while(l < sqrt(len)){
      if(abs(acv[l+1])/acv[1] < C*sqrt(log(len)/len)){
        ind <- ind+1
      } else{
        if(ind>0) ind <- 0
      }
      if(ind==max.K) break
      l <- l+1
    }
    lam <- max(1, l-max.K); M <- 2*lam
  }
  k <- tri.kern(len, M)	
  c(acv[1]+2*sum(k[-1]*acv[2:(M+1)]), 2*sum(k[-1]*(1:M)*acv[2:(M+1)]))
}