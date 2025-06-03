
func.null.stat <- function(mat, op=2, res, T, c.mat, prob, burnin=100, p, q, Bsim=200, boot.op=2, f, eps, off.diag=TRUE, sgn, mby, tby, gam,do.parallel=0){
  if(do.parallel > 0){
    cl <- parallel::makeCluster(do.parallel); doParallel::registerDoParallel(cl)
  }
  `%parDo%` <- ifelse(do.parallel > 0, `%dopar%`, `%do%`)
  n <- dim(res)[1]; len <- dim(res)[2]
  if(boot.op==2) C <- cor.shrink(t(res), verbose=FALSE)
  null.stat <- foreach(l=iter(1:Bsim), .combine=cbind, .packages=c("Rcpp", "RcppArmadillo", "segMGarch")) %parDo% {
    #if(boot.op==2) nz <- t(mvrnorm_arma(T+burnin, C))
    if(boot.op==1){
      ind <- c()
      while(length(ind)<T+burnin){ 
        L <- rgeom(1, prob); I <- sample(len, 1)
        ind <- c(ind, rep(1:len, 1+ceiling(L/len))[I:(I+L-1)]) 
      }
      ind <- ind[1:(T+burnin)]; nz <- res[, ind]
    }
    if(boot.op==3){
      ind <- sample(1:len, T+burnin, replace=TRUE)
      nz <- res[, ind]
    }
    ndenom <- nh <- nx <- nz*0
    for(t in (1+max(p, q)):(T+burnin)){
      ndenom[, t] <- nh[, t] <- c.mat[, 1]
      if(p > 0){
        tmp <- apply(c.mat[, 1+1:p, drop=FALSE]*nx[, (t-1):(t-p), drop=FALSE]^2, 1, sum)
        nh[, t] <- nh[, t] + tmp
        ndenom[, t] <- ndenom[, t] + tmp/f
      }
      if(q > 0){
        tmp <- apply(c.mat[, 1+p+1:q, drop=FALSE]*nh[, (t-1):(t-q), drop=FALSE], 1, sum)
        nh[, t] <- nh[, t] + tmp
        ndenom[, t] <- ndenom[, t] + tmp/f 		
      }	 
      nx[, t] <- sqrt(nh[, t])*nz[, t]
      ndenom[, t] <- ndenom[, t] + eps*nx[, t]^2
    }
    nx <- nx[, -(1:burnin)]; ndenom <- ndenom[, -(1:burnin)]
    ntx <- nx/sqrt(ndenom)
    nttx <- t(apply(ntx^2, 1, function(z){z/mean(z)}))
    if(off.diag) nttx <- rbind(nttx, t(func_input_off(nz[, -(1:burnin)], sgn, 0, 0)$input))
    tmp <- c()
    for(i in 1:ncol(mat)){
      s <- mat[2, i]; e <- mat[4, i]
      bfd <- func_density_by(nttx[, s:e], mby, tby, gam)
      tmp <- c(tmp, max(bfd$res[, op]))
      rm(bfd)
    }
    rm(nz, nh, nx, ntx, nttx)
    tmp
  }
  if(do.parallel > 0) parallel::stopCluster(cl)
  null.stat
}
