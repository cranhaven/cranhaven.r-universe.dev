corTS <- function(xresid, yresid,
                 xz.dl.dtheta, yz.dl.dtheta,
                 xz.d2l.dtheta.dtheta, yz.d2l.dtheta.dtheta,
                 dxresid.dthetax, dyresid.dthetay,fisher=FALSE){
  
  TS = cor(xresid, yresid)
  
  xresid2 = xresid^2
  yresid2 = yresid^2
  xbyyresid = xresid * yresid
  mean.xresid = mean(xresid)
  mean.yresid = mean(yresid)
  mean.xbyyresid = mean(xbyyresid)
  
  bigphi = cbind(xz.dl.dtheta,
                 yz.dl.dtheta,
                 mean.xresid - xresid,
                 mean.yresid - yresid,
                 mean.xbyyresid - xbyyresid,
                 mean(xresid2)-xresid2,
                 mean(yresid2)-yresid2,
                 0)
  
  npar.xz = dim(xz.dl.dtheta)[2]
  npar.yz = dim(yz.dl.dtheta)[2]
  Ntheta = npar.xz + npar.yz + 6
  N = dim(xz.dl.dtheta)[1]
  
  A = matrix(0,Ntheta,Ntheta)
  A[1:npar.xz, 1:npar.xz] = xz.d2l.dtheta.dtheta
  A[npar.xz+(1:npar.yz), npar.xz+(1:npar.yz)] = yz.d2l.dtheta.dtheta
  A[Ntheta-6+(1:6), Ntheta-6+(1:6)] = diag(N, 6)
  
  bigpartial = rbind(c(dxresid.dthetax %*% rep(1, N), rep(0, npar.yz)),
                     c(rep(0, npar.xz), dyresid.dthetay %*% rep(1, N)),
                     c(dxresid.dthetax %*% yresid, dyresid.dthetay %*% xresid),
                     c(dxresid.dthetax %*% (2*xresid), rep(0, npar.yz)),
                     c(rep(0, npar.xz), dyresid.dthetay %*% (2*yresid)))
  
  A[Ntheta-6+(1:5), 1:(npar.xz+npar.yz)] = -bigpartial
  
  ## TS also equals numTS / sqrt(varprod) = numTS * revsvp
  numTS = mean.xbyyresid - mean.xresid * mean.yresid
  var.xresid = mean(xresid2) - mean.xresid^2
  var.yresid = mean(yresid2) - mean.yresid^2
  varprod = var.xresid * var.yresid
  revsvp = 1/sqrt(varprod)
  dTS.dvarprod = numTS * (-0.5) * revsvp^3
  
  smallpartial = N *
    c(-mean.yresid * revsvp + dTS.dvarprod * (-2*mean.xresid*var.yresid),
      -mean.xresid * revsvp + dTS.dvarprod * (-2*mean.yresid*var.xresid),
      revsvp,
      dTS.dvarprod * var.yresid,
      dTS.dvarprod * var.xresid)
  A[Ntheta, Ntheta-6+(1:5)] = -smallpartial
  
  A[Ntheta, Ntheta-6+(1:5)] = -smallpartial
  
  SS = solve(A, t(bigphi))
  var.theta = tcrossprod (SS, SS)
  varTS = var.theta[Ntheta, Ntheta]
  pvalTS = 2 * pnorm( -abs(TS)/sqrt(varTS))
  
  if (fisher==TRUE){
    ####Fisher's transformation
    TS_f <- log( (1+TS)/(1-TS) )
    var.TS_f <- varTS*(2/(1-TS^2))^2
    pvalTS <- 2 * pnorm( -abs(TS_f)/sqrt(var.TS_f))
  }
  
  list(TS=TS,var.TS=varTS, pval.TS=pvalTS)
}

corTS.stratification <- function(ind, 
                                 presid.x, presid.y,
                                 dl.dtheta.x, dl.dtheta.y,
                                 d2l.dtheta.dtheta.x, d2l.dtheta.dtheta.y,
                                 dpresid.dtheta.x, dpresid.dtheta.y,
                                 fisher=FALSE){
  
  
  n <- length(ind)
  npar.xz = dim(dl.dtheta.x)[2]
  npar.yz = dim(dl.dtheta.y)[2]
  Ntheta = npar.xz + npar.yz + 6
  
  xz.dl.dtheta <- dl.dtheta.x
  yz.dl.dtheta <- dl.dtheta.y
  
  xz.d2l.dtheta.dtheta <- as.matrix(d2l.dtheta.dtheta.x)
  yz.d2l.dtheta.dtheta <- as.matrix(d2l.dtheta.dtheta.y)
  
  xresid <- rep(0, n)
  yresid <- rep(0, n)
  
  xresid[which(ind==1)] <- presid.x[which(ind==1)]
  yresid[which(ind==1)] <- presid.y[which(ind==1)]
  
  dxresid.dthetax <- matrix(0, npar.xz, n)
  dyresid.dthetay <- matrix(0, npar.yz, n)
  
  dxresid.dthetax[, which(ind==1)] <- dpresid.dtheta.x[, which(ind==1)]
  dyresid.dthetay[, which(ind==1)] <- dpresid.dtheta.y[, which(ind==1)]

  
  TS = cor(xresid[which(ind==1)], yresid[which(ind==1)])
  
  xresid2 = xresid^2
  yresid2 = yresid^2
  
  
  xbyyresid <-  xresid * yresid
  mean.xresid <- mean(xresid[which(ind==1)])
  mean.yresid <- mean(yresid[which(ind==1)])
  mean.xbyyresid = mean(xbyyresid[which(ind==1)])
  mean.xresid2 <- mean(xresid[which(ind==1)]^2)
  mean.yresid2 <- mean(yresid[which(ind==1)]^2)
  
  
  bigphi <- cbind(xz.dl.dtheta,
                  yz.dl.dtheta,
                  (mean.xresid - xresid)*ind,
                  (mean.yresid - yresid)*ind,
                  (mean.xbyyresid - xbyyresid)*ind,
                  (mean.xresid2 - xresid2)*ind,
                  (mean.yresid2 - yresid2)*ind,
                  0)
  
  A = matrix(0,Ntheta,Ntheta)
  
  A[1:npar.xz, 1:npar.xz] <- xz.d2l.dtheta.dtheta
  A[npar.xz+(1:npar.yz), npar.xz+(1:npar.yz)] = yz.d2l.dtheta.dtheta
  A[Ntheta-6+(1:6), Ntheta-6+(1:6)] = diag(sum(ind), 6)
  
  bigpartial = rbind(c( dxresid.dthetax %*% ind, rep(0, npar.yz)),
                     c( rep(0, npar.xz), dyresid.dthetay %*% ind ),
                     c(dxresid.dthetax %*% yresid, dyresid.dthetay %*% xresid),
                     c(dxresid.dthetax %*% (2*xresid), rep(0, npar.yz)),
                     c(rep(0, npar.xz), dyresid.dthetay %*% (2*yresid)))
  
  
  A[Ntheta-6+(1:5), 1:(npar.xz+npar.yz)] = -bigpartial
  
  ## TS also equals numTS / sqrt(varprod) = numTS * revsvp
  numTS = mean.xbyyresid - mean.xresid * mean.yresid
  var.xresid = mean.xresid2 - mean.xresid^2
  var.yresid = mean.yresid2 - mean.yresid^2
  varprod = var.xresid * var.yresid
  revsvp = 1/sqrt(varprod)
  dTS.dvarprod = numTS * (-0.5) * revsvp^3
  
  smallpartial = 
    sum(ind)*c(-mean.yresid * revsvp + dTS.dvarprod * (-2*mean.xresid*var.yresid),
               -mean.xresid * revsvp + dTS.dvarprod * (-2*mean.yresid*var.xresid),
               revsvp,
               dTS.dvarprod * var.yresid,
               dTS.dvarprod * var.xresid)
  
  A[Ntheta, Ntheta-6+(1:5)] = -smallpartial
  
  
  
  SS = solve(A, t(bigphi))
  var.theta = tcrossprod (SS, SS)
  varTS = var.theta[Ntheta, Ntheta]
  
  pvalTS = 2 * pnorm( -abs(TS)/sqrt(varTS))
  
  
  
  if (fisher==TRUE){
    ####Fisher's transformation
    TS_f <- log( (1+TS)/(1-TS) )
    var.TS_f <- varTS*(2/(1-TS^2))^2
    pvalTS <- 2 * pnorm( -abs(TS_f)/sqrt(var.TS_f))
  }
  
  result <- c(TS, varTS, pvalTS)
  names(result) <- c("TS", "var.TS", "pval.TS")
  return(result)
}

