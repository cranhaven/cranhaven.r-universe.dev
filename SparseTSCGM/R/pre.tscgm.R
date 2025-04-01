        
pre.tscgm <- function(xy.data=xy.data, time=time, model=c("ar1","ar2"))
 {       
  model = match.arg(model)

  if (model=="ar1") {
   X <- xy.data[1:time-1,,,drop=FALSE]
   Y <- xy.data[2:time,,,drop=FALSE]
      }
  else if (model=="ar2") {
    t1=time-1
    t2=time-2
    Y <- round(xy.data[3:time,,,drop=FALSE],3)
    X1 <- round(xy.data[2:t1,,,drop=FALSE],3)
    X2<- round(xy.data[1:t2,,,drop=FALSE],3)
    X <- abind(X1, X2,along = 2 )

  }
  T <- dim(Y)[1]
  p <- dim(X)[2]
  n <- dim(Y)[3]
  q <- dim(Y)[2]
  xtyi <- array(NA, c(p,q,n))
  xtxi <- array(NA, c(p,p,n))
  ytyi <- array(NA, c(q,q,n))

  for(i in 1:n){
      XX <- X[,,i]
      YY <- Y[,,i]
      XX2 <- X[,,i]^2
      YY2 <- Y[,,i]^2
      xtyi[,,i]=crossprod(XX,YY)
      xtxi[,,i]=crossprod(XX)
      ytyi[,,i]=crossprod(YY)

    }
  xty=apply(xtyi, c(1,2), sum)
  xtx=apply(xtxi, c(1,2), sum)
  yty=apply(ytyi, c(1,2), sum)
  xtxt=apply(xtxi, c(1,2), sum)/(n*T)
  xtx2=(n*T)*colMeans(apply(XX2, c(1,2), sum))
  yty2=(n*T)*colMeans(apply(YY2, c(1,2), sum))

  out.data <- list(xty=xty, xtx=xtx, yty=yty, xtxt=xtxt, xtx2=xtx2, yty2=yty2,
      p=p, T=T, n=n, q=q)
  return(out.data)
}

         