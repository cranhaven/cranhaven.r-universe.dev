plotTAG <- function(object, include.legend = TRUE, legend.position = "bottomright"){
  omega.est <- object$omega
  theta.est <- object$s
  delta.est <- object$delta
  lam.est <- object$lambda
  xx <- object$X
  d <- ncol(xx)
  n <- nrow(xx)

  if(is.null(object$y) == TRUE && is.null(object$ty) == TRUE){
    stop("Please include y or ty in the parTAG")
  }
  if(is.null(object$y) == TRUE){
    Y.t <- object$ty
    if(lam.est == 0){
      object$y <- exp(Y.t)
    }else{
      object$y <- (Y.t*lam.est + 1)^(1/lam.est)
    }
    YY <- object$y
  }

  if(is.null(object$ty) == TRUE){
    YY <- object$y
    if(lam.est == 0){
      object$ty <- log(YY)
    }else{
      object$ty <- (YY^(lam.est) - 1)/lam.est
    }
    Y.t <- object$ty
  }
  #check column names
  if(is.null(colnames(xx)) == TRUE){
    colnames(xx) <- paste("X",1:d)
  }

  #Caluculate TAG
  one <- matrix(1, nrow=n)
  Ide <- diag(1, n)
  K <- matrix(0, nrow=n, ncol=n)
  for(i in 1:d){
    K <- K + (omega.est[i]*exp(-(1/(theta.est[i]^2))*(as.matrix(dist(xx[,i], diag = TRUE, upper = TRUE))^2)))
  }
  K <- K + 10^(delta.est)*Ide
  inv.K=rcppeigen_invert_matrix(K)
  mu.hat=drop(t(one)%*%inv.K%*%Y.t/(t(one)%*%inv.K%*%one))
  hatc = inv.K %*% (Y.t - mu.hat)

  #Calculate main effects matrix
  xgrid <- seq(from=0,to=1,length=20)
  main.mat.y <- main.mat.ty <- matrix(0, nrow=length(xgrid), ncol=d)
  for(ind in 1:d){
    for(i in 1:length(xgrid)){
      h <- xgrid[i]-xx[,ind]
      amean  <- mean(exp(-(1/(theta.est[ind]^2))*(as.matrix(dist(xx[,ind], diag = TRUE, upper = TRUE))^2)) %*% hatc)
      main.mat.ty[i,ind] <- omega.est[ind]*(exp(-(1/(theta.est[ind]^2))*(h^2)) %*% hatc -  amean)
    }
  }


  rname <-  colnames(xx)

  plot(xgrid,  main.mat.ty[,1], ylim=range(main.mat.ty), type="l", xlab="", ylab="",
       lwd=2, col=1, main="Transformed Scale",
       cex.main=1.5, cex.axis=1.5, mgp=c(3,1,0), mar=c(2,4,2,1))
  #Main Effects Plot from TAG in the Transformed Scale
  for(ind in 2:d){
     points(xgrid,  main.mat.ty[,ind], type="l", lwd=2, col= ind, lty=ind)
  }
  legend(legend.position, lwd = rep(2,d), col=c(1:d), lty=c(1:d), legend = rname[1:d], cex=1, bty="n")



}

