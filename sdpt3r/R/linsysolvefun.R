linsysolvefun <- function(L,b){
  b <- as.matrix(b)
  x <- matrix(0,nrow(b),ncol(b))
  for(k in 1:ncol(b)){
    if(L$matfct_options == "chol"){
      x[L$perm,k] <- mextriang(L$R,mextriang(L$R,b[L$perm,k],2),1)
    }else if(L$matfct_options == "spchol"){
      x[L$perm,k] <- mextriang(L$Rt,mextriangsp(L$R,b[L$perm,k],2),1)
    }else if(L$matfct_options == "ldl"){
      X[L$p,k] <- t(t(solve(L$D,solve(L$L,b[L$p,k]))) / L$L)
    }else if(L$matfct_options == "spldl"){
      btmp <- b[,k] * L$s
      xtmp[L$p,1] <- solve(solve(L$Lt,solve(L$D,solve(L$L,btmp[L$p]))))
      x[,k] <- xtmp * L$s
    }else if(L$matfct_options == "lu"){
      x[,k] <- as.numeric(solve(L$U,solve(L$L,as.matrix(t(L$p) %*% b[,k]))))
    }else if(L$matfct_options == "splu"){
      btmp <- b[,k]/L$s
      x[L$q,k] <- solve(L$U,solve(L$L,as.matrix(t(L$p) %*% btmp)))
    }
  }
  return(x)
} 