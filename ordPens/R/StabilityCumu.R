
Stability.cumu <- function(x, y, lambda, n_iter=100, type=c("selection", "fusion"), ...){
  
  type <- match.arg(type)
  
  n <- nrow(x)
  Pi <- matrix(0, nrow=ncol(x), ncol=length(lambda))
  msize <- matrix(NA, nrow=n_iter, ncol=length(lambda))

  for(m in 1:length(lambda)){
    
    myS <- rep(0, ncol(x))
    
    for (i in 1:n_iter) {
      
      idx <- sample(n)
      idx1 <- idx[1:(n/2)]
      
      myy <- y[idx1]  
      myy <-c(myy, 1:max(x)) 
      
      myx <- x[idx1,]  
      myx <- rbind(myx, matrix(rep(1:max(x), times=ncol(x)), ncol=ncol(x), byrow=F, dimnames=list((nrow(x)+1):(nrow(x)+max(x)),colnames(x))))
      
      Xcod <- coding(myx, constant = F)
      
      lev <- seq_len(max(na.omit(myy))) 
      q <- length(lev) - 1; p <- ncol(Xcod) 
      xind <- c() 
      
      for(j in 1:ncol(myx)){
        tab <- apply(myx,2,table)
        if(is.list(tab)){
          xind <- c(xind, rep(j+1, unlist(lapply(tab,length))[j]-1))
        }else if(is.matrix(tab)){
          xind <- c(xind, rep(j+1, unlist(apply(tab,2,length))[j]-1))
        }
      }  

      if(type=="fusion"){
        model <- ordinalNet(x=Xcod, y=factor(c(myy)), family="cumulative", link="logit", 
                            lambdaVals = lambda[m]/length(myy),...) 
        mcoef <- -t(model$coefs[,-(1:q)])
        for (j in 1:max(xind-1)) {
          mcoef[(xind-1) == j ] <- cumsum(mcoef[(xind-1) == j ])
        } 
      }else{
        model <- ord.glasso(myx, y=c(myy), lambda=lambda[m], control=ordglasso_control(list(max.iter=1000)),...)  
        mcoef <- model$coef[1:p] 
      }
      
      select <- rep(0, ncol(x))
      index <- xind-1 
      for(k in 1:ncol(x)){  
        if(any(mcoef[index==k] != 0)) select[k] <- 1
      }
      myS <- select + myS
      msize[i,m] <- sum(select)
      
    }  
    
    Pi[,m] <- myS/n_iter 
    
  }  
  
  colnames(Pi) <- colnames(msize) <-  round(lambda, 3)

 return(list(Pi=Pi, msize=msize)) 
}

