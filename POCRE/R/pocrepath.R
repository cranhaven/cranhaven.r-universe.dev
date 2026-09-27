pocrepath<-function(y, x, delta=0.1, maxvar=dim(x)[1]/2, x.nop=NA, maxcmp=10,
                    ptype=c('ebtz','ebt','l1','scad','mcp'), lambda.init=1,
                    maxit=100, tol=1e-6, maxtps=500, gamma=3.7, pval=(dim(y)[2]==1))
{
  y <- as.matrix(y)
  x <- as.matrix(x)
  ptype <- ptype[1]

  retRes <- vector("list",length = maxtps)
  eps <- .Machine$double.eps
  n <- dim(x)[1]
  p <- dim(x)[2]
  k <- dim(y)[2]
  if(n!=dim(y)[1])
    stop("x and y should have the same number of rows!")

  ###Explore all tuning parameter values such that 0<#[beta~=0]<maxvar
  idxM <- 0
  lambda <- lambda.init
  while(idxM <= maxtps){
    cat(paste("Tuning Parameter:", lambda))
    idxM <- idxM + 1

    retRes[[idxM]] <- pocre(y,x,lambda,x.nop,maxvar,maxcmp,ptype,maxit,tol,gamma,pval)

    if(lambda>=lambda.init){
      if((retRes[[idxM]]$nzBeta==0) && (retRes[[idxM]]$bSparse)){
        if(retRes[[1]]$bSparse){
          lambda <- lambda.init-delta
        }else{
          break
        }
      }else{
        lambda <- lambda+delta
      }
    }else if((lambda<lambda.init) && (retRes[[idxM]]$bSparse)){
      lambda <- lambda-delta
    }else{
      break
    }
  }

  # Sort retRes according to lambda values:
  lambda <- NULL
  for(i in 1:idxM){
    lambda <- c(lambda,retRes[[i]]$lambda)
  }
  tmpL <- sort(lambda, index.return = T)$x
  idxL <- sort(lambda, index.return = T)$ix
  retRes <- retRes[idxL]
  class(retRes) <- c('pocrepath','pocre')
  return(retRes)
}
