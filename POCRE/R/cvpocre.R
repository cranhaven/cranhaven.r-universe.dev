cvpocre<-function(y, x, n.folds=10, delta=0.1, maxvar=dim(x)[1]/2,
                  ptype=c('ebtz','ebt','l1','scad','mcp'), maxit=100,
                  maxcmp=10, gamma=3.7, lambda.init=1, tol=1e-6,
                  crit=c('press','Pearson','Spearman','Kendall'))
{
  y <- as.matrix(y)
  x <- as.matrix(x)
  ptype <- ptype[1]
  crit <- crit[1]

  n <- dim(x)[1]
  p <- dim(x)[2]
  k <- dim(y)[2]
  eps <- .Machine$double.eps
  if(n!=dim(y)[1])
    stop("x and y should have the same number of rows!")

  if(n.folds<2){
    warning("  CVGpocre: The number of folds is reset to 2 ...")
    n.folds <- 2
  }

  infold <- sample(rep(1:n.folds, length.out=n))

  idxM <- 0
  lambda <- lambda.init
  cvc <- vector()

  while(idxM<=maxit){
    print(paste("    Tuning Parameter: ", lambda[length(lambda)]))

    idxM <- idxM +1
    cvc[idxM] <- 0
    nzBeta <- matrix(0,n.folds,1)
    bSparse <- matrix(1,n.folds,1)

    for(j in 1:n.folds){
      print(paste("    fold = ", j))

      #training data
      trX <- as.matrix(x[which(infold!=j),])
      trY <- as.matrix(y[which(infold!=j),])

      #test data
      tsX <- as.matrix(x[which(infold==j),])
      tsY <- as.matrix(y[which(infold==j),])
      rowtsY <- dim(tsY)[1]
      coltsY <- dim(tsY)[2]

      #temp <- pocre(trY,trX,lambda[idxM],covidx,maxvar,maxcmp,ptype,maxit,tol,gamma,pval=F)
      temp <- pocre(trY,trX,lambda[idxM],NA,maxvar,maxcmp,ptype,maxit,tol,gamma)
      tmpRes <- temp
      tmpEY <- repmat(tmpRes$mu, rowtsY, 1) + tsX %*% tmpRes$beta
      if( crit=="press"){
        cvc[idxM] <- cvc[idxM] + sum(sum((tsY-tmpEY)^2))/rowtsY
      } else if( crit=="Pearson" ){
        cvc[idxM] <- cvc[idxM] - cor(tsY, tmpEY, method = "pearson")
      } else if( crit=="Spearman" ){
        cvc[idxM] <- cvc[idxM] - cor(tsY, tmpEY, method = "Spearman")
      } else if( crit=="Kendall" ){
        cvc[idxM] <- cvc[idxM] - cor(tsY, tmpEY, method = "Kendall")
      } else{
        stop(" crit  is not supported!'")
      }
      nzBeta[j] <- sum(sum(abs(tmpRes$beta)>eps))/k
      bSparse[j] <- tmpRes$bSparse
    }
    if((lambda[length(lambda)]>=lambda.init) && (median(nzBeta)>0)){
      lambda[idxM+1] <- lambda[length(lambda)] + delta
    } else if((lambda[length(lambda)]>=lambda.init) && median(bSparse) ){
      lambda[idxM+1] <- lambda.init-delta
    } else if( (lambda[length(lambda)]<lambda.init) && median(bSparse) ){
      lambda[idxM+1] = lambda[length(lambda)] - delta
    } else{
      break
    }
  }
  retTP <- lambda[which(cvc==min(cvc))]
  retTP <- retTP[1]
  return(retTP)
}
