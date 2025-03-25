unconditional.sort <- function(Fa,Fb=NULL,Fc=NULL,R.Forward,dimA,dimB=NULL,dimC=NULL,type = 7){
  input <- list(Fa,Fb,Fc,R.Forward)
  
  if (!all(unlist(lapply(input,is.xts))|unlist(lapply(input,is.null)))) {
    stop("Please only input xts or NULL objects.")
  }

  if (is.null(Fb)) {
    Fb <- matrix(data=rep(1,nrow(Fa)),nrow=nrow(Fa),ncol=ncol(Fa))
    dimB <- 0:1/1
    Fc <- matrix(data=rep(1,nrow(Fa)),nrow=nrow(Fa),ncol=ncol(Fa))
    dimC <- 0:1/1
  } else if (is.null(Fc)){
    Fc <- matrix(data=rep(1,nrow(Fa)),nrow=nrow(Fa),ncol=ncol(Fa))
    dimC <- 0:1/1
  }
  
  
  # Error warning for dimension mismatch for the factors and R.Forward
  
  if (   identical(dim(Fa), dim(Fb), dim(Fc), dim(R.Forward)) == FALSE    ){
    stop(  "Check xts matrix input dimensions - they are not equal ")
  }

  dimA.len <- length(dimA)-1
  dimB.len <- length(dimB)-1
  dimC.len <- length(dimC)-1
  
  # Error warning for dimension of portfolio sort with regard to no. of assets
  
  if ( dimA.len*dimB.len*dimC.len > ncol(R.Forward) ) {
    stop( "The dimension of the portfolio sort is larger than the number of test assets - please reduce the portfolio sort dimension")
  } 
  
  
  labelsA <- seq(1,length(dimA)-1,1)
  labelsB <- seq(1,length(dimB)-1,1)
  labelsC <- seq(1,length(dimC)-1,1)

  output <- list()  
  
  Returns.Output = matrix( ncol = (dimA.len*dimB.len*dimC.len) ,nrow = nrow(R.Forward) )
  Returns.Output <- xts(x = Returns.Output, order.by = index(R.Forward))
  Returns.Output = as.xts(Returns.Output)
  
  portfolios = matrix(list(),length(Fa[,1]),1)

  for (t in 1:nrow(Fa)){
    temp <- vector(mode = "list", length = dimA.len*dimB.len*dimC.len) 
    Hold.Fa = t(as.matrix(as.numeric(cut(Fa[t,],breaks=c(quantile(Fa[t,], probs = dimA,na.rm=T,type = type)), labels=labelsA, include.lowest=T))))
    z <- 1

    if (all(Fb==1)) {Hold.Fb = Fb[t,]} else {
      Hold.Fb = t(as.matrix(as.numeric(cut(Fb[t,],breaks=c(quantile(Fb[t,], probs = dimB,na.rm=T,type = type)), labels=labelsB, include.lowest=T))))
    }

    if (all(Fc==1)) {Hold.Fc = Fc[t,]} else {
      Hold.Fc = t(as.matrix(as.numeric(cut(Fc[t,],breaks=c(quantile(Fc[t,], probs = dimC,na.rm=T,type = type)), labels=labelsC, include.lowest=T))))
    }

    for (q in 1:dimA.len){
      for (qq in 1:dimB.len) {
        for (qqq in 1:dimC.len){
          Fa.idx <- which(Hold.Fa==q)
          Fb.idx <- which(Hold.Fb==qq)
          Fc.idx <- which(Hold.Fc==qqq)    
          
          Q = intersect(Fa.idx,Fb.idx)
          Q = intersect(Q,Fc.idx)
          if (length(Q)==0){
            Sub.Portfolio.R.Forward <- 0
          } else {
            Sub.Portfolio.R.Forward = mean(R.Forward[t , Q],na.rm=T)
          }
          Returns.Output[t,z] <- Sub.Portfolio.R.Forward
          temp[[z]]=Q
          z=z+1
        }
      }
    }
    
    portfolios[[t]] <- temp
  
  }
  
  output$returns <- Returns.Output
  output$portfolios <- portfolios
  output$tickers <- as.data.frame( colnames(R.Forward) )
  
  colnames(output$returns) = as.character( seq(1,(dimA.len*dimB.len*dimC.len),1) )
  colnames(output$portfolios) = "Portfolios"
  colnames(output$tickers) = "Index List"
  
  return(output)
}  