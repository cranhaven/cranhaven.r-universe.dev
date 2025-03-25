conditional.sort <- function(Fa,Fb=NULL,Fc=NULL,R.Forward,dimA,dimB=NULL,dimC=NULL,type = 7){
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
    Hold= t(as.matrix(as.numeric(cut(Fa[t,],breaks=c(quantile(Fa[t,], probs = dimA, na.rm=T,type = type)), labels=labelsA, include.lowest=T))))
    
    z <- 1
    # Loop 2 -> every dimension of portfolio sort dimension 1 #
    #1:dimA
    # this should loop over DIM #  1:DimA
    for (q in 1:dimA.len){
      Q = which(Hold == q)

      Quantiles = Fa[ t , Q]
      Sub.Quantile = Fb[t , Q]
      Sub.sub.Quantile = Fc[t , Q]

      # Loop 3 -> dimension of sorter 2
      for (qq in 1:dimB.len){
        if (all(Fb==1)) {QQ=Sub.Quantile} else {
        QQ <- t(as.matrix(as.numeric(cut(Sub.Quantile,breaks=c(quantile(Sub.Quantile, probs = dimB,na.rm=T,type = type)), labels=labelsB, include.lowest=T))))
        }
        
        Sub.Portfolio.Tickers = which(QQ == qq)
        
        Sub.sub.Quantile2 = Sub.sub.Quantile[Sub.Portfolio.Tickers]
        
        for (qqq in 1:dimC.len){
          idx.Hold = Q
          idx.Hold = idx.Hold[Sub.Portfolio.Tickers]

          if (all(Fc==1)) {QQQ=Sub.sub.Quantile2} else {
          QQQ = t(as.matrix(as.numeric(cut(Sub.sub.Quantile2,breaks=c(quantile(Sub.sub.Quantile2, probs = dimC, na.rm=T,type = type)), labels=labelsC, include.lowest=T))))
          }
          
          Sub.Portfolio.Tickers2 = which(QQQ == qqq)
          idx.Hold = idx.Hold[Sub.Portfolio.Tickers2]
          
          Sub.Portfolio.R.Forward = mean(R.Forward[t , idx.Hold],na.rm=T)
          
          Returns.Output[t,z] <- Sub.Portfolio.R.Forward
          temp[[z]] <- idx.Hold
          z=z+1
        } 
      }
    }
    # End of time index loop #
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