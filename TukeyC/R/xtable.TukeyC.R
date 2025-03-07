xtable.TukeyC <- function(x,...){
  
  aux1 <- x$out$Result
  aux2 <- c(x$out$MSD[2,1],rep(NA,length(x$out$Result[,1])-1))
  aux3 <- c(x$out$Sig.level,rep(NA,length(x$out$Result[,1])-1))
  aux4 <- data.frame('Means' = aux1[,1],
                      aux1[,-1],
                     'Minimum Significant Difference' = aux2,
                     'Sig.level' = aux3)

  res <- xtable::xtable(aux4,...)
  class(res) <- c('xtable.TukeyC', 'xtable', 'data.frame')
  return(res)
}
