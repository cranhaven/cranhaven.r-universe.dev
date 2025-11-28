moransI.v <- function(Coords, Bandwidths, x, WType='Binary', family='adaptive', plot = TRUE){
  
  k = length(Bandwidths)
  moran.table <- matrix(data=NA, nrow= k, ncol=8)
  
  colnames(moran.table) <- c("ID", "k", "Moran's I", "Expected I", "Z resampling", "P-value res.",
                             "Z randomization", "P-value rand.")
  counter <- 1

  for(b in Bandwidths){
    w <- w.matrix(Coords, b, WType, family)
    a.moran <- moransI.w(x, w)
    moran.table[counter,1] <- counter
    moran.table[counter,2]<-b
    moran.table[counter,3:8] <- unlist(a.moran)
    counter <- counter + 1
  }
  
  if (plot == TRUE) {
    if (family == 'adaptive') {
      plot(moran.table[,2], moran.table[,3], main="Global Moran's I", sub="", 
           xlab="# of neighbours", ylab="Moran's I")
    } else {
      plot(moran.table[,2], moran.table[,3], main="Global Moran's I", sub="", 
           xlab="Bandwidth", ylab="Moran's I")
    }
  }
  
  return(moran.table)
}