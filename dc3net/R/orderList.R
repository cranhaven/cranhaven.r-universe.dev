# Order the list of interactions regarding MI values.

orderList <- function(mylist, colindx=1, decreasing=TRUE)
{
  
  if(decreasing==TRUE){ 
    yt<-as.data.frame(mylist)  
    maxM <- yt[order(yt[,colindx], decreasing=TRUE),] 
  }
  
  if(decreasing==FALSE){ 
    yt<-as.data.frame(mylist)  
    maxM <- yt[order(yt[,colindx], decreasing=FALSE),] 
  }
  return(maxM)
}