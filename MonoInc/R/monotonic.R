monotonic <-
function(data, id.col=NULL, y.col=NULL, direction='inc')
{
  ################check monotonicity increasing/decreasing##########
  mono.tonic <- function(vector,direction)
  {
    if(direction=="inc"){
      return(all(vector==sort(vector, na.last=TRUE)))
    }else{
      return(all(vector==sort(vector, decreasing = TRUE, na.last=TRUE)))}
  }
  if(dim(data)[1]==1 || dim(data)[2]==1||is.null(dim(data))==TRUE){
    ################check monotonicity increasing##########
    vector<-as.vector(data)
    return(mono.tonic(vector,direction))
  }else{
    ids<-unique(data[,id.col])
    mono <- matrix(NA, length(ids),2)
    foreach(i=1:length(ids), .combine='rbind') %do%
    {
      x <- subset(data, data[,id.col]==ids[i])
      x <- x[,y.col]
      M <- mono.tonic(x,direction)    #check for monotonicity using a montonic function
      mono[i,] <- c(ids[i], M)
    }
    i <- NULL; rm(i)
    colnames(mono) <- c("id", "Montonic")
    return(mono)
  }
}
