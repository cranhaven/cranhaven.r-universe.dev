#Check that the MST of the current point configuration matches
#the original minimum spanning tree. This is checked within a 
#tolerance level set by tol
rgrsCheckMST <- function(Xn,EdgeLengths,i,tol){
  #EdgeLengthMST <- mst(as.matrix(dist(Xn)))$weight
  EdgeLengthMST <- spantree(as.matrix(dist(Xn)))$dist
  SEL <- sum(EdgeLengths[1:(i+1)])
  SELMST <- sum(EdgeLengthMST)
  
  if(abs(SEL - SELMST) <= tol){
    Accept <- TRUE
  }else{
    Accept <- FALSE
  }
  
  return(Accept)
}