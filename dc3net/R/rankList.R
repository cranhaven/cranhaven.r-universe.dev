rankList <- function(topTest, mimCont)
{
  topTest<-as.matrix(topTest)
  
  ind<- as.integer(topTest[,4])
  ind2<- as.integer(topTest[,5])
  
  topLng <- nrow(topTest)
  probLng <- ncol(mimCont)

  # Rank the non-square mimCont matrix for topTest
  Ranked <- c() 
  tmp <- rep(0, probLng)
  
  for(i in 1: topLng){
    orindx <- order(mimCont[ind[i],],decreasing=T)  
    tmp[orindx] <-c(1: probLng) 
    Ranked <- append(Ranked,tmp[ind2[i]] )
  }   
  RankedC1 <- Ranked

  Ranked <- c()
  ind<- as.integer(topTest[,5])
  ind2<- as.integer(topTest[,4])
  
  # Rank the non-square mimCont matrix for topTest
  Ranked <- c() 
  tmp <- rep(0, probLng)
  
  for(i in 1: topLng){
    orindx <- order(mimCont[ind[i],],decreasing=T)  
    tmp[orindx] <-c(1: probLng) 
    Ranked <- append(Ranked,tmp[ind2[i]] )
  }   
  RankedC2 <- Ranked
  rm(Ranked)
  return(cbind(RankedC1,RankedC2))
}
