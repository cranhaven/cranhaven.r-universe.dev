w.matrix <- function(Coords, Bandwidth, WType='Binary', family='adaptive'){
  
    Distances <- dist(Coords)
    Dij <- as.matrix(Distances)
    
    Obs <- length(Coords[,1])
    
    if(family =='adaptive' && Bandwidth>=Obs){
      Bandwidth <- Obs-1
      msg<-cat("The number of nearest neighbours exceeds the number of observations minus one.\nBandwidth set to:", Bandwidth,"\n")
      } else if(family =='adaptive' && Bandwidth==0){
          Bandwidth <- 2
          msg<-cat("The selected number of nearest neighbours is zero.\nBandwidth set to:", Bandwidth,"\n")
          }
    if(family=='fixed') {
      Dn.min <- apply(Dij, 2, FUN=function(x) {min(x[x > 0])})
      dn.max <- max(Dn.min)
      
      if (Bandwidth < dn.max){
        Dn <- apply(Dij, 2, FUN=function(x) {sum(x[x <= Bandwidth])})
        Dn.v <- as.vector(Dn)          
        nn <- sum(Dn.v==0)
        if (nn > round(0.1*Obs)) { 
          stop("More than 10% of the observations have no neighbours. Choose a higher bandwidth and try again.\n")
        } else{ msg<-cat("The bandwidth ", Bandwidth, " will result ", nn,
                         " observations without neighbours. \nTo avoid this you could set the bandwidth to at least: ", dn.max,"\n")
        }
        }
     }
        
    Wts <- matrix(data=0, nrow=Obs, ncol=Obs)
    
    for(i in 1:Obs)
      {
        #Create a dataset of the distances 
        DNeighbour <- Dij[,i]
      
        #Sort by distance
        DSorted <- sort(DNeighbour)
      
        if(family=='adaptive')
          { 
            #Keep Nearest Neighbours
            SubSet1 <- DSorted[2:(Bandwidth+1)]
          
            #Find furthest neighbour
            Kernel_H <- max(SubSet1)
          } 
        else if(family=='fixed')
              {
                Kernel_H <- Bandwidth
              }
        
      #Calculate weights
      
      for(j in 1:Obs)
        {
          if (DNeighbour[[j]] > Kernel_H)
            {
              Wts[i,j]<-0 
            }
          else
            {
              if(WType=='Bi-square')
                {
                  Wts[i,j]<-(1-(DNeighbour[[j]]/Kernel_H)^2)^2
                }
              else
                {
                  Wts[i,j]<-1
                }
          
            }
         if (j==i)
           {
              Wts[i,j]<-0
           }
        
        }
      
        if(WType=='RSBi-square')
          {
            Wts[i,] <- Wts[i,]/sum(Wts[i,])
          }
      
      }
    
      return(Wts)
}
    
  