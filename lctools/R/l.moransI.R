l.moransI<-function(Coords, Bandwidth, x, WType='Binary', scatter.plot = TRUE, family='adaptive'){

  Distances<-dist(Coords)
  Dij <- as.matrix(Distances)
  
  #Observations
  Obs<-length(x)
  
  
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
  
  
  # if(Bandwidth>=Obs){
  #   Bandwidth<-Obs-1
  #   msg<-cat("Bandwidth set to:",Bandwidth)}
    
  #Mean(x)
  mean.x=mean(x)
  sd.x=sd(x)
   
  #Inference
  n<-Obs
  m4<-sum((x - mean.x)^4)/n
  m2<-sum((x - mean.x)^2)/n
  b2<-m4/(m2^2)
 
  #Local moran
  l.moran<-matrix(data=NA,nrow=Obs,ncol=8)
  l.moran_nom<-matrix(data=0,nrow=Obs,ncol=1)
  moran_denom<-0.0
  
  Wts<-matrix(data=0,nrow=Obs,ncol=Obs)
  
  for(i in 1:Obs){    
    
    l.moran[i,6]<-(x[i] - mean.x)/sd.x
    wXj<-0.0
    
    #Get the data and add the distances 
    DataSet<-data.frame(x,DNeighbour=Dij[,i])
    
    #Sort by distance
    DataSetSorted<- DataSet[order(DataSet$DNeighbour),]
    
    if(family=='adaptive')
    { 
      #Keep Nearest Neighbours
      SubSet1 <- DataSetSorted[2:(Bandwidth+1),]
    
      #Find furthest neighbour
      Kernel_H <- max(SubSet1$DNeighbour)
    } 
    else if(family=='fixed')
    {
      Kernel_H <- Bandwidth
    }
    
    #Calculate weights
    for(j in 1:Obs){
     
      if (DataSet$DNeighbour[j] > Kernel_H){
        Wts[i,j]<-0 
        }
      else{
        if(WType=='Bi-square'){
          
          Wts[i,j]<-(1-(DataSet$DNeighbour[j]/Kernel_H)^2)^2}
        
        else{
          #Wts[i,j]<-1/Bandwidth} #/Bandwidth
          Wts[i,j]<-1} #/Bandwidth
    }
      
      if (j!=i){
        l.moran_nom[i,1]<-l.moran_nom[i,1] + (Wts[i,j]*(x[j]-mean.x))
        wXj<-wXj + (Wts[i,j]*x[j])
      }
       else{
        Wts[i,j]<-0}
       }
    l.moran[i,1]<-(x[i]-mean.x)*l.moran_nom[i,1]
    l.moran[i,7]<-wXj
    
    moran_denom<-moran_denom + ((x[i]-mean.x)*(x[i]-mean.x))
    
    #Inference
   
    #local expected I E(I)
    l.moran[i,2]<--sum(Wts[i,])/(n-1)
    
    #local Var(I)
    a1<-sum(Wts[i,]*Wts[i,])
    A1<-(n-b2)/(n-1)
    a2<-sum(sum(crossprod(Wts[i,])))
    A2<-(2*b2-n)/((n-1)*(n-2))
    
    l.moran[i,3]<-a1*A1 + a2*A2 - ((sum(Wts[i,])^2)/((n-1)^2))
    
   }
  
  Denom <-moran_denom / Obs
  
  #local I
  l.moran[,1]<-l.moran[,1]/Denom
  
  #local Z
  l.moran[,4]<-(l.moran[,1]-l.moran[,2])/sqrt(l.moran[,3])
  
  #local p
  l.moran[,5]<-2*pnorm(-abs(l.moran[,4]))
  
  mean.wXj<-mean(l.moran[,7])
  sd.wXj<-sd(l.moran[,7])
  
  l.moran[,7]<-(l.moran[,7]-mean.wXj)/ sd.wXj
  
  for(i in 1:Obs){
    if (l.moran[i,5]>0.05) {l.moran[i,8]<-0}
    else {
      if(l.moran[i,6]>0 && l.moran[i,7]>0) {l.moran[i,8]<-1}
      else{
        if(l.moran[i,6]<0 && l.moran[i,7]<0) {l.moran[i,8]<-2}
        else{
          if(l.moran[i,6]<0 && l.moran[i,7]>0) {l.moran[i,8]<-3}
          else{
            if(l.moran[i,6]>0 && l.moran[i,7]<0) {l.moran[i,8]<-4}
          }}}
    }
  }
 
  Results<-data.frame(ID=c(1:Obs),Ii=l.moran[,1], Ei=l.moran[,2],Vi=l.moran[,3], Zi=l.moran[,4],p.value=l.moran[,5], Xi=l.moran[,6],wXj=l.moran[,7], Cluster=l.moran[,8])
  
  if (scatter.plot == TRUE){
    
    xmin <- round(ifelse(abs(min(Results[,7])) > abs(min(Results[,8])), abs(min(Results[,7])), 
                       abs(min(Results[,8]))))
    xmax <- round(ifelse(abs(max(Results[,7])) > abs(max(Results[,8])), abs(max(Results[,7])), 
                       abs(max(Results[,8]))))
    xmax <-ifelse(xmin>xmax,xmin,xmax)+1
    ymax <-xmax
    xmin <- -xmax
    ymin <- -ymax
    reg1 <- lm(Results[,8]~Results[,7])
    
    plot(Results[,7], Results[,8], main="Moran's I Scatter Plot", sub="", xlab=deparse(substitute(x)), 
         ylab=paste("lagged",deparse(substitute(x)), sep=" "), xlim=c(xmin, xmax), ylim=c(ymin, ymax))
    abline(h=0)
    abline(v=0)
    abline(reg1, col="red")
  }
  
  return(Results)
}