`kmodes` <-
function(data,k,k2){
Len<-dim(data)
truecluster<-data[,Len[2]]
data[,Len[2]] <- -999
m<-Len[2]-1
mumean<-data[k2,1:m]
conte<-1
while(conte==1){
  indicator=1
  for (i in 1:Len[1]){
     d<-NULL
     for (j in 1:k){ 
      d<-c(d, distancem(data[i,-Len[2]],mumean[j,-Len[2]]))  
      }
      ind<-which.min(d)
      md<-min(d)
      indicator<-indicator & (ind==data[i,Len[2]])
      data[i,Len[2]]=ind
      indd<-which(data[,Len[2]]== ind)
      if(length(indd)==1){
      cluster<-data[indd,]
      mumean[ind,]=cluster[-Len[2]]
      }else{
       cluster<-data[indd,]
       mumean[ind,]=Modv(cluster)[-Len[2]]
      }
   }
   if (indicator ==1) break
}
obscluster<-data[,Len[2]]
return(obscluster)
}
