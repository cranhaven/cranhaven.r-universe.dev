musvtest <-
function(sdata,stn,gn,time="month",status="status",quant=c("No",-0.2,0.2)){

  colnms<-tolower(colnames(sdata))
  
  if(is.character(stn)){
    kind<-is.element(colnms,tolower(stn)) 
    sn<-which(kind==TRUE)
  }else if(is.numeric(stn)){
    sn<-stn
  }
  
  if(is.character(gn)){
    kind<-is.element(colnms,tolower(gn)) 
    ln<-which(kind==TRUE)
  }else if(is.numeric(gn)){
    ln<-gn
  }
  
  #datag<-rbind(data[,1:5],data[,1:5],data[,1:5],data[,1:5])

  res=matrix(NA,nrow = ncol(sdata), ncol = 5)
  #cbdata<-cbind(datag,proteinx)
  for(i in stn:ln){
    #print(proteinx[[i]])}
    res[i,]<-SKMCresult(data=sdata,mol=i, time=time,status=status,quant=quant)

  }
  res1<-cbind(as.data.frame(colnames(sdata)),res)
  colnames(res1)<-c("Gene","Hazad risk","hazard rate","standard error","z-value","p-value")
  res2<-res1[c(stn:ln),]
  return(res2)
}
