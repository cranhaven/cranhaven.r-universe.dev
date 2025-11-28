mvstest <-
function(sdata,X,stn,gn, status,time,quant=c("No",-0.2,0.2)){

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
  if(length(X)==0){
    stop("no covariates are given")
  }else if(length(X)==1){

  res<-matrix(NA,nrow = ncol(sdata),ncol = 10)
  #cbdata<-cbind(datag,proteinx)
  for(i in sn:ln){
    #print(proteinx[[i]])}
    res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)

  }
  res1<-cbind(as.data.frame(colnames(sdata)),res)

  colnames(res1)<-c(
    "Gene", "gene.hazard.risk", "gene.hazard.rate",
    "gene.SE", "gene.z-score", "gene.pvalue",
    paste(X, "Hazard.risk", sep = "."),
    paste(X, "Hazard.rate", sep = "."),
    paste(X, "SE", sep = "."),
    paste(X, "z-score", sep = "."),
    paste(X, "pvalue", sep = ".")
    )
  }else if(length(X)==2){
    res<-matrix(NA,nrow=ncol(sdata),ncol = 15)
    for(i in sn:ln){
     
      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)
      

    }
    res1<-cbind(as.data.frame(colnames(sdata)),res)

    colnames(res1)<-c(
      "Gene","gene.hazard.risk","gene.hazard.rate", 
      "gene.SE","gene.z-score", "gene.pvalue",  
      paste(X[1],"Hazard.risk",sep = "."),
      paste(X[1],"Hazard.rate",sep = "."), 
      paste(X[1],"SE",sep = "."),
      paste(X[1],"z-score",sep = "."),
      paste(X[1],"pvalue",sep = "."),
      paste(X[2],"Hazard.risk",sep = "."),
      paste(X[2],"Hazard.rate",sep = "."), 
      paste(X[2],"SE",sep = "."),
      paste(X[2],"z-score",sep = "."),
      paste(X[2],"pvalue",sep = ".")
      )

  }else if(length(X)==3){
    res<-matrix(NA,nrow = ncol(sdata),ncol = 20)
    for(i in sn:ln){
      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)

    }
    res1<-cbind(as.data.frame(colnames(sdata)),res)

    colnames(res1)<-c(
      "Gene","gene.hazard.risk","gene.hazard.rate", 
      "gene.SE","gene.z-score", "gene.pvalue",  
      paste(X[1],"Hazard.risk",sep = "."),
      paste(X[1],"Hazard.rate",sep = "."), 
      paste(X[1],"SE",sep = "."),
      paste(X[1],"z-score",sep = "."),
      paste(X[1],"pvalue",sep = "."),
      paste(X[2],"Hazard.risk",sep = "."),
      paste(X[2],"Hazard.rate",sep = "."), 
      paste(X[2],"SE",sep = "."),
      paste(X[2],"z-score",sep = "."),
      paste(X[2],"pvalue",sep = "."),
      paste(X[3],"hazad.risk",sep = "."),
      paste(X[3],"hazard.rate",sep ="."),
      paste(X[3],"SE",sep = "."),
      paste(X[3],"z-score",sep = "."),
      paste(X[3],"pvalue",sep = ".")
    )
  }else if(length(X)==4) {
    res=matrix(NA,nrow = ncol(sdata),ncol = 25)

    for(i in sn:ln){

      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)

    }
    res1<-cbind(as.data.frame((colnames(sdata))),res)
    colnames(res1)<-c(
      "Gene", "gene.hazard.risk", "gene.hazard.rate",
      "gene.SE", "gene.z-score", "gene.pvalue",
      paste(X[1], "Hazard.risk", sep = "."),
      paste(X[1], "Hazard.rate", sep = "."),
      paste(X[1], "SE", sep = "."),
      paste(X[1], "z-score", sep = "."),
      paste(X[1], "pvalue", sep = "."),
      paste(X[2], "Hazard.risk", sep = "."),
      paste(X[2], "Hazard.rate", sep = "."),
      paste(X[2], "SE", sep = "."),
      paste(X[2], "z-score", sep = "."),
      paste(X[2], "pvalue", sep = "."),
      paste(X[3], "hazad.risk", sep = "."),
      paste(X[3], "hazard.rate", sep = "."),
      paste(X[3], "SE", sep = "."),
      paste(X[3], "z-score", sep = "."),
      paste(X[3], "pvalue", sep = "."),
      paste(X[4], "hazad.risk", sep = "."),
      paste(X[4], "hazard.rate", sep = "."),
      paste(X[4], "SE", sep = "."),
      paste(X[4], "z-score", sep = "."),
      paste(X[4], "pvalue", sep = ".")
      )    
  }else if(length(X)==5){
    res=matrix(NA,nrow = ncol(sdata),ncol = 30)
    
    for(i in sn:ln){
      
      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)
      
    }
    res1<-cbind(as.data.frame((colnames(sdata))),res)
    colnames(res1)<-c(
      "Gene", "gene.hazard.risk", "gene.hazard.rate",
      "gene.SE", "gene.z-score", "gene.pvalue",
      paste(X[1], "Hazard.risk", sep = "."),
      paste(X[1], "Hazard.rate", sep = "."),
      paste(X[1], "SE", sep = "."),
      paste(X[1], "z-score", sep = "."),
      paste(X[1], "pvalue", sep = "."),
      paste(X[2], "Hazard.risk", sep = "."),
      paste(X[2], "Hazard.rate", sep = "."),
      paste(X[2], "SE", sep = "."),
      paste(X[2], "z-score", sep = "."),
      paste(X[2], "pvalue", sep = "."),
      paste(X[3], "hazad.risk", sep = "."),
      paste(X[3], "hazard.rate", sep = "."),
      paste(X[3], "SE", sep = "."),
      paste(X[3], "z-score", sep = "."),
      paste(X[3], "pvalue", sep = "."),
      paste(X[4], "hazad.risk", sep = "."),
      paste(X[4], "hazard.rate", sep = "."),
      paste(X[4], "SE", sep = "."),
      paste(X[4], "z-score", sep = "."),
      paste(X[4], "pvalue", sep = "."),
      paste(X[5], "hazad.risk", sep = "."),
      paste(X[5], "hazard.rate", sep = "."),
      paste(X[5], "SE", sep = "."),
      paste(X[5], "z-score", sep = "."),
      paste(X[5], "pvalue", sep = ".")
      )
       
  }else if(length(X)==6){
    res=matrix(NA,nrow = ncol(sdata),ncol = 35)
    
    for(i in sn:ln){
      
      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)
      
    }
    res1<-cbind(as.data.frame((colnames(sdata))),res)
    colnames(res1)<-c(
      "Gene","gene.hazard.risk","gene.hazard.rate", 
      "gene.SE","gene.z-score", "gene.pvalue",  
      paste(X[1],"Hazard.risk",sep = "."),
      paste(X[1],"Hazard.rate",sep = "."), 
      paste(X[1],"SE",sep = "."),
      paste(X[1],"z-score",sep = "."),
      paste(X[1],"pvalue",sep = "."),
      paste(X[2],"Hazard.risk",sep = "."),
      paste(X[2],"Hazard.rate",sep = "."), 
      paste(X[2],"SE",sep = "."),
      paste(X[2],"z-score",sep = "."),
      paste(X[2],"pvalue",sep = "."),
      paste(X[3],"hazad.risk",sep = "."),
      paste(X[3],"hazard.rate",sep ="."),
      paste(X[3],"SE",sep = "."),
      paste(X[3],"z-score",sep = "."),
      paste(X[3],"pvalue",sep = "."),
      paste(X[4],"hazad.risk",sep = "."),
      paste(X[4],"hazard.rate",sep ="."),
      paste(X[4],"SE",sep = "."),
      paste(X[4],"z-score",sep = "."),
      paste(X[4],"pvalue",sep = "."),
      paste(X[5],"hazad.risk",sep = "."),
      paste(X[5],"hazard.rate",sep ="."),
      paste(X[5],"SE",sep = "."),
      paste(X[5],"z-score",sep = "."),
      paste(X[5],"pvalue",sep = "."),
      paste(X[6],"hazad.risk",sep = "."),
      paste(X[6],"hazard.rate",sep ="."),
      paste(X[6],"SE",sep = "."),
      paste(X[6],"z-score",sep = "."),
      paste(X[6],"pvalue",sep = "."))

  }else if(length(X)==7){
    res=matrix(NA,nrow = ncol(sdata),ncol = 40)
    
    for(i in sn:ln){
      
      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)
      
    }
    res1<-cbind(as.data.frame((colnames(sdata))),res)
    colnames(res1)<-c(
      "Gene","gene.hazard.risk","gene.hazard.rate", 
      "gene.SE","gene.z-score", "gene.pvalue", 
      paste(X[1],"Hazard.risk",sep = "."),
      paste(X[1],"Hazard.rate",sep = "."), 
      paste(X[1],"SE",sep = "."),
      paste(X[1],"z-score",sep = "."),
      paste(X[1],"pvalue",sep = "."),
      paste(X[2],"Hazard.risk",sep = "."),
      paste(X[2],"Hazard.rate",sep = "."), 
      paste(X[2],"SE",sep = "."),
      paste(X[2],"z-score",sep = "."),
      paste(X[2],"pvalue",sep = "."),
      paste(X[3],"hazad.risk",sep = "."),
      paste(X[3],"hazard.rate",sep ="."),
      paste(X[3],"SE",sep = "."),
      paste(X[3],"z-score",sep = "."),
      paste(X[3],"pvalue",sep = "."),
      paste(X[4],"hazad.risk",sep = "."),
      paste(X[4],"hazard.rate",sep ="."),
      paste(X[4],"SE",sep = "."),
      paste(X[4],"z-score",sep = "."),
      paste(X[4],"pvalue",sep = "."),   
      paste(X[5],"hazad.risk",sep = "."),
      paste(X[5],"hazard.rate",sep ="."),
      paste(X[5],"SE",sep = "."),
      paste(X[5],"z-score",sep = "."),
      paste(X[5],"pvalue",sep = "."),
      paste(X[6],"hazad.risk",sep = "."),
      paste(X[6],"hazard.rate",sep ="."),
      paste(X[6],"SE",sep = "."),
      paste(X[6],"z-score",sep = "."),
      paste(X[6],"pvalue",sep = "."),  
      paste(X[7],"hazad.risk",sep = "."),
      paste(X[7],"hazard.rate",sep ="."),
      paste(X[7],"SE",sep = "."),
      paste(X[7],"z-score",sep = "."),
      paste(X[7],"pvalue",sep = ".")                      
    )  

    
  }else if(length(X)==8){
    res=matrix(NA,nrow = ncol(sdata),ncol = 45)
    
    for(i in sn:ln){
      
      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)
      
    }
    res1<-cbind(as.data.frame((colnames(sdata))),res)
    colnames(res1)<-c(
      "Gene","gene.hazard.risk","gene.hazard.rate", 
      "gene.SE","gene.z-score", "gene.pvalue",  
      paste(X[1],"Hazard.risk",sep = "."),
      paste(X[1],"Hazard.rate",sep = "."), 
      paste(X[1],"SE",sep = "."),
      paste(X[1],"z-score",sep = "."),
      paste(X[1],"pvalue",sep = "."),
      paste(X[2],"Hazard.risk",sep = "."),
      paste(X[2],"Hazard.rate",sep = "."), 
      paste(X[2],"SE",sep = "."),
      paste(X[2],"z-score",sep = "."),
      paste(X[2],"pvalue",sep = "."),
      paste(X[3],"hazad.risk",sep = "."),
      paste(X[3],"hazard.rate",sep ="."),
      paste(X[3],"SE",sep = "."),
      paste(X[3],"z-score",sep = "."),
      paste(X[3],"pvalue",sep = "."),
      paste(X[4],"hazad.risk",sep = "."),
      paste(X[4],"hazard.rate",sep ="."),
      paste(X[4],"SE",sep = "."),
      paste(X[4],"z-score",sep = "."),
      paste(X[4],"pvalue",sep = "."),   
      paste(X[5],"hazad.risk",sep = "."),
      paste(X[5],"hazard.rate",sep ="."),
      paste(X[5],"SE",sep = "."),
      paste(X[5],"z-score",sep = "."),
      paste(X[5],"pvalue",sep = "."),
      paste(X[6],"hazad.risk",sep = "."),
      paste(X[6],"hazard.rate",sep ="."),
      paste(X[6],"SE",sep = "."),
      paste(X[6],"z-score",sep = "."),
      paste(X[6],"pvalue",sep = "."),  
      paste(X[7],"hazad.risk",sep = "."),
      paste(X[7],"hazard.rate",sep ="."),
      paste(X[7],"SE",sep = "."),
      paste(X[7],"z-score",sep = "."),
      paste(X[7],"pvalue",sep = "."), 
      paste(X[8],"hazad.risk",sep = "."),
      paste(X[8],"hazard.rate",sep ="."),
      paste(X[8],"SE",sep = "."),
      paste(X[8],"z-score",sep = "."),
      paste(X[8],"pvalue",sep = ".")) 
 
    
  }else if(length(X)==9){
    res=matrix(NA,nrow = ncol(sdata),ncol = 50)
    
    for(i in sn:ln){
      
      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)
      
    }
    res1<-cbind(as.data.frame((colnames(sdata))),res)
    colnames(res1)<-c(
      "Gene","gene.hazard.risk","gene.hazard.rate", 
      "gene.SE","gene.z-score", "gene.pvalue",  
      paste(X[1],"Hazard.risk",sep = "."),
      paste(X[1],"Hazard.rate",sep = "."), 
      paste(X[1],"SE",sep = "."),
      paste(X[1],"z-score",sep = "."),
      paste(X[1],"pvalue",sep = "."),
      paste(X[2],"Hazard.risk",sep = "."),
      paste(X[2],"Hazard.rate",sep = "."), 
      paste(X[2],"SE",sep = "."),
      paste(X[2],"z-score",sep = "."),
      paste(X[2],"pvalue",sep = "."),
      paste(X[3],"hazad.risk",sep = "."),
      paste(X[3],"hazard.rate",sep ="."),
      paste(X[3],"SE",sep = "."),
      paste(X[3],"z-score",sep = "."),
      paste(X[3],"pvalue",sep = "."),
      paste(X[4],"hazad.risk",sep = "."),
      paste(X[4],"hazard.rate",sep ="."),
      paste(X[4],"SE",sep = "."),
      paste(X[4],"z-score",sep = "."),
      paste(X[4],"pvalue",sep = "."),  
      paste(X[5],"hazad.risk",sep = "."),
      paste(X[5],"hazard.rate",sep ="."),
      paste(X[5],"SE",sep = "."),
      paste(X[5],"z-score",sep = "."),
      paste(X[5],"pvalue",sep = "."),
      paste(X[6],"hazad.risk",sep = "."),
      paste(X[6],"hazard.rate",sep ="."),
      paste(X[6],"SE",sep = "."),
      paste(X[6],"z-score",sep = "."),
      paste(X[6],"pvalue",sep = "."),  
      paste(X[7],"hazad.risk",sep = "."),
      paste(X[7],"hazard.rate",sep ="."),
      paste(X[7],"SE",sep = "."),
      paste(X[7],"z-score",sep = "."),
      paste(X[7],"pvalue",sep = "."), 
      paste(X[8],"hazad.risk",sep = "."),
      paste(X[8],"hazard.rate",sep ="."),
      paste(X[8],"SE",sep = "."),
      paste(X[8],"z-score",sep = "."),
      paste(X[8],"pvalue",sep = "."),
      paste(X[9],"hazad.risk",sep = "."),
      paste(X[9],"hazard.rate",sep ="."),
      paste(X[9],"SE",sep = "."),
      paste(X[9],"zscore",sep = "."),
      paste(X[9],"pvalue",sep = ".")
    )  


    
  }
  else if(length(X)==10){
    res=matrix(NA,nrow = ncol(sdata),ncol = 55)
    
    for(i in sn:ln){
      
      res[i,]<-MVKMresult(data=sdata,X=X,mol=i,status=status,time=time,quant=quant)
      
    }
    res1<-cbind(as.data.frame((colnames(sdata))),res)
    colnames(res1)<-c(
      "Gene","gene.hazard.risk","gene.hazard.rate", 
      "gene.SE","gene.z-score", "gene.pvalue",  
      paste(X[1],"Hazard.risk",sep = "."),
      paste(X[1],"Hazard.rate",sep = "."), 
      paste(X[1],"SE",sep = "."),
      paste(X[1],"z-score",sep = "."),
      paste(X[1],"pvalue",sep = "."),
      paste(X[2],"Hazard.risk",sep = "."),
      paste(X[2],"Hazard.rate",sep = "."), 
      paste(X[2],"SE",sep = "."),
      paste(X[2],"z-score",sep = "."),
      paste(X[2],"pvalue",sep = "."),
      paste(X[3],"hazad.risk",sep = "."),
      paste(X[3],"hazard.rate",sep ="."),
      paste(X[3],"SE",sep = "."),
      paste(X[3],"z-score",sep = "."),
      paste(X[3],"pvalue",sep = "."),
      paste(X[4],"hazad.risk",sep = "."),
      paste(X[4],"hazard.rate",sep ="."),
      paste(X[4],"SE",sep = "."),
      paste(X[4],"z-score",sep = "."),
      paste(X[4],"pvalue",sep = "."),   
      paste(X[5],"hazad.risk",sep = "."),
      paste(X[5],"hazard.rate",sep ="."),
      paste(X[5],"SE",sep = "."),
      paste(X[5],"z-score",sep = "."),
      paste(X[5],"pvalue",sep = "."),
      paste(X[6],"hazad.risk",sep = "."),
      paste(X[6],"hazard.rate",sep ="."),
      paste(X[6],"SE",sep = "."),
      paste(X[6],"z-score",sep = "."),
      paste(X[6],"pvalue",sep = "."),  
      paste(X[7],"hazad.risk",sep = "."),
      paste(X[7],"hazard.rate",sep ="."),
      paste(X[7],"SE",sep = "."),
      paste(X[7],"z-score",sep = "."),
      paste(X[7],"pvalue",sep = "."), 
      paste(X[8],"hazad.risk",sep = "."),
      paste(X[8],"hazard.rate",sep ="."),
      paste(X[8],"SE",sep = "."),
      paste(X[8],"z-score",sep = "."),
      paste(X[8],"pvalue",sep = "."),
      paste(X[9],"hazad.risk",sep = "."),
      paste(X[9],"hazard.rate",sep ="."),
      paste(X[9],"SE",sep = "."),
      paste(X[9],"zscore",sep = "."),
      paste(X[9],"pvalue",sep = "."),
      paste(X[10],"hazad.risk",sep = "."),
      paste(X[10],"hazard.rate",sep ="."),
      paste(X[10],"SE",sep = "."),
      paste(X[10],"z-score",sep = "."),
      paste(X[10],"pvalue",sep = ".")
    )    
  }
  res2<-res1[c(sn:ln),]
  return(res2)
}
