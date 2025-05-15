optSmacofSymInterval<-function(x,dataType="simple",normalizations=NULL
,distances=NULL,mdsmodels=NULL,spline.degrees=c(2),outputCsv="",outputCsv2="",y=NULL,outDec=",",stressDigits=6,HHIDigits=2,...){
  if(dataType=="sda"){
    x<-SO2Simple(x)
  }
  if(dataType=="separate_tables"){
    xy<-array(0,c(nrow(x),ncol(x),2))
    xy[,,1]<-x;
    xy[,,2]<-y;
    x<-xy
  }
  if(dataType=="rows"){
    xy<-array(0,c(nrow(x)/2,ncol(x),2))
    xy[,,1]<-x[seq(1,nrow(x)-1,2),];
    xy[,,2]<-x[seq(2,nrow(x),2),];
    x<-xy
  }
  if(dataType=="columns"){
    xy<-array(0,c(nrow(x)/2,ncol(x),2))
    xy[,,1]<-x[,seq(1,ncol(x)-1,2)];
    xy[,,2]<-x[,seq(2,ncol(x),2)];
    x<-xy
  }
  
  eps=1e-06
  ndim=2
  itmax=1000
  
  z <- list(...)
  if(!is.null(z$eps)) eps<-z$eps
  if(!is.null(z$ndim)) ndim<-z$ndim
  if(!is.null(z$itmax)) itmax<-z$itmax
  # Data set

  options(OutDec=outDec)
  # MDS parameters

  metnor<-c("n1","n2","n3","n3a","n4","n5","n5a","n6","n6a","n7","n8","n9","n9a","n10","n11","n12","n12a","n13")  
  metscale<-c("ratio","interval","mspline")
  metdist<-c("H_q1","H_q2","U_2_q1","U_2_q2","SO_3")
  metdistAll<-metdist
  
  if(!is.null(normalizations)){
    if (!is.vector(normalizations)) stop (paste("Optional parameter \"normalizations\" must be a vector containing only values from the following list: \n",paste(metnor,collapse=" ; ")))
    for (i in 1:length(normalizations))
    {
      if (sum(metnor==normalizations[i])!=1 && normalizations[i]!="n0"){
        stop (paste("Optional parameter \"normalizations\" must be a vector containing only values from the following list: \n",paste(metnor,collapse=" ; ")))
      }
    }
    metnor<-normalizations
  }
  if(!is.null(mdsmodels)){
    if (!is.vector(mdsmodels)) stop (paste("Optional parameter \"mdsmodels\" must be a vector containing only values from the following list: \n",paste(metscale,collapse=" ; ")))
    for (i in 1:length(mdsmodels))
    {
      if (sum(metscale==mdsmodels[i])!=1){
        stop (paste("Optional parameter \"mdsmodels\" must be a vector containing only values from the following list: \n",paste(metscale,collapse=" ; ")))
      }
    }
    metscale<-mdsmodels
  }
  if(!is.null(distances)){
    if (!is.vector(distances)) stop (paste("Optional parameter \"distances\" must be a vector containing only values from the following list: \n",paste(metdist,collapse=" ; ")))
    for (i in 1:length(distances))
    {
      if (sum(metdist==distances[i])!=1){
        stop (paste("Optional parameter \"distances\" must be a vector containing only values from the following list: \n",paste(metdist,collapse=" ; ")))
      }
    }
    metdist<-distances
  }
  
  
  colnor<-NULL
  colscale<-NULL
  coldist<-NULL
  coldegrees<-NULL
  results<-rep(0,length(metnor)*length(metscale)*length(metdist))
  HHI<-rep(0,length(metnor)*length(metscale)*length(metdist))
  R2<-rep(0,length(metnor)*length(metscale)*length(metdist))
  r<-rep(0,length(metnor)*length(metscale)*length(metdist))
  
  metall<-NULL
  
  for(a in metnor){
    for(b in metscale){
      for(c in metdist){
        if(b!="mspline"){
          degrees=c(-1)
        }
        else{
          degrees=spline.degrees
        }
        for(d in degrees){
          metall<-c(metall,paste(a," ",b," ",c," ",d))
          colnor<-c(colnor,a)
          colscale<-c(colscale,b)
          coldist<-c(coldist,c)
          if(d!=-1){
            coldegrees<-c(coldegrees,d)
          }
          else{
            coldegrees<-c(coldegrees,"")
            
          }
          
        }
      }
    }
  }
  minstress<-1e10
  minstressj<--1
  mn<-length(metall)
  cl <- as.list(match.call())[-1]
  cl[["x"]]<-NULL
  cl[["normalizations"]]<-NULL
  cl[["mdsmodels"]]<-NULL
  cl[["distances"]]<-NULL
  cl[["outputCsv"]]<-NULL
  cl[["outputCsv2"]]<-NULL
  cl[["spline.degrees"]]<-NULL
  cl[["dataType"]]<-NULL
  cl[["y"]]<-NULL
  cl[["outDec"]]<-NULL
  cl[["stressDigits"]]<-NULL
  cl[["HHIDigits"]]<-NULL
  for(j in 1:mn){
    type="ratio"
    if(grepl("interval",metall[j])){
      type="interval"
    }
    if(grepl("mspline",metall[j])){
      type="mspline"
    }
    
    for(method  in metdistAll){
      if(grepl(method,metall[j])){
        bindx<-data.Normalization(as.matrix(rbind(x[,,1],x[,,2])),type=trimws(substr(metall[j],1,4)))
        nx<-x
        nx[,,1]<-bindx[1:dim(x)[[1]],]
        nx[,,2]<-bindx[(dim(x)[[1]]+1):dim(bindx)[[1]],]
        
        if(method=="U_2_q1"){
          dd=dist.Symbolic(nx,type = "U_2",power = 1)
        }
        if(method=="U_2_q2"){
          dd=dist.Symbolic(nx,type = "U_2",power = 2)
        }
        if(method=="H_q1"){
          dd=dist.Symbolic(nx,type="H",power = 1) 
        }
        if(method=="H_q2"){
          dd=dist.Symbolic(nx,type="H",power = 2) 
        }
        if(method=="SO_3"){
          #BACKWARD COMPATIBILITY
          tryCatch({dd=do.call("dist_SDA",c(simple2SO(nx),type = "SO_3"))},error={dd=do.call("dist.SDA",c(simple2SO(nx),type = "SO_3"))})
        }
        def_args <- list(delta=dd,ndim=ndim,type=type,eps=eps,itmax=itmax,spline.degree=as.integer(substr(metall[j], nchar(metall[j]), nchar(metall[j]))))
      if(def_args[["spline.degree"]]==1)def_args[["spline.degree"]]=NULL;
      res<-do.call("smacofSym",c(cl,def_args[!names(def_args) %in% names(cl)]))
      }
    }
    model<-lm(as.vector(res$confdist)~as.vector(res$delta))
    if(minstress>res$stress){
      minstress<-res$stress
      minstressj<-j
    }
    results[j]<-res$stress
    HHI[j]<-sum((res$spp)^2)
    R2[j]<-summary.lm(model)$r.squared
    r[j]<-cor(as.vector(res$confdist),as.vector(res$delta))
  }
  if(sum(colscale=="mspline")!=0){
    resultsFull<-cbind(colnor,colscale,coldegrees,coldist,format(round(results,stressDigits),scientific = FALSE),format(round(HHI,HHIDigits),scientific = FALSE))[order(results),]
    colnames(resultsFull)<-c("Normalization method", "MDS model","Spline degree","Distance measure","STRESS 1","HHI spp")
  }
  else{
    resultsFull<-cbind(colnor,colscale,coldist,format(round(results,4),scientific = FALSE),format(round(HHI,4),scientific = FALSE))[order(results),]
    colnames(resultsFull)<-c("Normalization method", "MDS model","Distance measure","STRESS 1","HHI spp")
  }
  if(outputCsv!=""){
    write.table(resultsFull, file=outputCsv,row.names=TRUE,col.names=NA)
  }
  if(outputCsv2!=""){
    write.table(resultsFull, file=outputCsv2,sep=";",dec=",",row.names=TRUE,col.names=NA)
  }
  return (resultsFull)
}
  