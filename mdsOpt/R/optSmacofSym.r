.optSmacofSym<-function(x,normalizations=NULL,distances=NULL,mdsmodels=NULL,weights=NULL,spline.degrees=c(2),outputCsv="",outputCsv2="",outDec=",",stressDigits=6,HHIDigits=2,...){
  
  if(is.null(dim(x))){
    dim(x)<-c(length(x),1)
  }
  
  eps=1e-06
  ndim=2
  itmax=1000
  
  z <- list(...)
  if(!is.null(z$eps)) eps<-z$eps
  if(!is.null(z$ndim)) ndim<-z$ndim
  if(!is.null(z$itmax)) itmax<-z$itmax
  # Data set
  x<-as.matrix(x)
  options(OutDec=outDec)
  # MDS parameters
  metnor<-c("n1","n2","n3","n3a","n4","n5","n5a","n6","n6a","n7","n8","n9","n9a","n10","n11","n12","n12a","n13")
  metscale<-c("ratio","interval","mspline","ordinal")
  metdist<-c("euclidean","manhattan","maximum","seuclidean","GDM1")
  metdistAll<-c("euclidean","manhattan","maximum","seuclidean","GDM1")
  
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
  if(!is.null(weights) && (sum(weights)!=1 || sum(weights<0)!=0 ||length(weights)!=ncol(x))){
    stop("weights should satisfy conditions: each weight takes value from [0; 1] and sum of weights eguals one")
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
  cl[["weights"]]<-NULL
  cl[["spline.degrees"]]<-NULL
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
    
    normalized<-data.Normalization(x,type=trimws(substr(metall[j],1,4)))
    if(!is.null(weights)){
      for(i in 1:ncol(normalized)){
        normalized[,i]<-normalized[,i]*weights[i]
      }
    }
    for(method  in metdistAll[1:3]){
      
      if(grepl(method,metall[j])){
      def_args <- list(delta=dist(normalized,method=method),type=type,ndim=ndim, eps=eps,itmax=itmax,spline.degree=as.integer(substr(metall[j], nchar(metall[j]), nchar(metall[j]))))    
        if(def_args[["spline.degree"]]==1)def_args[["spline.degree"]]=NULL;
        res<-do.call("smacofSym",c(cl,def_args[!names(def_args) %in% names(cl)]))
      }
    }
    method  =metdistAll[4]
    if(grepl(method,metall[j])){
      def_args <- list(delta=dist(normalized,method="euclidean")^2,type=type,ndim=ndim,spline.degree=as.integer(substr(metall[j], nchar(metall[j]), nchar(metall[j]))))
      if(def_args[["spline.degree"]]==1)def_args[["spline.degree"]]=NULL;
      res<-do.call("smacofSym",c(cl,def_args[!names(def_args) %in% names(cl)]))
    }
    method  = metdistAll[5]
    if(grepl(method,metall[j])){
      if(!is.null(weights)){
        def_args <- list(delta=dist.GDM(data.Normalization(x,type=trimws(substr(metall[j],1,4))),weightsType="different1",weights=weights),ndim=ndim,type=type,eps=eps,itmax=itmax,spline.degree=as.integer(substr(metall[j], nchar(metall[j]), nchar(metall[j]))))
      }
      else{
        def_args <- list(delta=dist.GDM(data.Normalization(x,type=trimws(substr(metall[j],1,4)))),ndim=ndim,type=type,eps=eps,itmax=itmax,spline.degree=as.integer(substr(metall[j], nchar(metall[j]), nchar(metall[j]))))
      }
      if(def_args[["spline.degree"]]==1)def_args[["spline.degree"]]=NULL;
      res<-do.call("smacofSym",c(cl,def_args[!names(def_args) %in% names(cl)]))
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
    resultsFull<-cbind(colnor,colscale,coldist,format(round(results,stressDigits),scientific = FALSE),format(round(HHI,HHIDigits),scientific = FALSE))[order(results),]
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


optSmacofSym_mMDS<-function(x,normalizations=NULL,distances=NULL,
                            mdsmodels=NULL,weights=NULL,
                            spline.degrees=c(2),
                            outputCsv="",outputCsv2="",outDec=",",
                            stressDigits=6,HHIDigits=2,...){
  metscale<-c("ratio","interval","mspline")
  if(!is.null(mdsmodels)){
    if (!is.vector(mdsmodels)) stop (paste("Optional parameter \"mdsmodels\" must be a vector containing only values from the following list: \n",paste(metscale,collapse=" ; ")))
    for (i in 1:length(mdsmodels))
    {
      if (sum(metscale==mdsmodels[i])!=1){
        stop (paste("Optional parameter \"mdsmodels\" must be a vector containing only values from the following list: \n",paste(metscale,collapse=" ; ")))
      }
    }
  }
  else{
    mdsmodels=metscale;
  }
  res<-.optSmacofSym(x=x,normalizations=normalizations,distances=distances,mdsmodels=mdsmodels,weights=weights,spline.degrees=spline.degrees,outputCsv=outputCsv,outputCsv2=outputCsv2,outDec=outDec,stressDigits=stressDigits,HHIDigits=HHIDigits,...)
  return(res)
  
}

optSmacofSym_nMDS<-function(x,normalizations=NULL,
                            distances=NULL,
                            mdsmodels=c("ordinal"),
                            weights=NULL,outputCsv="",
                            outputCsv2="",outDec=",",stressDigits=6,HHIDigits=2,...){
  metscale<-c("ordinal")
  if(!is.null(mdsmodels)){
    if (!is.vector(mdsmodels)) stop (paste("Optional parameter \"mdsmodels\" must be a vector containing only values from the following list: \n",paste(metscale,collapse=" ; ")))
    for (i in 1:length(mdsmodels))
    {
      if (sum(metscale==mdsmodels[i])!=1){
        stop (paste("Optional parameter \"mdsmodels\" must be a vector containing only values from the following list: \n",paste(metscale,collapse=" ; ")))
      }
    }
  }
  else{
    mdsmodels=metscale
  }
  res<-.optSmacofSym(x=x,normalizations=normalizations,distances=distances,mdsmodels=mdsmodels,weights=NULL,outputCsv=outputCsv,outputCsv2=outputCsv2,outDec=outDec,stressDigits=stressDigits,HHIDigits=HHIDigits,...)
  # removing last two column, not applicable for non-metric scaling
  return (res)
}
