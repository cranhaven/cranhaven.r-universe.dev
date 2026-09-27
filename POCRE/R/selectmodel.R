selectmodel<-function(ppobj, msc=NULL)
{
  n <- ppobj[[1]]$n
  p <- ppobj[[1]]$p
  if(is.null(msc)){
    msc <- max(1-0.5*log(n)/log(p-1),0)
  }

  if(inherits(ppobj[[1]],"pocre")|inherits(ppobj[[1]],"fpocre")){
    retRes <- selectlinearmodel(ppobj,msc)
  } else if(inherits(ppobj[[1]],"gpocre")){
    retRes <- selectglmodel(ppobj,msc)
  } else{
    warning("selectmodel: The model is not supported...")
  }
  return(retRes)
}

selectlinearmodel<-function(inRes,inMSC)
{
  n <- inRes[[1]]$n
  p <- inRes[[1]]$p
  k <- matrix(0,length(inRes),1)
  loglik <- matrix(0,length(inRes),1)
  for(i in 1:length(inRes)){
    if(p==1){
    k[i] <- inRes[[i]]$effp
    loglik[i] <- inRes[[i]]$loglik
    }
  }
  if(inMSC==2){
    vmsc <- -2*loglik+2*(k+2)
    strMSC <- "AIC"
  } else if(inMSC==3){
    vmsc <- -2*loglik+2*((k*n)/(n-k-1)+2)
    strMSC <- "AICc"
  } else{
    vmsc <- -2*loglik+ (k*log(n)+2*inMSC*log(choose(p+1,k)))+2
    strMSC <- paste("EBIC(gamma=",inMSC)
  }
  idxOpt <- which(vmsc==min(vmsc))
  idxOpt <- idxOpt[1]
  print(paste("    Tuning Parameter (' strMSC '): ",inRes[[idxOpt]]$lambda))
  retRes <- inRes[[idxOpt]]
  retRes$vmsc <- vmsc[idxOpt]
  return(retRes)
}


selectglmodel<-function(inRes,inMSC)
{
  n <- inRes[[1]]$n
  p <- inRes[[1]]$p
  k <- matrix(0,length(inRes),1)
  deviance <- matrix(0,length(inRes),1)
  for(i in 1:length(inRes)){
    k[i] <- inRes[[i]]$effp
    deviance[i] <- inRes[[i]]$loglik
  }
  if(inMSC==2){
    vmsc <- deviance+2*k
    strMSC <- "AIC"
  } else if(inMSC==3){
    vmsc <- deviance + 2*k*n/(n-k-1)
    strMSC <- "AICc"
  } else{
    vmsc <- deviance+ (k*log(n)+2*inMSC*log(choose(p+1,k))) + 2
    strMSC <- paste("EBIC(gamma=",inMSC)
  }
  idxOpt <- which(vmsc==min(vmsc))
  idxOpt <- idxOpt[1]
  print(paste("    Tuning Parameter (' strMSC '): "),inRes[idxOpt]$lambda)
  retRes <- inRes[[idxOpt]]
  retRes$vmsc <- vmsc[idxOpt]
}
