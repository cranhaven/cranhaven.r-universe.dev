
GenoScan.prelim<-function(Y, X=NULL, id=NULL, out_type="C", B=5000){
  ##Preliminary
  Y<-as.matrix(Y);n<-nrow(Y)

  if(length(X)!=0){X0<-svd(as.matrix(X))$u}else{X0<-NULL}
  X0<-cbind(rep(1,n),X0)

  if(out_type=="C"){nullglm<-glm(Y~0+X0,family=gaussian)}
  if(out_type=="D"){nullglm<-glm(Y~0+X0,family=binomial)}

  if (length(id)==0){id<-1:n}

  mu<-nullglm$fitted.values;Y.res<-Y-mu
  #permute the residuals
  index<-sapply(1:B,function(x)sample(1:length(Y)));temp.Y.res<-Y.res[as.vector(index)]
  re.Y.res<-matrix(temp.Y.res,length(Y),B)

  #prepare the preliminary features
  result.prelim<-list(Y=Y,id=id,n=n,X0=X0,nullglm=nullglm,out_type=out_type,re.Y.res=re.Y.res)
  return(result.prelim)
}

GenoScan.Region<-function(result.prelim,G,pos,Gsub.id=NULL,Z=NULL,MAF.weights='beta',test='combined',window.size=c(5000,10000,15000,20000,25000,50000),MAF.threshold=1,impute.method='fixed'){
  #load preliminary features
  Y<-result.prelim$Y;X0<-result.prelim$X0
  n<-result.prelim$n;id<-result.prelim$id
  nullglm<-result.prelim$nullglm;out_type<-result.prelim$out_type
  mu<-nullglm$fitted.values;Y.res<-Y-mu
  re.Y.res<-result.prelim$re.Y.res
  n<-nrow(Y);p<-ncol(G);X0<-svd(X0)$u
  if(length(Z)==0){Z<-as.matrix(rep(1,ncol(G)))}else{
    Z<-cbind(rep(1,ncol(G)),Z)
  }

  #match phenotype id and genotype id
  if(length(Gsub.id)==0){match.index<-1:nrow(G)}else{
    match.index<-match(result.prelim$id,Gsub.id)
  }
  if(mean(is.na(match.index))>0){
    msg<-sprintf("Some individuals are not matched with genotype. The rate is%f", mean(is.na(match.index)))
    warning(msg,call.=F)
  }
  G<-G[match.index,]
  # missing genotype imputation
  G[G==9]<-NA
  N_MISS<-sum(is.na(G))
  if(N_MISS>0){
    msg<-sprintf("The missing genotype rate is %f. Imputation is applied.", N_MISS/nrow(G)/ncol(G))
    warning(msg,call.=F)
    G<-Impute(G,impute.method)
  }

  # genotype
  G<-as.matrix(G);center.G<-t(t(G)-colMeans(G))
  MAF<-colMeans(G)/2;MAF[is.na(MAF)]<-0
  MAF[MAF>0.5]<-1-MAF[MAF>0.5]
  index<-which(colMeans(center.G^2)!=0 & MAF<MAF.threshold)
  G<-as.matrix(G[,index])
  pos<-pos[index]
  Z<-matrix(Z[index,],length(index),ncol(Z))
  MAF<-MAF[index]
  G<-Matrix(G,sparse=T)

  if(length(index)==0){
    msg<-sprintf("No variant has variation, return NA")
    warning(msg,call.=F)
    p.value<-NA
    return(list(p.value=p.value))
  }

  if(MAF.weights=='beta'){weights<-dbeta(MAF,1,25)}
  if(MAF.weights=='equal'){weights<-rep(1,length(MAF))}#rep(1,ncol(SNP.set))
  Z<-weights*Z

  #calculate score statistics and resampling based score statistics
  mu<-nullglm$fitted.values;Y.res<-Y-mu
  score<-t(G)%*%Y.res
  re.score<-t(t(G)%*%re.Y.res)
  score<-as.matrix(score);re.score<-as.matrix(re.score)

  #generate a matrix to specify the variants in each window
  window.matrix0<-c()
  for(size in window.size){
    if (size==1){next}
    #pos.tag<-pos
    pos.tag<-seq(min(pos),max(pos),by=size*1/2)
    pos.tag<-sapply(pos.tag,function(x)pos[which.min(abs(x-pos))])
    window.matrix0<-cbind(window.matrix0,sapply(pos.tag,function(x)as.numeric(pos>=x & pos<x+size)))
  }
  if(length(window.matrix0)!=0){
    #merge identical windows to get actual windows (start and end with variant position)
    window.string<-apply(window.matrix0,2,function(x)paste(as.character(x),collapse = ""))
    window.matrix0<-as.matrix(window.matrix0[,match(unique(window.string),window.string)])

    if(1 %in% window.size){window.matrix0<-as.matrix(window.matrix0[,apply(window.matrix0,2,sum)>1])} #single variants will be added back later
    #incorperate weights (MAF/annotations)
    window.matrix<-c()
    for(i in 1:ncol(Z)){window.matrix<-cbind(window.matrix,Z[,i]*window.matrix0)}
    #calculate scan statistics for all windows
    window.summary<-t(apply(window.matrix0,2,function(x)c(min(pos[which(x==1)]),max(pos[which(x==1)]))))
    if(test=='dispersion'){
      score.window<-as.vector(t(score^2)%*%window.matrix^2)
      re.score.window<-re.score^2%*%window.matrix^2
      #calculate p-values for all windows
      all.p<-Get.p(rbind(score.window,re.score.window),re.score.window)
      p.window<-all.p[1,]
      window.summary<-cbind(window.summary,matrix(all.p[1,],nrow(window.summary),ncol(Z)))
    }
    if(test=='burden'){
      score.window<-as.vector(t(score)%*%window.matrix)^2
      re.score.window<-(re.score%*%window.matrix)^2
      #calculate p-values for all windows
      all.p<-Get.p(rbind(score.window,re.score.window),re.score.window)
      p.window<-all.p[1,]
      window.summary<-cbind(window.summary,matrix(all.p[1,],nrow(window.summary),ncol(Z)))
    }
    if(test=='combined'){
      score.window<-as.vector(t(score^2)%*%window.matrix^2)
      re.score.window<-re.score^2%*%window.matrix^2
      #calculate p-values for all windows
      all.p<-Get.p(rbind(score.window,re.score.window),re.score.window)

      score.window<-as.vector(t(score)%*%window.matrix)^2
      re.score.window<-(re.score%*%window.matrix)^2
      #calculate p-values for all windows
      all.p<-cbind(all.p,Get.p(rbind(score.window,re.score.window),re.score.window))

      window.summary<-cbind(window.summary,matrix(all.p[1,],nrow(window.summary),ncol(Z)*2))
    }
  }else{all.p<-NULL;window.summary<-NULL}

  #add single base pair windows
  if(1 %in% window.size){
    msg<-sprintf('single base pair windows are included, large sample size is required for accurate results')
    warning(msg,call.=F)
  }
  re.p<-as.matrix(all.p[-1,]) # resampled p-values
  temp.p<-all.p[1,] # p-values

  #calculate minimum p-values
  index<-which(!is.na(temp.p))
  minP<-min(temp.p[index]);
  re.minP<-as.matrix(apply(as.matrix(re.p[,index]),1,min,na.rm=T))

  if(length(index)==1){
    msg<-sprintf("Only one window has variation, test reduces to 1 df. chisq test")
    warning(msg,call.=F)
    p.value<-temp.p[index]
    return(list(score=score,re.score=re.score,n.marker=p,window.summary=window.summary,minP=minP,re.minP=re.minP,M=1,opt.window=window.summary[1:2],p.value=p.value))
  }else{
    p.value<-Get.Gumbel.p(-log(minP),-log(re.minP)) #moment matching based p-value
  }
  #Get number of independent tests
  M<-Get.Gumbel.M(-log(re.minP))

  if(nrow(window.summary)==1){sig.summary<-window.summary[min(window.summary[,-(1:2)],na.rm=T)<0.05/M,]
  }else{sig.summary<-window.summary[apply(as.matrix(window.summary[,-(1:2)]),1,min,na.rm=T)<0.05/M,]}

  return(list(n.marker=ncol(G),window.summary=window.summary,M=M,threshold=0.05/M,p.value=p.value))
}

GenoScan.SingleWindow<-function(result.prelim,G,Gsub.id=NULL,Z=NULL,MAF.weights='beta',test='combined',MAF.threshold=1,impute.method='fixed'){
  #load preliminary features
  Y<-result.prelim$Y;X0<-result.prelim$X0
  n<-result.prelim$n;id<-result.prelim$id
  nullglm<-result.prelim$nullglm;out_type<-result.prelim$out_type
  mu<-nullglm$fitted.values;Y.res<-Y-mu
  re.Y.res<-result.prelim$re.Y.res
  n<-nrow(Y);p<-ncol(G);X0<-svd(X0)$u
  if(length(Z)==0){Z<-as.matrix(rep(1,ncol(G)))}else{
    Z<-cbind(rep(1,ncol(G)),Z)
  }

  #match phenotype id and genotype id
  if(length(Gsub.id)==0){match.index<-1:nrow(G)}else{
    match.index<-match(result.prelim$id,Gsub.id)
  }
  if(mean(is.na(match.index))>0){
    msg<-sprintf("Some individuals are not matched with genotype. The rate is%f", mean(is.na(match.index)))
    warning(msg,call.=F)
  }
  G<-G[match.index,]
  # missing genotype imputation
  G[G==9]<-NA
  N_MISS<-sum(is.na(G))
  if(N_MISS>0){
    msg<-sprintf("The missing genotype rate is %f. Imputation is applied.", N_MISS/nrow(G)/ncol(G))
    warning(msg,call.=F)
    G<-Impute(G,impute.method)
  }

  # genotype
  G<-as.matrix(G);center.G<-t(t(G)-colMeans(G))
  MAF<-colMeans(G)/2;MAF[is.na(MAF)]<-0
  MAF[MAF>0.5]<-1-MAF[MAF>0.5]
  index<-which(colMeans(center.G^2)!=0 & MAF<MAF.threshold)
  G<-as.matrix(G[,index])
  pos<-pos[index]
  Z<-matrix(Z[index,],length(index),ncol(Z))
  MAF<-MAF[index]
  G<-Matrix(G,sparse=T)

  if(MAF.weights=='beta'){weights<-dbeta(MAF,1,25)}
  if(MAF.weights=='equal'){weights<-rep(1,length(MAF))}#rep(1,ncol(SNP.set))
  Z<-weights*Z

  #calculate score statistics and resampling based score statistics
  mu<-nullglm$fitted.values;Y.res<-Y-mu
  score<-t(G)%*%Y.res
  re.score<-t(t(G)%*%re.Y.res)
  score<-as.matrix(score);re.score<-as.matrix(re.score)

  window.matrix<-c()
  for(i in 1:ncol(Z)){window.matrix<-cbind(window.matrix,Z[,i])}

  if(length(index)==0){
    msg<-sprintf("No variant has variation, return NA")
    warning(msg,call.=F)
    if(test=='dispersion'|test=='burden'){p.value<-rep(NA,ncol(Z))}
    if(test=='combined'){p.value<-rep(NA,ncol(Z)*2)}
    return(list(p.value=p.value))
  }

  if(test=='dispersion'){
    score.window<-as.vector(t(score^2)%*%window.matrix^2)
    re.score.window<-re.score^2%*%window.matrix^2
    #calculate p-values for all windows
    all.p<-Get.p(rbind(score.window,re.score.window),re.score.window)
  }
  if(test=='burden'){
    score.window<-as.vector(t(score)%*%window.matrix)^2
    re.score.window<-(re.score%*%window.matrix)^2
    #calculate p-values for all windows
    all.p<-Get.p(rbind(score.window,re.score.window),re.score.window)
  }
  if(test=='combined'){
    score.window<-as.vector(t(score^2)%*%window.matrix^2)
    re.score.window<-re.score^2%*%window.matrix^2
    #calculate p-values for all windows
    all.p<-Get.p(rbind(score.window,re.score.window),re.score.window)

    score.window<-as.vector(t(score)%*%window.matrix)^2
    re.score.window<-(re.score%*%window.matrix)^2
    #calculate p-values for all windows
    all.p<-cbind(all.p,Get.p(rbind(score.window,re.score.window),re.score.window))
  }
  window.p<-all.p[1,] # p-values
  re.p<-as.matrix(all.p[-1,]) # resampled p-values

  minP<-min(window.p[index],na.rm=T);
  re.minP<-as.matrix(apply(as.matrix(re.p),1,min,na.rm=T))

  return(list(n.marker=ncol(G),p.value=as.numeric(window.p)))
}


#cauculated p-values using resampled test statistics
Get.p<-function(Q,re.Q){ #Q a A*q matrix of test statistics, re.Q a B*q matrix of resampled test statistics
  re.mean<-apply(re.Q,2,mean)
  re.variance<-apply(re.Q,2,var)
  re.kurtosis<-apply((t(re.Q)-re.mean)^4,1,mean)/re.variance^2-3
  re.df<-(re.kurtosis>0)*12/re.kurtosis+(re.kurtosis<=0)*100000
  re.p<-t(pchisq((t(Q)-re.mean)*sqrt(2*re.df)/sqrt(re.variance)+re.df,re.df,lower.tail=F))
  return(re.p)
}

Get.Gumbel.p<-function(Q,re.Q){ #Q a A*q matrix of test statistics, re.Q a B*q matrix of resampled test statistics
  re.mean<-apply(re.Q,2,mean)
  re.variance<-apply(re.Q,2,var)
  beta<-sqrt(re.variance*6/pi^2)
  mu<-re.mean-beta*0.5772156649 #Euler Mascheroni constant

  upperGumbel<-function(x,mu,beta){
    p<-1-exp(-exp(-(x-mu)/beta))
    p[p==0]<-exp(-(x-mu)/beta)[p==0]
    return(p)
  }
  re.p<-upperGumbel(Q,mu,beta)

  return(re.p)
}

Get.Gumbel.M<-function(re.Q){ #Q a A*q matrix of test statistics, re.Q a B*q matrix of resampled test statistics
  re.Q<-as.matrix(re.Q)
  re.mean<-apply(re.Q,2,mean)
  re.variance<-apply(re.Q,2,var)
  beta<-sqrt(re.variance*6/pi^2)
  mu<-re.mean-beta*0.5772156649 #Euler Mascheroni constant

  threshold.Q<-mu-log(-log(1-0.05))*beta
  M<-0.05/exp(-threshold.Q)
  return(M)
}

Impute<-function(Z, impute.method){
  p<-dim(Z)[2]
  if(impute.method =="random"){
    for(i in 1:p){
      IDX<-which(is.na(Z[,i]))
      if(length(IDX) > 0){
        maf1<-mean(Z[-IDX,i])/2
        Z[IDX,i]<-rbinom(length(IDX),2,maf1)
      }
    }
  } else if(impute.method =="fixed"){
    for(i in 1:p){
      IDX<-which(is.na(Z[,i]))
      if(length(IDX) > 0){
        maf1<-mean(Z[-IDX,i])/2
        Z[IDX,i]<-2 * maf1
      }
    }
  } else if(impute.method =="bestguess") {
    for(i in 1:p){
      IDX<-which(is.na(Z[,i]))
      if(length(IDX) > 0){
        maf1<-mean(Z[-IDX,i])/2
        Z[IDX,i]<-round(2 * maf1)
      }
    }
  } else {
    stop("Error: Imputation method shoud be \"fixed\", \"random\" or \"bestguess\" ")
  }
  return(as.matrix(Z))
}

#percentage notation
percent <- function(x, digits = 3, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
