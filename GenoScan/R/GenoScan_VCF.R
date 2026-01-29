
GenoScan.VCF.chr<-function(result.prelim,vcf.filename,chr,pos.min=NULL,pos.max=NULL,Gsub.id=NULL,annot.filename=NULL,cell.type=NULL,MAF.weights='beta',test='combined',window.size=c(5000,10000,15000,20000,25000,50000),MAF.threshold=1,impute.method='fixed'){

  time.start<-proc.time()[3]
  region.step=0.2*10^6
  options(scipen = 999) #remove scientific notations
  #data("GenoScan.info")
  #pos.min<-0#min(pos)
  #pos.max<-GenoScan.info$hg19.chrom.sizes[match(paste0('chr',chr),GenoScan.info$hg19.chrom.sizes[,1]),2]#max(pos)+50000
  if(length(pos.min)==0){pos.min<-1}
  if(length(pos.max)==0){
    #hg19.chrom.sizes<-read.table('http://hgdownload.cse.ucsc.edu/goldenPath/hg19/bigZips/hg19.chrom.sizes')
    #hg19.chrom.sizes<-GenoScan.info$hg19.chrom.sizes
    hg19.chrom.sizes<-data.frame(fread('http://hgdownload.cse.ucsc.edu/goldenPath/hg19/bigZips/hg19.chrom.sizes'))
    pos.max<-50000+hg19.chrom.sizes[match(paste0('chr',chr),hg19.chrom.sizes[,1]),2]#max(pos)+50000
  }

  start<-pos.min;null.model<-F;
  result.summary<-c();M.sum<-0
  while (start<pos.max){
    end<-start+region.step
    print(paste0(percent((start-pos.min)/(pos.max-pos.min)),' finished. Time used: ', round(proc.time()[3]-time.start,digits=1),'s. Scan ',start,'-',end))

    #dir.string<-paste0('tabix /home/skardia_lab/zihuai/GenoNet/GenoNetScores_byChr/GenoNet_',chr,'.bed.gz chr',chr,':',max(0,start-5000),'-',end+5000,' > /home/skardia_lab/zihuai/SFARI_SSC_WGS_1b/MidFiles/ScanT_Subset_',i,'_',l,'.txt')
    range<-paste0(chr,":",start,"-",end)
    GenoScan.Region.fit<-GenoScan.VCF.Region(result.prelim,vcf.filename,range,annot.filename=annot.filename,cell.type=cell.type,MAF.weights=MAF.weights,test=test,window.size=window.size,MAF.threshold=MAF.threshold,impute.method=impute.method,job_id=paste0('GenoScan_temp_',chr,':',pos.min,'-',pos.max))
    if ('try-error' %in% class(GenoScan.Region.fit)|is.na(GenoScan.Region.fit$p.value)==T){
      GenoScan.Region.fit<-c();GenoScan.Region.fit$M<-0;GenoScan.Region.fit$p.value<-NA;GenoScan.Region.fit$re.minP<-NA;GenoScan.Region.fit$window.summary<-c()
    }

    result.summary<-rbind(result.summary,GenoScan.Region.fit$window.summary)
    M.sum<-M.sum+GenoScan.Region.fit$M

    start<-end-50000
  }
  unique.index<-match(unique(paste0(result.summary[,1],'-',result.summary[,2])),paste0(result.summary[,1],'-',result.summary[,2]))
  result.summary<-cbind(chr,result.summary[unique.index,])

  if(length(cell.type)==0){
    colnames(result.summary)<-c('chr','start','end','dispersion','burden')
  }else{
    if('all'%in%cell.type){
      cell.type<-c("E001", "E002", "E003", "E004", "E005", "E006", "E007", "E008", "E009", "E010",
                   "E011", "E012", "E013", "E014", "E015", "E016", "E017", "E018", "E019", "E020",
                   "E021", "E022", "E023", "E024", "E025", "E026", "E027", "E028", "E029", "E030",
                   "E031", "E032", "E033", "E034", "E035", "E036", "E037", "E038", "E039", "E040",
                   "E041", "E042", "E043", "E044", "E045", "E046", "E047", "E048", "E049", "E050",
                   "E051", "E052", "E053", "E054", "E055", "E056", "E057", "E058", "E059", "E061",
                   "E062", "E063", "E065", "E066", "E067", "E068", "E069", "E070", "E071", "E072",
                   "E073", "E074", "E075", "E076", "E077", "E078", "E079", "E080", "E081", "E082",
                   "E083", "E084", "E085", "E086", "E087", "E088", "E089", "E090", "E091", "E092",
                   "E093", "E094", "E095", "E096", "E097", "E098", "E099", "E100", "E101", "E102",
                   "E103", "E104", "E105", "E106", "E107", "E108", "E109", "E110", "E111", "E112",
                   "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E120", "E121", "E122",
                   "E123", "E124", "E125", "E126", "E127", "E128", "E129")
    }
    colnames(result.summary)<-c('chr','start','end',
                                c('dispersion',paste0('dispersion-',cell.type)),
                                c('burden',paste0('burden-',cell.type)))
  }

  return(list(window.summary=result.summary,M.sum=M.sum,threshold=0.05/M.sum))
}

GenoScan.VCF.Region<-function(result.prelim,vcf.filename,range,Gsub.id=NULL,annot.filename=NULL,cell.type=NULL,MAF.weights='beta',test='combined',window.size=c(5000,10000,15000,20000,25000,50000),MAF.threshold=1,impute.method='fixed',job_id=range){
  #load vcf region
  #vcf.fileName<-paste0("/home/skardia_lab/zihuai/SFARI_SSC_WGS_1b/SSC_WGS_SNP_PASS_chr",i,".vcf.gz")
  #range<-'1:2000000-2010000'
  chr<-as.numeric(gsub(":.*$","",range))
  G <- t(readVCFToMatrixByRange(vcf.filename, range,annoType='')[[1]])
  #match phenotype id and genotype id
  if(length(Gsub.id)==0){match.index<-match(result.prelim$id,rownames(G))}else{
    match.index<-match(result.prelim$id,Gsub.id)
  }
  if(mean(is.na(match.index))>0){
    msg<-sprintf("Some individuals are not matched with genotype. The rate is%f", mean(is.na(match.index)))
    warning(msg,call.=F)
  }
  G<-G[match.index,]
  if(ncol(G)<=1){
    msg<-'Number of variants in the specified range is <= 1, please try a larger region'
    warning(msg,call.=F)
    p.value<-NA
    return(list(p.value=p.value))
  }
  # missing genotype imputation
  G[G==-9 | G==9]<-NA
  N_MISS<-sum(is.na(G))
  if(N_MISS>0){
    msg<-sprintf("The missing genotype rate is %f. Imputation is applied.", N_MISS/nrow(G)/ncol(G))
    warning(msg,call.=F)
    G<-Impute(G,impute.method)
  }
  #get positions
  pos<-as.numeric(gsub("^.*\\:","",colnames(G)))

  #load preliminary features
  Y<-result.prelim$Y;X0<-result.prelim$X0
  n<-result.prelim$n;id<-result.prelim$id
  nullglm<-result.prelim$nullglm;out_type<-result.prelim$out_type
  mu<-nullglm$fitted.values;Y.res<-Y-mu
  re.Y.res<-result.prelim$re.Y.res
  n<-nrow(Y);p<-ncol(G);X0<-svd(X0)$u

  # genotype
  G<-as.matrix(G);center.G<-t(t(G)-colMeans(G))
  MAF<-colMeans(G)/2;MAF[is.na(MAF)]<-0
  G[,MAF>0.5]<-2-G[,MAF>0.5]
  MAF[MAF>0.5]<-1-MAF[MAF>0.5]
  index<-which(colMeans(center.G^2)!=0 & MAF<MAF.threshold)
  G<-as.matrix(G[,index])
  pos<-pos[index]
  MAF<-MAF[index]
  G<-Matrix(G,sparse=T)

  if(length(index)==0){
    msg<-sprintf("No variant has variation, return NA")
    warning(msg,call.=F)
    p.value<-NA
    return(list(p.value=p.value))
  }

  if(MAF.weights=='beta'){weights<-as.matrix(dbeta(MAF,1,25))}
  if(MAF.weights=='equal'){weights<-as.matrix(rep(1,length(MAF)))}#rep(1,ncol(SNP.set))
  #get functional annotations
  if(length(annot.filename)!=0 & length(cell.type)!=0){
    dir.string<-paste0('tabix ',annot.filename,' chr',chr,':',min(pos),'-',max(pos),' > ',job_id,'.txt')
    system(dir.string)
    score<-try(data.frame(fread(paste0(job_id,'.txt'))),silent = T)
    if(length(score)!=0){
      colnames(score)<-c('chr','start','end','name',
                         "E001", "E002", "E003", "E004", "E005", "E006", "E007", "E008", "E009", "E010",
                         "E011", "E012", "E013", "E014", "E015", "E016", "E017", "E018", "E019", "E020",
                         "E021", "E022", "E023", "E024", "E025", "E026", "E027", "E028", "E029", "E030",
                         "E031", "E032", "E033", "E034", "E035", "E036", "E037", "E038", "E039", "E040",
                         "E041", "E042", "E043", "E044", "E045", "E046", "E047", "E048", "E049", "E050",
                         "E051", "E052", "E053", "E054", "E055", "E056", "E057", "E058", "E059", "E061",
                         "E062", "E063", "E065", "E066", "E067", "E068", "E069", "E070", "E071", "E072",
                         "E073", "E074", "E075", "E076", "E077", "E078", "E079", "E080", "E081", "E082",
                         "E083", "E084", "E085", "E086", "E087", "E088", "E089", "E090", "E091", "E092",
                         "E093", "E094", "E095", "E096", "E097", "E098", "E099", "E100", "E101", "E102",
                         "E103", "E104", "E105", "E106", "E107", "E108", "E109", "E110", "E111", "E112",
                         "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E120", "E121", "E122",
                         "E123", "E124", "E125", "E126", "E127", "E128", "E129")
      if('all'%in%cell.type){Z<-as.matrix(score[floor((pos-score[1,2])/25)+1,-c(1:4)])}else{
        Z<-as.matrix(score[floor((pos-score[1,2])/25)+1,cell.type])
      }
      Z[is.na(Z)]<-0
      Z<-cbind(weights,as.vector(weights)*Z)
    }else{
      if('all'%in%cell.type){Z<-matrix(weights,length(weights),128)}else{
        Z<-matrix(weights,length(weights),length(cell.type)+1)
      }
    }
  }else{
    Z<-as.matrix(weights)
  }

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

  return(list(minP=minP,re.minP=re.minP,n.marker=ncol(G),window.summary=window.summary,M=M,p.value=p.value))
}
