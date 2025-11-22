analyze_AssetCorr<-function(DTS,N,B=NA,DB=NA,JC=FALSE,CI_Boot=NA,Adjust=0.0001,type="bca",
                  Intra=c("AMM","FMM","CMM","JDP1","JDP2","MLE","AMLE","Beta","Mode"),
                  Inter=c("Copula","Cov","JDP","MLE")){

  
  NS=ncol(DTS)
  NT=nrow(DTS)
  sector_names=colnames(DTS)
  if(length(sector_names)==0){
    sector_names=rep("",NS)}
  #Input checks:
  
  if(NS>1){
    for(k in 1:(NS)){
     
        d1=DTS[,k]
        n1=N[,k]
      
        if(is.numeric(d1)){d1=d1}else{stop(paste("d in column ", k ," is not numeric", sep = ""))}
        if(is.numeric(n1)){n1=n1}else{stop(paste("n in column ", k ," is not numeric", sep = ""))}

    }
  }
  

  
  
  #intra correlations will be estimated via MLE (best approach)
    rho=numeric(NS)
    for(k in 1:NS)
      rho[k]=intraMLE(d = DTS[,k],n = N[,k])$Original
  
  
  #Intra-Schaetzer
  for(k in 1:NS){
    temp=intraALL(d = DTS[,k],n = N[,k],B = B,DB = DB,JC = JC,CI_Boot = CI_Boot,type = type,Estimator = Intra,Adjust=Adjust)
    if(k==1)
      Estimators_Intra=data.frame(Sektor=k,Sektor_Name=sector_names[k],temp)
    else
      Estimators_Intra=rbind(Estimators_Intra,data.frame(Sektor=k,Sektor_Name=sector_names[k],temp))
  }
  
  
  #Inter-Schaetzer
  if(NS>1){
    for(k in 1:(NS-1)){
      for(l in (k+1):NS){
        temp=interALL(d1 = DTS[,k],n1 = N[,k],d2 = DTS[,l],n2 = N[,l],rho1 = rho[k],rho2 = rho[l],B = B,DB = DB,JC = JC,CI_Boot = CI_Boot,Estimator = Inter)
        if(k==1 && l==2)
          Estimators_Inter=data.frame(Sektor_1=k,Sektor_Name_1=sector_names[k],Sektor_2=l,Sektor_Name_2=sector_names[l],temp)
        else
          Estimators_Inter=rbind(Estimators_Inter,data.frame(Sektor_1=k,Sektor_Name_1=sector_names[k],Sektor_2=l,Sektor_Name_2=sector_names[l],temp))
      }
    }
  }
  else{
    Estimators_Inter=data.frame()}



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#Data preperation
Defaults=NULL
Sector=NULL
for (t in 1:NS){
  
  Defaults=c(Defaults,DTS[,t])
  Name=paste0("S",t)
  Sector=c(Sector,rep(Name,NT))
}
Time=rep(1:NT,NS)
Plot<-data.frame(Time,Defaults,Sector)
Mean=apply(DTS,2,mean,na.rm=TRUE)
  DTS_plot=ggplot(Plot,aes(x=Time,y=Defaults,col=Sector))+
    geom_line()+theme_bw()+
    geom_hline(data = data.frame(Sector=unique(Sector),Mean=Mean),mapping = aes(yintercept=Mean,col=Sector))+ylab("Default Time Series")
 
  colnames(Estimators_Intra)<-c("Sector","Sector_Name", "Estimator","Estimate","Type","correction","B","DB","CI_Boot","CI")
  colnames(Estimators_Inter)<-c("Sector_1","Sector_Name_1","Sector_2","Sector_Name_2", "Estimator","Estimate","Type","correction","B","DB","CI_Boot","CI")
  
  if(!is.na(CI_Boot)){
    
    Estimator=Estimators_Intra$Estimator
    Estimate=Estimators_Intra$Estimate
    CI=Estimators_Intra$CI
    correction=Estimators_Intra$correction
    
    Intra_plot=ggplot(data = Estimators_Intra,aes(x=Estimator,y=Estimate,shape=CI,col=correction))+theme_bw() +geom_point()+facet_grid(.~Sector)+ylab("Intra Asset Correlation")
    
    Estimator=Estimators_Inter$Estimator
    Estimate=Estimators_Inter$Estimate
    CI=Estimators_Inter$CI
    correction=Estimators_Inter$correction
    
    Inter_plot=ggplot(data = Estimators_Inter,aes(x=Estimator,y=Estimate,col=correction,shape=CI))+theme_bw()+geom_point()+facet_grid(Sector_1~Sector_2)+ylab("Inter Correlation")
  }else{
    Estimator=Estimators_Intra$Estimator
    Estimate=Estimators_Intra$Estimate
    correction=Estimators_Intra$correction
    
    Intra_plot=ggplot(data = Estimators_Intra,aes(x=Estimator,y=Estimate,col=correction))+theme_bw()+geom_point()+facet_grid(.~Sector)+ylab("Intra Asset Correlation")
    
    Estimator=Estimators_Inter$Estimator
    Estimate=Estimators_Inter$Estimate
    correction=Estimators_Inter$correction
    
    Inter_plot=ggplot(data = Estimators_Inter,aes(x=Estimator,y=Estimate,col=correction))+theme_bw()+geom_point()+facet_grid(Sector_1~Sector_2)+ylab("Inter Correlation")
    }
    
    
  
  
  
 
  multiplot(DTS_plot,Intra_plot,Inter_plot,layout = matrix(c(1,2,3,3),ncol = 1))
  
  return(list(Estimators_Intra=Estimators_Intra,Estimators_Inter=Estimators_Inter))
}