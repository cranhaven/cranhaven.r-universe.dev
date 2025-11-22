intraALL<-function(d,n,B=NA,DB=NA,JC=FALSE,CI_Boot=NA,Adjust=0.0001,plot=FALSE,type="bca",Quantile=0.999,Estimator=c("AMM","FMM","CMM","JDP1","JDP2","MLE","AMLE","Beta","Mode"),show_progress=FALSE){
  if(is.numeric(Adjust)){Adjust=Adjust}else{stop("Adjust is not numeric")}
  if(is.numeric(d)){d=d}else{stop("d is not numeric")}
  if(is.numeric(n)){n=n}else{stop("n is not numeric")}
  if(length(d)==length(n)){}else{stop("Input vectors do not have the same length")}
  
  NT=length(d)
  NE=length(Estimator)
  
  #Punktschaetzer mit Jackknife und Standardschaetzer
  PEST=data.frame(Estimator=Estimator,value=NA,Type=c("PEST"),correction=rep("none",NE),B=NA,DB=NA,CI_Boot=NA,lower_upper_ci=c(""),stringsAsFactors = FALSE)
  JackEST=data.frame(Estimator=Estimator,value=NA,Type=c("CEST"),correction=rep("Jackknife",NE),B=NA,DB=NA,CI_Boot=NA,lower_upper_ci=c(""),stringsAsFactors = FALSE)
  
  if(show_progress){
    if(JC)
      cat("original and jackknife corrected estimators\n")
    else
      cat("original estimators\n")
    pb=txtProgressBar(style = 3)
  }
  for(i in 1:NE){
    try({
      temp=
        switch (JackEST$Estimator[i],
                "AMM" = intraAMM(d = d,n = n,JC = JC),
                "FMM" = intraFMM(d = d,n = n,JC = JC),
                "CMM" = intraCMM(d = d,n = n,JC = JC),
                "JDP1" = intraJDP1(d = d,n = n,JC = JC),
                "JDP2" = intraJDP2(d = d,n = n,JC = JC),
                "MLE" = intraMLE(d = d,n = n,JC = JC),
                "AMLE" = intraAMLE(d = d,n = n,JC = JC,Adjust = Adjust),
                "Beta" = intraBeta(d = d,n = n,JC = JC,Quantile=Quantile),
                "Mode" = intraMode(d = d,n = n,JC = JC),
                NA
        )
      PEST$value[i]=temp$Original
      if(JC)
        JackEST$value[i]=temp$Jackknife
    })
    if(show_progress)
      setTxtProgressBar(pb,value = i/NE)
  }
  if(show_progress)
    cat("\n")
  Estimators=PEST
  if(JC)
    Estimators=rbind(Estimators,JackEST)
  
  if(!is.na(B)){
    #Punkt-/Intervallschaetzer mit Bootstrap
    BootEST=data.frame(Estimator=Estimator,value=NA,Type=c("CEST"),correction=rep("Bootstrap",NE),B=B,DB=NA,CI_Boot=NA,lower_upper_ci=c(""),stringsAsFactors = FALSE)
    LIBootEST=data.frame(Estimator=Estimator,value=NA,Type=c("IEST"),correction=rep("Bootstrap",NE),B=B,DB=NA,CI_Boot=CI_Boot,lower_upper_ci=rep("lower",NE),stringsAsFactors = FALSE)
    UIBootEST=data.frame(Estimator=Estimator,value=NA,Type=c("IEST"),correction=rep("Bootstrap",NE),B=B,DB=NA,CI_Boot=CI_Boot,lower_upper_ci=rep("upper",NE),stringsAsFactors = FALSE)
    
    if(show_progress){
      cat("bootstrap corrected estimators\n")
      pb=txtProgressBar(style = 3)  
    }
    if(!is.na(CI_Boot)){
      for(i in 1:NE){
        try({
          temp=
            switch (LIBootEST$Estimator[i],
                    "AMM" = intraAMM(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type),
                    "FMM" = intraFMM(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type),
                    "CMM" = intraCMM(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type),
                    "JDP1" = intraJDP1(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type),
                    "JDP2" = intraJDP2(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type),
                    "MLE" = intraMLE(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type),
                    "AMLE" = intraAMLE(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type,Adjust = Adjust),
                    "Beta" = intraBeta(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type,Quantile=Quantile),
                    "Mode" = intraMode(d = d,n = n,B = B,CI_Boot = CI_Boot,type = type),
                    NA
            )
          BootEST$value[i]=temp$Bootstrap
          LIBootEST$value[i]=temp$CI_Boot[1]
          UIBootEST$value[i]=temp$CI_Boot[2]
        })
        if(show_progress)
          setTxtProgressBar(pb,value = i/NE)
      }
      if(show_progress)
        cat("\n")
      Estimators=rbind(Estimators,BootEST,LIBootEST,UIBootEST)
    }
    else{
      for(i in 1:NE){
        try({
          temp=
            switch (LIBootEST$Estimator[i],
                    "AMM" = intraAMM(d = d,n = n,B = B),
                    "FMM" = intraFMM(d = d,n = n,B = B),
                    "CMM" = intraCMM(d = d,n = n,B = B),
                    "JDP1" = intraJDP1(d = d,n = n,B = B),
                    "JDP2" = intraJDP2(d = d,n = n,B = B),
                    "MLE" = intraMLE(d = d,n = n,B = B),
                    "AMLE" = intraAMLE(d = d,n = n,B = B,Adjust = Adjust),
                    "Beta" = intraBeta(d = d,n = n,B = B,Quantile=Quantile),
                    "Mode" = intraMode(d = d,n = n,B = B),
                    NA
            )
          BootEST$value[i]=temp$Bootstrap
        })
        if(show_progress)
          setTxtProgressBar(pb,value = i/NE)
      }
      if(show_progress)
        cat("\n")
      Estimators=rbind(Estimators,BootEST)
    }
  }
  
  if(all(!is.na(DB))){
    #Punktschaetzer mit Double-Bootstrap
    DBootEST=data.frame(Estimator=Estimator,value=NA,Type=c("CEST"),correction=rep("Double Bootstrap",NE),B=DB[1],DB=DB[2],CI_Boot=NA,lower_upper_ci=c(""),stringsAsFactors = FALSE)
    
    if(show_progress){
      cat("double bootstrap corrected estimators\n")
      pb=txtProgressBar(style = 3)  
    }
    
    for(i in 1:NE){
      try({
        DBootEST$value[i]=
          switch (DBootEST$Estimator[i],
                  "AMM" = intraAMM(d = d,n = n,DB = DB)$Double_Bootstrap,
                  "FMM" = intraFMM(d = d,n = n,DB = DB)$Double_Bootstrap,
                  "CMM" = intraCMM(d = d,n = n,DB = DB)$Double_Bootstrap,
                  "JDP1" = intraJDP1(d = d,n = n,DB = DB)$Double_Bootstrap,
                  "JDP2" = intraJDP2(d = d,n = n,DB = DB)$Double_Bootstrap,
                  "MLE" = intraMLE(d = d,n = n,DB = DB)$Double_Bootstrap,
                  "AMLE" = intraAMLE(d = d,n = n,DB = DB,Adjust = Adjust)$Double_Bootstrap,
                  "Beta" = intraBeta(d = d,n = n,DB = DB,Quantile=Quantile)$Double_Bootstrap,
                  "Mode" = intraMode(d = d,n = n,DB = DB)$Double_Bootstrap,
                  NA
          )
      })
      if(show_progress)
        setTxtProgressBar(pb,value = i/NE)
    }
    if(show_progress)
      cat("\n")
    Estimators=rbind(Estimators,DBootEST)  
  }
  
  if(plot==TRUE){
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
   
    Plot=data.frame(1:length(d),d)
    colnames(Plot)<-c("Time","Defaults")
    Time<-Plot$Time
    Defaults<-Plot$Defaults
    DTS_plot=ggplot(Plot,aes(x=Time,y=Defaults))+theme_bw() +geom_line()+ggtitle("AssetCorr- An Overview" )+theme(plot.title = element_text(hjust = 0.5,size=15, face="bold.italic"))
    if(!is.na(CI_Boot)){
      
      colnames(Estimators)<-c("Estimator","Estimate","Type","correction","B","DB","CI_Boot","CI")
      
      Estimate=Estimators$Estimate
      CI=Estimators$CI
      correction=Estimators$correction
      
        EST_plot=ggplot(data = Estimators,aes(x="",y=Estimate,shape=CI,col=correction))+theme_bw() +geom_point()+facet_grid(.~Estimator)+theme(axis.title.x = element_blank())
     
    }else{
      colnames(Estimators)<-c("Estimator","Estimate","Type","correction","B","DB","CI_Boot","CI")
      Estimate=Estimators$Estimate
      correction=Estimators$correction
      EST_plot=ggplot(data = Estimators,aes(x="",y=Estimate,col=correction))+theme_bw()  +geom_point()+facet_grid(.~Estimator)+theme(axis.title.x = element_blank())
     
       
    }
    multiplot(DTS_plot,EST_plot)
    }
  return(Estimators) 
}

