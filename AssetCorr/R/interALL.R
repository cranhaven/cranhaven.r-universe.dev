interALL<-function(d1,n1,d2,n2,rho1,rho2,B=NA,DB=NA,JC=FALSE,CI_Boot=NA,plot=FALSE,type="bca",Estimator=c("Copula","Cov","JDP","MLE"),show_progress=FALSE){
  
  if(is.numeric(d1)){d1=d1}else{stop("d1 is not numeric")}
  if(is.numeric(n1)){n1=n1}else{stop("n1 is not numeric")}
  if(is.numeric(d2)){d2=d2}else{stop("d2 is not numeric")}
  if(is.numeric(n2)){n2=n2}else{stop("n2 is not numeric")}
  if(is.numeric(rho1)){rho1=rho1}else{stop("rho1 is not numeric")}
  if(is.numeric(rho2)){rho2=rho2}else{stop("rho1 is not numeric")}
  if(length(d1)==length(n1) && length(d2)==length(n2) && length(d1)==length(d2)){}else{stop("Input vectors do not have the same length")}
  
  
  NT1=length(d1)
  NT2=length(d2)
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
                "Copula" = interCopula(df1 = d1/n1,df2=d2/n2,JC = JC),
                "Cov" = interCov(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,JC = JC),
                "JDP" = interJDP(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,JC = JC),
                "MLE" = interMLE(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,JC = JC),
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
                    "Copula" = interCopula(df1 = d1/n1,df2=d2/n2,B = B,CI_Boot = CI_Boot),
                    "Cov" = interCov(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,B=B,CI_Boot = CI_Boot),
                    "JDP" = interJDP(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,B=B,CI_Boot = CI_Boot),
                    "MLE" = interMLE(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,B=B,CI = CI_Boot),
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
                    "Copula" = interCopula(df1 = d1/n1,df2=d2/n2,B = B),
                    "Cov" = interCov(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,B=B),
                    "JDP" = interJDP(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,B=B),
                    "MLE" = interMLE(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,B=B),
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
                  "Copula" = interCopula(df1 = d1/n1,df2=d2/n2,DB = DB)$Double_Bootstrap,
                  "Cov" = interCov(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,DB=DB)$Double_Bootstrap,
                  "JDP" = interJDP(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,DB=DB)$Double_Bootstrap,
                  "MLE" = interMLE(d1 = d1,n1 = n1,d2 = d2,n2 = n2,rho1 = rho1,rho2 = rho2,DB=DB)$Double_Bootstrap,
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
    DTS=cbind(d1,d2)
    N=cbind(n1,n2)
    NT=length(d1)
    Time=rep(1:NT,2)
    Defaults=c(DTS[,1],DTS[,2])
    Sector=c(rep("S1",NT),rep("S2",NT))
    Mean=apply(DTS,2,mean,na.rm=TRUE)
    
    DTS_plot=ggplot(data.frame(Time=Time,Defaults=Defaults,Sector=Sector),aes(x=Time,y=Defaults,col=Sector))+
      geom_line()+
      geom_hline(data = data.frame(Sector=c("S1","S2"),Mean=Mean),mapping = aes(yintercept=Mean,col=Sector))+ylab("Default Time Series")
    
    if(!is.na(CI_Boot)){
      
      colnames(Estimators)<-c("Estimator","Estimate","Type","correction","B","DB","CI_Boot","CI")
      
      Estimate=Estimators$Estimate
      CI=Estimators$CI
      correction=Estimators$correction
      
      EST_plot=ggplot(data = Estimators,aes(x="",y=Estimate,shape=CI,col=correction))+theme_bw() +geom_point()+facet_grid(.~Estimator)+theme(axis.title.x = element_blank())+ylab("Inter Correlation")
      
    }else{
      colnames(Estimators)<-c("Estimator","Estimate","Type","correction","B","DB","CI_Boot","CI")
      Estimate=Estimators$Estimate
      correction=Estimators$correction
      EST_plot=ggplot(data = Estimators,aes(x="",y=Estimate,col=correction))+theme_bw()  +geom_point()+facet_grid(.~Estimator)+theme(axis.title.x = element_blank())+ylab("Inter Correlation")
      
      
    }
    

    
    
    multiplot(DTS_plot,EST_plot,layout = matrix(c(1,2,2,2),ncol = 1))
    
   
  }
  return(Estimators) 
}

