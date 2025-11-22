intraAMM <-
  function(d,n,B=0,DB=c(0,0),JC=FALSE, CI_Boot, type="bca", plot=FALSE){
    if(is.numeric(d)){d=d}else{stop("d is not numeric")}
    if(is.numeric(n)){n=n}else{stop("n is not numeric")}
    if(B==0&& plot==TRUE){stop("please select a number of bootstrap repititions for the plot")}
    if(B%%1==0){B=B}else{stop("B is not an integer")}
    if(DB[1]%%1==0 && DB[2]%%1==0 ){DB=DB}else{stop("At least one entry in DB is not an integer")}
    if(length(d)==length(n)){}else{stop("Input vectors do not have the same length")}
    
    
    d1=d/n
    
    
    
    estimate=function(X){
      
      
      pd_mean=mean(X)
      var_dat=var(X)
      foo=function(rho){
        corr=matrix(c(1,rho,rho,1),2)
        prob=pmvnorm(lower=c(-Inf,-Inf),upper=c(qnorm(pd_mean),qnorm(pd_mean)),mean=c(0,0),corr=corr)
        return(prob-pd_mean^2-var_dat)
      } 
      
      Est<-list(Original = uniroot(foo,c(0,1))$root
      )
    }
    
    Estimate_Standard<- estimate(d1)
    if(B>0){ 
      N<-length(n)
      D<- matrix(ncol=1, nrow=N,d1)
      
      BCA=function(data, indices){
        
        d <- data[indices,]
        
        tryCatch(estimate(d)$Original,error=function(e)NA)
        
      }
      
      boot1<- boot(data = D, statistic = BCA, R=B)
      
      
      Estimate_Bootstrap<-list(Original = boot1$t0, Bootstrap=2*boot1$t0 - mean(boot1$t,na.rm = TRUE),bValues=boot1$t )
      
      
      if(missing(CI_Boot)){Estimate_Bootstrap=Estimate_Bootstrap}else{
        if(type=="norm"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type)$normal[2:3])}
        if(type=="basic"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type)$basic[4:5])}
        if(type=="perc"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))$percent[4:5]}
        if(type=="bca"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))$bca[4:5]}
        if(type=="all"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))}
        
        Estimate_Bootstrap<-list(Original = boot1$t0, Bootstrap=2*boot1$t0 - mean(boot1$t,na.rm = TRUE),CI_Boot=Conf,bValues=boot1$t )
        
      }
      
      if(plot==TRUE){
        Dens<-density(boot1$t, na.rm = TRUE)
        XY<-cbind(Dens$x,Dens$y)
        label<-data.frame(rep("Bootstrap density",times=length(Dens$x)))
        Plot<-cbind(XY,label)
        colnames(Plot)<-c("Estimate","Density","Label")
        
        
        SD<-cbind(rep(boot1$t0,times=length(Dens$x)), Dens$y,rep("Standard estimate",times=length(Dens$x)))
        colnames(SD)<-c("Estimate","Density","Label")
        BC<-cbind(rep(Estimate_Bootstrap$Bootstrap,times=length(Dens$x)), Dens$y,rep("Bootstrap corrected estimate",times=length(Dens$x)))
        colnames(BC)<-c("Estimate","Density","Label")
        
        Plot<-rbind(Plot,SD, BC)
        Plot$Estimate<-as.numeric(Plot$Estimate)
        Plot$Density<- as.numeric(Plot$Density)
        
        Estimate<-Plot$Estimate
        Density<-Plot$Density
        Label<-Plot$Label
        P<-ggplot()
        P<-P+with(Plot, aes(x=Estimate, y=Density, colour=Label)) +
          geom_line()+
          scale_colour_manual(values = c("black", "red", "orange"))+
          theme_minimal(base_size = 15) +
          ggtitle("Bootstrap Density" )+
          theme(plot.title = element_text(hjust = 0.5),legend.position="bottom",legend.text = element_text(size = 12),legend.title = element_text( size = 12), legend.justification = "center",axis.text.x= element_text(face = "bold", size = 12)) 
        print(P)
        
      }
      
      
      
      
    }
    
    if(DB[1]!=0){
      IN=DB[1]
      OUT=DB[2]
      
      theta1=NULL
      theta2=matrix(ncol = OUT, nrow=IN)
      for(i in 1:OUT){
        N<-length(d1)
        Ib<-sample(N,N,replace=TRUE)  ## sampling with replacement
        Db<-d1[Ib] 
        try(theta1[i]<-estimate(Db)$Original, silent = TRUE)
        
        for(c in 1:IN){
          Ic<-sample(N,N,replace=TRUE)  ## sampling with replacement
          Dc<-Db[Ic] 
          try( theta2[c,i]<-estimate(Dc)$Original, silent = TRUE)
          
        }
      }
      Boot1<- mean(theta1, na.rm = TRUE)
      Boot2<- mean(theta2, na.rm = TRUE)
      BC<- 2*Estimate_Standard$Original -Boot1
      DBC<- (3*Estimate_Standard$Original-3*Boot1+Boot2)
      
      Estimate_DoubleBootstrap<-list(Original = Estimate_Standard$Original, Bootstrap=BC, Double_Bootstrap=DBC, oValues=theta1, iValues=theta2)
      
    }
    
    
    
    if(JC==TRUE){
      N=length(d1)
      Test=NULL
      for(v in 1:N){
        d2<-d1[-v]
        
        try(Test[v]<-estimate(d2)$Original)
        
      }
      
      Estimate_Jackknife<-list(Original = Estimate_Standard$Original, Jackknife=(N*Estimate_Standard$Original-(N-1)*mean(Test)))
      
      
      
    } 
    
    if(B>0){return(Estimate_Bootstrap)}
    if(JC==TRUE){return(Estimate_Jackknife)}
    if(DB[1]!=0){return(Estimate_DoubleBootstrap)}
    if(B==0 && JC==FALSE && DB[1]==0){return(Estimate_Standard)}
    
    
    
  }
