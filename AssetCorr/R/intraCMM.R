intraCMM <-
  function(d,n,l=0, B=0, DB=c(0,0), JC=FALSE,CI_Boot,type="bca", plot=FALSE){
    if(is.numeric(d)){d=d}else{stop("d is not numeric")}
    if(is.numeric(n)){n=n}else{stop("n is not numeric")}
    if(B==0&& plot==TRUE){stop("please select a number of bootstrap repititions for the plot")}
    if(B%%1==0){B=B}else{stop("B is not an integer")}
    if(DB[1]%%1==0 && DB[2]%%1==0 ){DB=DB}else{stop("At least one entry in DB is not an integer")}
    if(length(d)==length(n)){}else{stop("Input vectors do not have the same length")}
    
    d1=d/n
    CI=0
    estimate=function(X,CI){
      if(CI==0){  
        pd_mean=1/length(X)*sum(X)
        var_dat= 1/length(X)*sum((X^2-X/n))
        foo=function(rho){
          corr=matrix(c(1,rho,rho,1),2)
          prob=pmvnorm(lower=c(-Inf,-Inf),upper=c(qnorm(pd_mean),qnorm(pd_mean)),mean=c(0,0),corr=corr)
          return(prob-var_dat)
        } 
        Res<-uniroot(foo,c(0,1))$root
        
        s=qnorm(pd_mean)
        
        ABL1<- 1/(2*pi*sqrt(1-Res^2))*exp(-(s^2/(1+Res)))
        ABL2<- ((s^2+ Res*(1-2*s^2) + s^2*Res^2 -Res^3)/(2*pi*(1-Res^2)^(5/2)))*exp(-(s^2/(1+Res)))
        
        Time<-length(X)
        if(l>0){
          tryCatch(AC<-(acf(X^2, plot = FALSE, type = "covariance")$acf)[(1:l),1,1], error = function(e) 0)
          Sum=NULL
          for (z in 1:l){
            
            Sum[z]<-(1-z/Time)*AC[z]
          }
          AB=sum(Sum)} else{AB=0}
        
        
        
        nX=X^2
        nM=1/length(X)*sum(nX)
        var2=var(nX)
        
        
        Res2=(Res +(ABL2/(Time*ABL1^3))*(var2/2 + AB))
        
        
        Est<-list(Original =(Res +(ABL2/(Time*ABL1^3))*(var2/2 + AB)))
      }else{
        pd_mean=1/length(X)*sum(X)
        var_dat= 1/length(X)*sum((X^2-X/n))
        foo=function(rho){
          corr=matrix(c(1,rho,rho,1),2)
          prob=pmvnorm(lower=c(-Inf,-Inf),upper=c(qnorm(pd_mean),qnorm(pd_mean)),mean=c(0,0),corr=corr)
          return(prob-var_dat)
        } 
        Res<-uniroot(foo,c(0,1))$root
        
        s=qnorm(pd_mean)
        
        ABL1<- 1/(2*pi*sqrt(1-Res^2))*exp(-(s^2/(1+Res)))
        ABL2<- ((s^2+ Res*(1-2*s^2) + s^2*Res^2 -Res^3)/(2*pi*(1-Res^2)^(5/2)))*exp(-(s^2/(1+Res)))
        
        Time<-length(X)
        if(l>0){
          tryCatch(AC<-(acf(X^2, plot = FALSE, type = "covariance")$acf)[(1:l),1,1], error = function(e) 0)
          Sum=NULL
          for (z in 1:l){
            
            Sum[z]<-(1-z/Time)*AC[z]
          }
          AB=sum(Sum)} else{AB=0}
        
        
        
        nX=X^2
        nM=1/length(X)*sum(nX)
      var2=var(nX)

      
      Res2=(Res +(ABL2/(Time*ABL1^3))*(var2/2 + AB))
        
        Est<-list(Original =Res2, CI=c(Res2-(qt(1-(1-CI)/2,Time-1)*abs(1/ABL1))/sqrt(Time)*sqrt(var2+2*(AB)),Res2+(qt(1-(1-CI)/2,Time-1)*abs(1/ABL1))/sqrt(Time)*sqrt(var2+2*(AB))))
      }
      
    }
    Estimate_Standard<- estimate(d1,CI)
    
    if(DB[1]!=0){
      IN=DB[1]
      OUT=DB[2]
      
      theta1=NULL
      theta2=matrix(ncol = OUT, nrow=IN)
      for(i in 1:OUT){
        N<-length(d1)
        Ib<-sample(N,N,replace=TRUE)  ## sampling with replacement
        Db<-d1[Ib] 
        try(theta1[i]<-estimate(Db,CI)$Original, silent = TRUE)
        
        for(c in 1:IN){
          Ic<-sample(N,N,replace=TRUE)  ## sampling with replacement
          Dc<-Db[Ic] 
          try( theta2[c,i]<-estimate(Dc,CI)$Original, silent = TRUE)
          
        }
      }
      Boot1<- mean(theta1, na.rm = TRUE)
      Boot2<- mean(theta2, na.rm = TRUE)
      BC<- 2*Estimate_Standard$Original -Boot1
      DBC<- (3*Estimate_Standard$Original-3*Boot1+Boot2)
      
      Estimate_DoubleBootstrap<-list(Original = Estimate_Standard$Original, Bootstrap=BC, Double_Bootstrap=DBC, oValues=theta1, iValues=theta2)
      
    }
    
    if(B>0){ 
      N<-length(n)
      D<- matrix(ncol=1, nrow=N,d1)
      
      BCA=function(data, indices){
        
        d <- data[indices,]
        
        tryCatch(estimate(d,CI)$Original,error=function(e)NA)
        #try(estimate(d))
      }
      
      boot1<- boot(data = D, statistic = BCA, R=B)
      
      Estimate_Bootstrap<-list(Original = boot1$t0, Bootstrap=2*boot1$t0 - mean(boot1$t,na.rm = TRUE),bValues=boot1$t )
      if(missing(CI_Boot)){Estimate_Bootstrap=Estimate_Bootstrap}else{
        if(type=="norm"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type)$normal[2:3])}
        if(type=="basic"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type)$basic[4:5])}
        if(type=="perc"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))$percent[4:5]}
        if(type=="bca"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))$bca[4:5]}
        if(type=="all"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))}
        CI=CI_Boot
        
        pd_mean=mean(d1)
        var_dat= 1/length(d1)*sum((d1^2-d1/n))
        foo=function(rho){
          corr=matrix(c(1,rho,rho,1),2)
          prob=pmvnorm(lower=c(-Inf,-Inf),upper=c(qnorm(pd_mean),qnorm(pd_mean)),mean=c(0,0),corr=corr)
          return(prob-var_dat)
        } 
        Res<-uniroot(foo,c(0,1))$root
        s=qnorm(pd_mean)
        
        ABL1<- 1/(2*pi*sqrt(1-Res^2))*exp(-(s^2/(1+Res)))
        ABL2<- ((s^2+ Res*(1-2*s^2) + s^2*Res^2 -Res^3)/(2*pi*(1-Res^2)^(5/2)))*exp(-(s^2/(1+Res)))
        nX=d1^2
        nM=1/length(d1^2)*sum(d1^2)
        var2=1/length(d1)*sum(nX^2-nM^2)
        if(l>0){
          tryCatch(AC<-(acf(d1^2, plot = FALSE, type = "covariance")$acf)[(1:l),1,1], error = function(e) 0)
          Sum=NULL
          for (z in 1:l){
            
            Sum[z]<-(1-z/N)*AC[z]
          }
          AB=sum(Sum)} else{AB=0}
        Res2=(Res +(ABL2/(N*ABL1^3))*(var2/2 + AB))
        CI=c(Res2-(qt(1-(1-CI)/2,N-1)/abs(ABL1))/sqrt(N)*sqrt(var2+2*(AB)),Res2+(qt(1-(1-CI)/2,N-1)/abs(ABL1))/sqrt(N)*sqrt(var2+2*(AB)))
        
        Estimate_Bootstrap<-list(Original = boot1$t0, Bootstrap=2*boot1$t0 - mean(boot1$t,na.rm = TRUE),CI=CI,CI_Boot=Conf,bValues=boot1$t )
        
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
    
    if(JC==TRUE){
      
      N=length(d1)
      Test=NULL
      for(v in 1:N){
        d2<-d1[-v]
        
        try(Test[v]<-estimate(d2,CI)$Original)
        
      }
      
      Estimate_Jackknife<-list(Original = Estimate_Standard$Original, Jackknife=(N*Estimate_Standard$Original-(N-1)*mean(Test)))
      
      
      
    } 
    
    if(B>0){return(Estimate_Bootstrap)}
    if(JC==TRUE){return(Estimate_Jackknife)}
    if(DB[1]!=0){return(Estimate_DoubleBootstrap)}
    if(B==0 && JC==FALSE && DB[1]==0){return(Estimate_Standard)}
    
    
    
  }
