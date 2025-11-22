interCMM <-
  function(d1,n1,d2,n2,rho,l=0, B=0, DB=c(0,0), JC=FALSE,CI_Boot, type="bca", plot=FALSE){
    if(is.numeric(d1)){d1=d1}else{stop("d1 is not numeric")}
    if(is.numeric(n1)){n1=n1}else{stop("n1 is not numeric")}
    if(is.numeric(d2)){d2=d2}else{stop("d2 is not numeric")}
    if(is.numeric(n2)){n2=n2}else{stop("n2 is not numeric")}
    if(is.numeric(rho)){rho=rho}else{stop("rho is not numeric")}

    if(B==0&& plot==TRUE){stop("please select a number of bootstrap repititions for the plot")}
    if(B%%1==0){B=B}else{stop("B is not an integer")}
    if(DB[1]%%1==0 && DB[2]%%1==0 ){DB=DB}else{stop("At least one entry in DB is not an integer")}
    if(length(d1)==length(n1) && length(d2)==length(n2) && length(d1)==length(d2)){}else{stop("Input vectors do not have the same length")}
    
    
    def1=d1/n1
    def2=d2/n2
    pd_mean1=mean(def1)
    pd_mean2=mean(def2)
    CI=0
    estimate=function(def1,def2,CI){
      
      
      s=qnorm(pd_mean1)
      t=qnorm(pd_mean2)
      Term1<- 1/(2*pi*sqrt(1-rho^2))
      Term2<- exp(-((0.5*s^2-rho*s*t+0.5*t^2)/(1-rho^2)))
      ABL1<- Term1*Term2
      
      Term3_N<-s*t+ rho*(1-s^2-t^2) + s*t*rho^2 -rho^3 
      Term3_D<- 2*pi*(1-rho^2)^(5/2)
      ABL2<- (Term3_N/Term3_D)*Term2
      
      TS=def1*def2
      T<-length(def1)
      if(l>0){
        tryCatch(AC<-(acf(TS, plot = FALSE, type = "covariance")$acf)[(1:l),1,1], error = function(e) 0)
        Sum=NULL
        for (z in 1:l){
          
          Sum[z]<-(1-z/T)*AC[z]
        }
        AB=sum(Sum)} else{AB=0}
      var2=var(TS)
      Res<- rho +(ABL2/(T*ABL1^3))*(var2/2 + AB)
      
      if(CI==0){Res2=Res
      Est<-list(Original =Res2)
      }else{Est<-list(Original =Res, CI=c(Res-(qt(1-(1-CI)/2,T-1)*abs(ABL1))/sqrt(T)*sqrt(var2+2*(AB)),Res+(qt(1-(1-CI)/2,T-1)*abs(ABL1))/sqrt(T)*sqrt(var2+2*(AB))))
      }
      
  
      
    }
    Estimate_Standard<- estimate(def1,def2,CI)
    
    
    DEF<-rbind(def1,def2)
    
    if(DB[1]!=0){
      IN=DB[1]
      OUT=DB[2]
      
      theta1=NULL
      theta2=matrix(ncol = OUT, nrow=IN)
      for(i in 1:OUT){
        N<-length(def1)
        Ib<-sample(N,N,replace=TRUE)  ## sampling with replacement
        Db1<-def1[Ib] 
        Db2<-def2[Ib] 
        try(theta1[i]<-estimate(Db1,Db2,CI)$Original, silent = TRUE)
        
        for(c in 1:IN){
          Ic<-sample(N,N,replace=TRUE)  ## sampling with replacement
          Db3<-Db1[Ic] 
          Db4<-Db2[Ic] 
          try( theta2[c,i]<-estimate(Db3,Db4,CI)$Original, silent = TRUE)
          
        }
      }
      Boot1<- mean(theta1, na.rm = TRUE)
      Boot2<- mean(theta2, na.rm = TRUE)
      BC<- 2*Estimate_Standard$Original -Boot1
      DBC<- (3*Estimate_Standard$Original-3*Boot1+Boot2)
      
      Estimate_DoubleBootstrap<-list(Original = Estimate_Standard$Original, Bootstrap=BC, Double_Bootstrap=DBC, oValues=theta1, iValues=theta2)
      
    }
    if(B>0){ 
      N<-length(def1)
      
      N<-length(def1)
      convert=function(d){
        G=length(d)
        y1=list()
        for (y in 1:G){
          
          y1[[y]]=as.matrix((c(d[y])))
        }
        return(y1)
      }
      d1<-convert(def1)
      d2<-convert(def2)
      DEF_JC<-cbind(d1,d2)
      
      estimate2=function(X,CI){  
        def1=NULL
        N=length(X)/2
        for(t in 1:N){
          
          def1[t]<-X[[t]]
          
        }
        N1=2*N
        def2=NULL
        for(p in N:N1){
          
          def2[p]<-X[[p]]
          
        }
        def2<-def2[-(1:(N))]
        
        pd_mean1=mean(def1)
        pd_mean2=mean(def2)
        s=qnorm(pd_mean1)
        t=qnorm(pd_mean2)
        Term1<- 1/(2*pi*sqrt(1-rho^2))
        Term2<- exp(-((0.5*s^2-rho*s*t+0.5*t^2)/(1-rho^2)))
        ABL1<- Term1*Term2
        
        Term3_N<-s*t+ rho*(1-s^2-t^2) + s*t*rho^2 -rho^3 
        Term3_D<- 2*pi*(1-rho^2)^(5/2)
        ABL2<- (Term3_N/Term3_D)*Term2
        
        TS=def1*def2
        T<-length(def1)
        if(l>0){
          tryCatch(AC<-(acf(TS, plot = FALSE, type = "covariance")$acf)[(1:l),1,1], error = function(e) 0)
          Sum=NULL
          for (z in 1:l){
            
            Sum[z]<-(1-z/T)*AC[z]
          }
          AB=sum(Sum)} else{AB=0}
        var2=var(TS)
        
        Res<- rho +(ABL2/(T*ABL1^3))*(var2/2 + AB)
        
        if(missing(CI)){Res2=Res}else{Res2=Res+(qt(1-(1-CI)/2,T-1)/abs(ABL1))/sqrt(T)*sqrt(var2+2*(AB))}
        
        return(Res2)}
      BCA=function(data, indices){
        
        d <- data[indices,]
        
        tryCatch(estimate2(d),error=function(e)NA)
        #try(estimate2(d))
      }
      
      boot1<- boot(data = DEF_JC, statistic = BCA, R=B)
      
      Estimate_Bootstrap<-list(Original = boot1$t0, Bootstrap=2*boot1$t0 - mean(boot1$t,na.rm = TRUE),bValues=boot1$t )
      if(missing(CI_Boot)){Estimate_Bootstrap=Estimate_Bootstrap}else{
        if(type=="norm"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type)$normal[2:3])}
        if(type=="basic"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type)$basic[4:5])}
        if(type=="perc"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))$percent[4:5]}
        if(type=="bca"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))$bca[4:5]}
        if(type=="all"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))}
        CI=CI_Boot
        estimate4=function(def1,def2,CI){
          
          
          s=qnorm(pd_mean1)
          t=qnorm(pd_mean2)
          Term1<- 1/(2*pi*sqrt(1-rho^2))
          Term2<- exp(-((0.5*s^2-rho*s*t+0.5*t^2)/(1-rho^2)))
          ABL1<- Term1*Term2
          
          Term3_N<-s*t+ rho*(1-s^2-t^2) + s*t*rho^2 -rho^3 
          Term3_D<- 2*pi*(1-rho^2)^(5/2)
          ABL2<- (Term3_N/Term3_D)*Term2
          
          TS=def1*def2
          T<-length(def1)
          if(l>0){
            tryCatch(AC<-(acf(TS, plot = FALSE, type = "covariance")$acf)[(1:l),1,1], error = function(e) 0)
            Sum=NULL
            for (z in 1:l){
              
              Sum[z]<-(1-z/T)*AC[z]
            }
            AB=sum(Sum)} else{AB=0}
          var2=var(TS)
          Res<- rho +(ABL2/(T*ABL1^3))*(var2/2 + AB)
          
          if(CI==0){Res2=Res
          Est<-list(Original =Res2)
          }else{Est<-list(Original =Res, CI=c(Res-(qt(1-(1-CI)/2,T-1)/abs(ABL1))/sqrt(T)*sqrt(var2+2*(AB)),Res+(qt(1-(1-CI)/2,T-1)/abs(ABL1))/sqrt(T)*sqrt(var2+2*(AB))))
          }
          
          
          
        }
        CI1<- estimate4(def1,def2,CI)$CI
        
        Estimate_Bootstrap<-list(Original = boot1$t0, Bootstrap=2*boot1$t0 - mean(boot1$t,na.rm = TRUE),CI=CI1,CI_Boot=Conf,bValues=boot1$t )
        
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
      N<-length(def1)
    
      
      Test=NULL
      for(v in 1:N){
        d1<-def1[-v]
        d2<-def2[-v]
        try(Test[v]<-estimate(d1,d2,CI)$Original)
        
      }
      
      Estimate_Jackknife<-list(Original = Estimate_Standard$Original, Jackknife=(N*Estimate_Standard$Original-(N-1)*mean(Test)))
      
      
    }
    
    if(B>0){return(Estimate_Bootstrap)}
    if(JC==TRUE){return(Estimate_Jackknife)}
    if(DB[1]!=0){return(Estimate_DoubleBootstrap)}
    if(B==0 && JC==FALSE && DB[1]==0){return(Estimate_Standard)}
    
    
    
  }