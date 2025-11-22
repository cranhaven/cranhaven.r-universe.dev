interCopula <-
function(df1,df2,B=0, DB=c(0,0), JC=FALSE,CI,CI_Boot,type="bca", plot=FALSE){

  if(is.numeric(df1)){df1=df1}else{stop("df1 is not numeric")}
  if(is.numeric(df2)){df2=df2}else{stop("df2 is not numeric")}
  
  if(B==0&& plot==TRUE){stop("please select a number of bootstrap repititions for the plot")}
  if(B%%1==0){B=B}else{stop("B is not an integer")}
  if(DB[1]%%1==0 && DB[2]%%1==0 ){DB=DB}else{stop("At least one entry in DB is not an integer")}
  if(length(df1)==length(df2)){}else{stop("Input vectors do not have the same length")}

  
  estimate=function(df1,df2,CI){
    if(missing(CI)){  
    u1=pobs(as.matrix(df1))
    u2=pobs(as.matrix(df2))
    
    InterCor<-BiCopEst(u1,u2,family = 1)  

    Res2<-InterCor$par
    Est<-list(Original =Res2)}else{
      u1=pobs(as.matrix(df1))
      u2=pobs(as.matrix(df2))
      
      InterCor<-BiCopEst(u1,u2,family = 1,se=T) 

        
      a=sum(qnorm(u1)^2+qnorm(u2)^2)
      b=sum(qnorm(u1)*qnorm(u2))
      x=InterCor$par
      Hessian=(a*(3*x^2+1)-2*b*x^3-6*b*x+length(u1)*x^4-length(u1))/(x^2-1)^3
      SD=sqrt(1/(-Hessian))
      
      CI=1-(1-CI)/2
      Est<-list(Original =InterCor$par, CI=c(InterCor$par-qnorm(CI)*SD,InterCor$par+qnorm(CI)*SD))
      
    }
  
  }
  
  Estimate_Standard<-estimate(df1,df2,CI)
  DEF<-rbind(df1,df2)
  
  if(DB[1]!=0){
    IN=DB[1]
    OUT=DB[2]
    
    theta1=NULL
    theta2=matrix(ncol = OUT, nrow=IN)
    for(i in 1:OUT){
      N<-length(df1)
      Ib<-sample(N,N,replace=TRUE)  ## sampling with replacement
      Db1<-df1[Ib] 
      Db2<-df2[Ib] 
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
    N<-length(df1)
    convert=function(d){
      G=length(d)
      y1=list()
      for (y in 1:G){
        
        y1[[y]]=as.matrix((c(d[y])))
      }
      return(y1)
    }
    df1<-convert(df1)
    df2<-convert(df2)
    DEF_JC<-cbind(df1,df2)
    
    
    estimate2=function(X){
     
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
      
      u1=pobs(as.matrix(def1))
      u2=pobs(as.matrix(def2))
      
      InterCor<-BiCopEst(u1,u2,family = 1)  
      
      Res2<-InterCor$par
      return(Res2)
      
    }
    
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
      estimate_1=function(X,CI){
        
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
        Res2=list()
        
        u1=pobs(as.matrix(def1))
        u2=pobs(as.matrix(def2))
        
        InterCor<-BiCopEst(u1,u2,family = 1,se=TRUE) 
        Res2[[1]]<-InterCor$par
        CI=1-(1-CI)/2
        Res2[[2]]<-c(InterCor$par-qnorm(CI)*InterCor$se,InterCor$par+qnorm(CI)*InterCor$se)
        return(Res2)
        
      }
      
      
      CI1<-estimate_1(DEF_JC,CI)
      
      Estimate_Bootstrap<-list(Original = boot1$t0, Bootstrap=2*boot1$t0 - mean(boot1$t,na.rm = TRUE),CI=CI1[[2]],CI_Boot=Conf,bValues=boot1$t )
      
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
    N<-length(df1)

    
    Test=NULL
    for(v in 1:N){
      d1<-df1[-v]
      d2<-df2[-v]
      try(Test[v]<-estimate(d1,d2,CI)$Original)
      
    }
    
    Estimate_Jackknife<-list(Original = Estimate_Standard$Original, Jackknife=(N*Estimate_Standard$Original-(N-1)*mean(Test)))
    
    
    
  }
  
  if(B>0){return(Estimate_Bootstrap)}
  if(JC==TRUE){return(Estimate_Jackknife)}
  if(DB[1]!=0){return(Estimate_DoubleBootstrap)}
  if(B==0 && JC==FALSE && DB[1]==0){return(Estimate_Standard)}
  
  
  
}
