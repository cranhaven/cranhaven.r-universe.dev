interMLE <-
  function(d1,n1,d2,n2,rho1,rho2,B=0, DB=c(0,0),  JC=FALSE,CI=-1, plot=FALSE){
    
    Estimate_Bootstrap=NULL
    Estimate_Jackknife=NULL
    Estimate_Standard=NULL
    
    if(is.numeric(d1)){d1=d1}else{stop("d1 is not numeric")}
    if(is.numeric(n1)){n1=n1}else{stop("n1 is not numeric")}
    if(is.numeric(d2)){d2=d2}else{stop("d2 is not numeric")}
    if(is.numeric(n2)){n2=n2}else{stop("n2 is not numeric")}
    if(is.numeric(rho1)){rho1=rho1}else{stop("rho1 is not numeric")}
    if(is.numeric(rho2)){rho2=rho2}else{stop("rho1 is not numeric")}
    
    if(B%%1==0){B=B}else{stop("B is not an integer")}
    if(DB[1]%%1==0 && DB[2]%%1==0 ){DB=DB}else{stop("At least one entry in DB is not an integer")}
    if(length(d1)==length(n1) && length(d2)==length(n2) && length(d1)==length(d2)){}else{stop("Input vectors do not have the same length")}
    
    
    def1=rbind(d1,n1)
    def2=rbind(d2,n2)
   
    
    estimate=function(def1,def2,CI){
      
      d1<-def1[1,]
      n1<-def1[2,]
      d2<-def2[1,]
      n2<-def2[2,]
      integral=NULL
      nll=function(rho){
        ll=0
        PD1=mean(d1/n1)
        PD2=mean(d2/n2)
        integral=NULL
        for(i in 1:length(d1)){
          d1i=d1[i]
          n1i=n1[i]
          d2i=d2[i]
          n2i=n2[i]
          integrand=function(x){
            PDcond1=pnorm((qnorm(PD1)-sqrt(rho1)*x[,1])/sqrt(1-rho1))
            PDcond2=pnorm((qnorm(PD2)-sqrt(rho2)*x[,2])/sqrt(1-rho2))
            as.matrix(dbinom(d1i,n1i,PDcond1)*dbinom(d2i,n2i,PDcond2)*dmvnorm(x,sigma=matrix(c(1,rho,rho,1),2)))
          }
          myGrid <- createNIGrid(dim=2, type="GHe", level=45)
          integral[i]=quadrature(integrand, myGrid)
          if(is.na(integral[i])){integral[i]=1}
          ll=ll+log(integral[i])
        }
        # print(-ll)
        -ll
      }
      
      Res2=list()
      Res1<- optimise(nll, interval = c(-1, 1), maximum = FALSE)$minimum
      if(CI!=-1){hessian1<-hessian(nll,Res1)
      SD<- 1/sqrt(hessian1)
      CI<- 1-(1-CI)/2
      Est<-list(Original =Res1, CI=c(Res1-qnorm(CI)*SD,Res1+qnorm(CI)*SD))
      }else{Est<-list(Original =Res1)}
      
    
      
    }
    
    Estimate_Standard<-estimate(def1,def2,CI)
    E_S<-Estimate_Standard$Original
    DEF<-rbind(def1,def2)
    if(DB[1]!=0){
      IN=DB[1]
      OUT=DB[2]
      
      theta1=NULL
      theta2=matrix(ncol = OUT, nrow=IN)
      for(i in 1:OUT){
        N<-length(d1)
        Ib<-sample(N,N,replace=TRUE) 
        Db1<-def1[,Ib] 
        Db2<-def2[,Ib] 
        try(theta1[i]<-estimate(Db1,Db2,CI)$Original, silent = TRUE)
        
        for(c in 1:IN){
          Ic<-sample(N,N,replace=TRUE)  
          Db3<-Db1[,Ic] 
          Db4<-Db2[,Ic] 
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
      N<-length(d1)
      theta=NULL
     for(i in 1:B){
        Ib<-sample(N,N,replace=TRUE)  ## sampling with replacement
        Db<-DEF[,Ib] 
        DEF1<- Db[1:2,]
        DEF2<- Db[3:4,]
        theta[i]<-estimate(DEF1,DEF2,CI)$Original
      }
      Boot<- mean(theta, na.rm = TRUE)
      Estimate_Bootstrap<- 2*Estimate_Standard$Original - Boot
      Estimate_Bootstrap<-list(Original = E_S, Bootstrap=2*Estimate_Standard$Original - Boot,bValues=theta )
      
      if(plot==TRUE){
        Dens<-density(theta, na.rm = TRUE)
        XY<-cbind(Dens$x,Dens$y)
        label<-data.frame(rep("Bootstrap density",times=length(Dens$x)))
        Plot<-cbind(XY,label)
        colnames(Plot)<-c("Estimate","Density","Label")
        
        
        SD<-cbind(rep(E_S,times=length(Dens$x)), Dens$y,rep("Standard estimate",times=length(Dens$x)))
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
      N<-length(d1)
      
      def1=rbind(d1,n1)
      def2=rbind(d2,n2)
      
      N<-length(n1)
      
      Test=NULL
      for(v in 1:N){
        d1<-def1[,-v]
        d2<-def2[,-v]
        try(Test[v]<-estimate(d1,d2,CI)$Original)
        
      }
      
      Estimate_Jackknife<-list(Original = Estimate_Standard$Original, Jackknife=(N*Estimate_Standard$Original-(N-1)*mean(Test)))
      
    }
    
    if(B>0){return(Estimate_Bootstrap)}
    if(JC==TRUE){return(Estimate_Jackknife)}
    if(DB[1]!=0){return(Estimate_DoubleBootstrap)}
    if(B==0 && JC==FALSE && DB[1]==0){return(Estimate_Standard)}
    
  }
