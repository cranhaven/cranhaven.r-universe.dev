intraMLE <-
  function(d,n,B=0,DB=c(0,0),JC=FALSE,CI=0,CI_Boot,type="bca", plot=FALSE){
    Res2=list()
    if(is.numeric(d)){d=d}else{stop("d is not numeric")}
    if(is.numeric(n)){n=n}else{stop("n is not numeric")}
    if(B==0&& plot==TRUE){stop("please select a number of bootstrap repititions for the plot")}
    if(B%%1==0){B=B}else{stop("B is not an integer")}
    if(DB[1]%%1==0 && DB[2]%%1==0 ){DB=DB}else{stop("At least one entry in DB is not an integer")}
    if(length(d)==length(n)){}else{stop("Input vectors do not have the same length")}
    
    
    
    estimate=function(d,n,CI){ 
      if(CI==0){nll=function(rho){
        integral=NULL
        simpson <- function(fun, a, b, n=700) {

          h <- (b-a)/n
          x <- seq(a, b, by=h)
          if (n == 2) {
            s <- fun(x[1]) + 4*fun(x[2]) +fun(x[3])
          } else {
            s <- fun(x[1]) + fun(x[n+1]) + 2*sum(fun(x[seq(2,n,by=2)])) + 4 *sum(fun(x[seq(3,n-1, by=2)]))
          }
          s <- s*h/3
          return(s)
        }
        ll=0
        d1=d/n
        PD1=mean(d1)
        for(i in 1:length(d)){
          d1i=d[i]
          n1i=n[i]
          
          integrand=function(x){
            
            condPD <- pnorm((qnorm(PD1) - sqrt(rho) * x) / sqrt(1 - rho));
            return (choose(n1i, d1i) * (condPD^d1i) * ((1 - condPD)^(n1i - d1i)) * dnorm(x));
          }
      
          integral[i]=simpson(integrand,-10,10,n=10000)
          if(is.na(integral[i])){integral[i]=1}
          ll=ll+log(integral[i])
        }
       
        return(-ll)
      }
      
      Est<-list(Original =optimise(nll, interval = c(0, 1), maximum = FALSE)$minimum)
      }else{
        
        nll=function(rho){
          integral=NULL
          simpson <- function(fun, a, b, n=700) {
            h <- (b-a)/n
            x <- seq(a, b, by=h)
            if (n == 2) {
              s <- fun(x[1]) + 4*fun(x[2]) +fun(x[3])
            } else {
              s <- fun(x[1]) + fun(x[n+1]) + 2*sum(fun(x[seq(2,n,by=2)])) + 4 *sum(fun(x[seq(3,n-1, by=2)]))
            }
            s <- s*h/3
            return(s)
          }
          ll=0
          d1=d/n
          PD1=mean(d1)
          for(i in 1:length(d)){
            d1i=d[i]
            n1i=n[i]
            
            integrand=function(x){
              
              condPD <- pnorm((qnorm(PD1) - sqrt(rho) * x) / sqrt(1 - rho));
              return (choose(n1i, d1i) * (condPD^d1i) * ((1 - condPD)^(n1i - d1i)) * dnorm(x));
            }
            integral[i]=simpson(integrand,-10,10,n=10000)
            if(is.na(integral[i])){integral[i]=1}
            ll=ll+log(integral[i])
          }
          return(-ll)
        }
      
        Res1<- optimise(nll, interval = c(0, 1), maximum = FALSE)$minimum
        hessian1<-hessian(nll,Res1)
        SD<- 1/sqrt(hessian1)
        CI<- 1-(1-CI)/2
        Est<-list(Original =Res1, CI=c(Res1-qnorm(CI)*SD,Res1+qnorm(CI)*SD))
        
      } 
      
}
    
    Estimate_Standard<- estimate(d,n,CI)
    ######
    if(DB[1]!=0){
      IN=DB[1]
      OUT=DB[2]
      theta1=NULL
      theta2=matrix(ncol = OUT, nrow=IN)
      for(i in 1:OUT){
        N<-length(d)
        Ib<-sample(N,N,replace=TRUE)  ## sampling with replacement
        d_o<-d[Ib] 
        n_o<-n[Ib]
        try(theta1[i]<-estimate(d_o,n_o,CI)$Original, silent = TRUE)
        
        for(c in 1:IN){
          Ic<-sample(N,N,replace=TRUE)  ## sampling with replacement
          d_i<-d_o[Ic] 
          n_i<-n_o[Ic] 
          try( theta2[c,i]<-estimate(d_i,n_i,CI)$Original, silent = TRUE)
          
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
      D<- matrix(ncol=1, nrow=N,d)
      
      BCA=function(data,n, indices){
        
        d <- data[indices,]
        n<-n[indices]
        tryCatch(estimate(d,n,CI)$Original,error=function(e)NA)
        
        
      }
      
      boot1<- boot(data = D, statistic = BCA, n=n, R=B)
      
      Estimate_Bootstrap<-list(Original = boot1$t0, Bootstrap=2*boot1$t0 - mean(boot1$t,na.rm = TRUE),bValues=boot1$t )
      if(missing(CI_Boot)){Estimate_Bootstrap=Estimate_Bootstrap}else{
        if(type=="norm"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type)$normal[2:3])}
        if(type=="basic"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type)$basic[4:5])}
        if(type=="perc"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))$percent[4:5]}
        if(type=="bca"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))$bca[4:5]}
        if(type=="all"){Conf=(boot.ci(boot1,conf=CI_Boot,type = type))}
        
        CI=CI_Boot
        
        nll=function(rho){
          integral=NULL
          simpson <- function(fun, a, b, n=700) {
            h <- (b-a)/n
            x <- seq(a, b, by=h)
            if (n == 2) {
              s <- fun(x[1]) + 4*fun(x[2]) +fun(x[3])
            } else {
              s <- fun(x[1]) + fun(x[n+1]) + 2*sum(fun(x[seq(2,n,by=2)])) + 4 *sum(fun(x[seq(3,n-1, by=2)]))
            }
            s <- s*h/3
            return(s)
          }
          ll=0
          d1=d/n
          PD1=mean(d1)
          for(i in 1:length(d)){
            d1i=d[i]
            n1i=n[i]
            
            integrand=function(x){
              
              condPD <- pnorm((qnorm(PD1) - sqrt(rho) * x) / sqrt(1 - rho));
              return (choose(n1i, d1i) * (condPD^d1i) * ((1 - condPD)^(n1i - d1i)) * dnorm(x));
            }
            integral[i]=simpson(integrand,-10,10,n=10000)
            if(is.na(integral[i])){integral[i]=1}
            ll=ll+log(integral[i])
          }
          return(-ll)
        }
        
        Res1<- optimise(nll, interval = c(0, 1), maximum = FALSE)$minimum
        hessian1<-hessian(nll,Res1)
        SD<- 1/sqrt(hessian1)
        CI<- 1-(1-CI)/2
       CI1=c(Res1-qnorm(CI)*SD,Res1+qnorm(CI)*SD)
        
        
        
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
      N=length(d)
      Test=NULL
      for(v in 1:N){
        d2<-d[-v]
        n2<-n[-v]
        try(Test[v]<-estimate(d2,n2,CI)$Original)
        
      }
      
      Estimate_Jackknife<-list(Original = Estimate_Standard$Original, Jackknife=(N*Estimate_Standard$Original-(N-1)*mean(Test)))
      
      
      
      
    } 
    if(B>0){return(Estimate_Bootstrap)}
    if(JC==TRUE){return(Estimate_Jackknife)}
    if(DB[1]!=0){return(Estimate_DoubleBootstrap)}
    if(B==0 && JC==FALSE && DB[1]==0){return(Estimate_Standard)}
    
    
  }
