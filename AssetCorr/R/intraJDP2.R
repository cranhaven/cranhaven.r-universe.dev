intraJDP2 <-
function(d, n, B=0, DB=c(0,0), JC=FALSE, CI_Boot, type="bca", plot=FALSE){
  if(is.numeric(d)){d=d}else{stop("d is not numeric")}
  if(is.numeric(n)){n=n}else{stop("n is not numeric")}
  if(B==0&& plot==TRUE){stop("please select a number of bootstrap repititions for the plot")}
  if(B%%1==0){B=B}else{stop("B is not an integer")}
  if(DB[1]%%1==0 && DB[2]%%1==0 ){DB=DB}else{stop("At least one entry in DB is not an integer")}
  if(length(d)==length(n)){}else{stop("Input vectors do not have the same length")}
  
  estimate=function(d,n){
    estimateDefaultIntracorr <- function(pOneDefault, pTwoDefaults)# Equation 7 in Kalkbrenner
    {
      intracorr <- (pTwoDefaults - pOneDefault^2) / (pOneDefault - pOneDefault^2);
      return (intracorr);
    }
    estimateAssetCorr <- function(pBothDefaults, PD_1, PD_2)
    {
      #   if (PD_1 == 0)
      #   {
      #     PD_1 = 10^-9;
      #   }
      #   if (PD_2 == 0)
      #   {
      #     PD_1 = 10^-9;
      #   }
      #   if (is.nan(pBothDefaults))
      #   {
      #     pBothDefaults = 10^-9;
      #   }
      c1 <- qnorm(PD_1);
      c2 <- qnorm(PD_2);
      fEquation <- function(rho)
      {
        return (pmvnorm(lower = c(-Inf, -Inf), upper = c(c1, c2), sigma = matrix(c(1, rho, rho, 1), nrow = 2)) - pBothDefaults);
      }
      return (uniroot(fEquation,c(0,1))$root);
    }
    
    numPeriods <- length(n)
    probOneDefault<- mean(d/n)
    tempVec <-0
    for (t in 1:numPeriods)
    {
      tempVec<- (tempVec + (d[t]^2 ) / (n[t]^2 ));
    }
    probTwoDefaults <- tempVec / numPeriods
    defaultCorr <- estimateDefaultIntracorr(probOneDefault, probTwoDefaults);
    Est<-list(Original =estimateAssetCorr(probTwoDefaults, probOneDefault, probOneDefault))
  }
  Estimate_Standard<- estimate(d,n)
  if(B>0){
    
    
    N<-length(n)
    D<- matrix(ncol=1, nrow=N,d)
    
    BCA=function(data,n, indices){
      
      d <- data[indices,]
      n<-n[indices]
      tryCatch(estimate(d,n)$Original,error=function(e)NA)
      
    }
    
    boot1<- boot(data = D, statistic = BCA, n=n, R=B)
    
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
      N<-length(d)
      Ib<-sample(N,N,replace=TRUE)  ## sampling with replacement
      d_o<-d[Ib] 
      n_o<-n[Ib]
      try(theta1[i]<-estimate(d_o,n_o)$Original, silent = TRUE)
      
      for(c in 1:IN){
        Ic<-sample(N,N,replace=TRUE)  ## sampling with replacement
        d_i<-d_o[Ic] 
        n_i<-n_o[Ic] 
        try( theta2[c,i]<-estimate(d_i,n_i)$Original, silent = TRUE)
        
      }
    }
    Boot1<- mean(theta1, na.rm = TRUE)
    Boot2<- mean(theta2, na.rm = TRUE)
    BC<- 2*Estimate_Standard$Original -Boot1
    DBC<- (3*Estimate_Standard$Original-3*Boot1+Boot2)
    
    Estimate_DoubleBootstrap<-list(Original = Estimate_Standard$Original, Bootstrap=BC, Double_Bootstrap=DBC, oValues=theta1, iValues=theta2)
    
  }
  
  
  if(JC==TRUE){
    N=length(d)
    Test=NULL
    for(v in 1:N){
      d2<-d[-v]
      n2<-n[-v]
      try(Test[v]<-estimate(d2,n2)$Original)
      
    }
    
    Estimate_Jackknife<-list(Original = Estimate_Standard$Original, Jackknife=(N*Estimate_Standard$Original-(N-1)*mean(Test)))
    
  } 
  
  if(B>0){return(Estimate_Bootstrap)}
  if(JC==TRUE){return(Estimate_Jackknife)}
  if(DB[1]!=0){return(Estimate_DoubleBootstrap)}
  if(B==0 && JC==FALSE && DB[1]==0){return(Estimate_Standard)}
  
}
