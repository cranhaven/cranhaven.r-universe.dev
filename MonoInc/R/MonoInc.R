MonoInc <-
function(data,id.col,x.col,y.col,data.r=NULL,tol=0, direction='inc', w1=.5, min, max, impType1='nn', impType2='reg', sum=FALSE)           
{
  #This function will predict the missing value using a regression which will be used for the weighted regression imputation
  pred.lm <- function(x,y,miss)		#function to compute a predicted value not in the dataset - miss: x of missing y
  {
    fit <- lm(y~x)
    slope <- fit$coefficients[[2]]
    int <- fit$coefficients[[1]]
    
    return(int + slope*miss) 
  }
  ########### Last Observation carried forward Function #############
  LOCF.func <- function(data.keep, data.fix, xcol, ycol)	#data.fix should have NAs for variable that will be imputed
  {
    ln <- data.fix
    for(j in 1:nrow(data.fix))
    {
      data <- rbind(data.keep, data.fix[j,])
      data <- data[order(data[,xcol]),]
      m <- match(NA, data[,3])
      if(isTRUE(m>1)==T) (ln[j,3] <- data[m-1,3])
    }
    return(ln)
  }
  ############## Last & Next Imputation Function #################
  LN.func <- function(data.keep, data.fix, xcol, ycol)	
  {
    ln <- data.fix
    for(j in 1:nrow(data.fix))
    {
      data <- rbind(data.keep, data.fix[j,])
      data <- data[order(data[,xcol]),]
      m <- match(NA, data[,3])
      if(isTRUE(m>1)==T & isTRUE(m<nrow(data))==T) (ln[j,3] <- mean(c(data[m-1,3], data[m+1,3])))
    }
    return(ln)
  }
  ###################### Fractional Regression Function ################
  frac.reg <- function(x, y, miss)
  {
    fit <- lm(y~x)
    slope <- fit$coefficients[[2]]
    int <- fit$coefficients[[1]]
    
    return(int + slope*miss + sample(fit$residuals,1))
  }
  ############### Nearest Neighbor Function ###############
  NN.func <- function(data.keep, data.fix, xcol, ycol)			#returns matrix of all imputed values
  {
    n.y <- nrow(data.fix)
    y.mat <- data.fix
    for(j in 1:n.y)
    {
      nn <- which.min(abs(data.keep[,xcol] - data.fix[j,xcol]))		#identify placement of nearest x neighbor
      n<-data.keep[nn,xcol]
      m<- which(data.keep[,xcol]==n) 
      if(data.fix[j,xcol]>=n && length(m)>1){
        x<-max(m)
        replace<-x
      }else if(data.fix[j,xcol]<n && length(m)>1){
        x<-min(m)
        replace<-x
      }else{
        replace<-nn}
      NN.y <- data.keep[replace,ycol]						#identify y value of nearest neighbor   
      y.mat[j,ycol] <- NN.y 
    }
    return(y.mat)
  }
  
  #flag data that needs to be imputed
  flag<-mono.flag(data,id.col,x.col,y.col,min,max,data.r,tol, direction)
  flag<- subset(flag, !(is.na(flag[,2])))

  if(direction=="inc"){
  #data.keep
  mono1.keep<-foreach(i = 1:nrow(flag), .combine='rbind') %do%
  {
    if(flag[i,4]==FALSE && flag[i,5]==FALSE && isTRUE(is.na(flag[i,3]))==FALSE){
      return(flag[i,])}
  }
  mono1.keep <-unique(mono1.keep[1:nrow(mono1.keep),])
  mono1.keep <- subset(mono1.keep, !(is.na(mono1.keep[,2])))

  mono.fix<-foreach(i = 1:nrow(flag), .combine='rbind') %do%
  {
  if(flag[i,4]==TRUE||flag[i,5]==TRUE||isTRUE(is.na(flag[i,3]))==TRUE){
    return(flag[i,])}
  }
  mono.fix<-unique(mono.fix[1:nrow(mono.fix),])
  mono.fix<- subset(mono.fix, !(is.na(mono.fix[,2])))
  }else{
  #data.keep
  mono1.keep<-foreach(i = 1:nrow(flag), .combine='rbind') %do%
    {
      if(flag[i,4]==TRUE && flag[i,5]==FALSE && isTRUE(is.na(flag[i,3]))==FALSE){
        return(flag[i,])}
    }
  mono1.keep <-unique(mono1.keep[1:nrow(mono1.keep),])
  mono1.keep <- subset(mono1.keep, !(is.na(mono1.keep[,2])))
  

  mono.fix<-foreach(i = 1:nrow(flag), .combine='rbind') %do%
  {
    if(flag[i,4]==FALSE||flag[i,5]==TRUE||isTRUE(is.na(flag[i,3]))==TRUE){
      return(flag[i,])}
  }
  mono.fix<-unique(mono.fix[1:nrow(mono.fix),])
  mono.fix<- subset(mono.fix, !(is.na(mono.fix[,2])))
  }
  #data.fix
  mono.fix1<-mono.fix
  mono.fix1[,3]<-NA
  
  #impute data 
  if((sum==TRUE && is.null(impType1)==FALSE && is.null(impType2)==FALSE)||(sum==TRUE && is.null(impType1)==FALSE)){
  cat("Error: choose either sum or an imputation method")
  #summary 
  }else if(sum==TRUE){
    #impute data 
    U<-unique(mono.fix[,1]) 
    #NN
   a <-foreach(i = 1:length(U), .combine='rbind') %do%
      {
        x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
        x<-x[order(x[,2]),] 
        y<-subset(mono.fix, mono.fix[,1]==U[i]) 
        y<-y[order(y[,2]),] 
        z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
        z<-z[order(z[,2]),]
        if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
          a<-y
          names(a)<-names(x)
          a<-as.data.frame(rbind(x,a))
          a<-a[order(a[,2]),]
        }else{
          a <-NN.func(x,z, 2, 3)
          names(a)<-names(x)
          a<-as.data.frame(rbind(x,a))
          a<-a[order(a[,2]),]
        }
        return(a)
      }
      a<-as.data.frame(rbind(a,mono1.keep))
      a<-as.data.frame(unique(a[1:nrow(a),]))
      a<-a[order(a[,1]),]
    #REG
    b <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        b<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }else{
        b<-pred.lm(x[,2],x[,3],miss[,2])
        b<- cbind(miss[,1:2], b,miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }
      return(b)
    }
    b<-as.data.frame(rbind(b,mono1.keep))
    b<-as.data.frame(unique(b[1:nrow(b),]))
    b<-b[order(b[,1]),]
    #LOCF
    c <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        c<-y
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }else{
        c <-LOCF.func(x,z, 2, 3)
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }
      return(c)
    }
    c<-as.data.frame(rbind(c,mono1.keep))
    c<-as.data.frame(unique(c[1:nrow(c),]))
    c<-c[order(c[,1]),]
    
    #LN
    d <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i])
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) || (sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        d<-y
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }else{
        d <-LN.func(x, z, 2, 3)
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }
      return(d)
    }
    d<-as.data.frame(rbind(d,mono1.keep))
    d<-as.data.frame(unique(d[1:nrow(d),]))
    d<-d[order(d[,1]),]
    #FR
    e <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        e<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }else{
        e<-frac.reg(x[,2],x[,3],miss[,2])
        e<- cbind(miss[,1:2], e,miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }
      return(e)
    }
    e<-as.data.frame(rbind(e,mono1.keep))
    e<-as.data.frame(unique(e[1:nrow(e),]))
    e<-e[order(e[,1]),]
    
    mono.keep <-rbind(mono1.keep,mono.fix1) #all data with NAs for data that needs to be imputed
    idx<-match(row.names(mono.keep), row.names(a))
    idx<-na.omit(idx)
    sortnames <- colnames(mono.keep)[1:2]
    mono.keep <- mono.keep[do.call("order", mono.keep[sortnames]),]
    mono.keep<-mono.keep[idx,]
    idx<-row.names(mono.keep)
    sum<-cbind(a[idx,1:2],mono.keep[idx,3],a[idx,3],b[idx,3],c[idx,3],d[idx,3],e[idx,3:5])
    colnames(sum)<-c("ID", "X", "Y","Nearest.Neighbor", "Regression", "LOCF", "Last.Next","Fractional.Reg","Decreasing", "Outside.Range" )

    return(sum)
  }else if(sum==FALSE){
#ALL IMPUTATIONS 
  if(impType1 =="nn" && is.null(impType2)==TRUE){ #Nearest Neighbor Imputation
    U<- unique(mono.fix[,1]) 
    a <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        a<-y
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }else{
        a <-NN.func(x,z, 2, 3)
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }
      return(a)
    }
    a<-as.data.frame(rbind(a,mono1.keep))
    a<-as.data.frame(unique(a[1:nrow(a),]))
    a<-a[order(a[,1]),] 
    return(a)
  }else if(impType1 =="reg" && is.null(impType2)==TRUE){ #Regression Imputation
    U<- unique(mono.fix[,1])
    b <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        b<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }else{
        b<-pred.lm(x[,2],x[,3],miss[,2])
        b<- cbind(miss[,1:2], b,miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }
      return(b)
    }
    b<-as.data.frame(rbind(b,mono1.keep))
    b<-as.data.frame(unique(b[1:nrow(b),]))
    b<-b[order(b[,1]),]
    return(b)
  }else if(impType1 =="locf" && is.null(impType2)==TRUE){ #Last Obervation Carried Forward
    U<-unique(mono.fix[,1])
    c <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        c<-y
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }else{
        c <-LOCF.func(x,z, 2, 3)
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }
      return(c)
    }
    c<-as.data.frame(rbind(c,mono1.keep))
    c<-as.data.frame(unique(c[1:nrow(c),]))
    c<-c[order(c[,1]),]
    return(c)
  }else if(impType1 =="ln" && is.null(impType2)==TRUE){ #Last and Next Imputation
    U<-unique(mono.fix[,1])
    d <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i])
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) || (sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        d<-y
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }else{
        d <-LN.func(x, z, 2, 3)
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }
      return(d)
    }
    d<-as.data.frame(rbind(d,mono1.keep))
    d<-as.data.frame(unique(d[1:nrow(d),]))
    d<-d[order(d[,1]),]
    return(d)
  }else if(impType1 =="fr" && is.null(impType2)==TRUE){ #Fractional Regression Imputation
    U<-unique(mono.fix[,1])
    e <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        e<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }else{
        e<-frac.reg(x[,2],x[,3],miss[,2])
        e<- cbind(miss[,1:2], e,miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }
      return(e)
    }
    e<-as.data.frame(rbind(e,mono1.keep))
    e<-as.data.frame(unique(e[1:nrow(e),]))
    e<-e[order(e[,1]),]
    return(e)
    ###WEIGHTED IMPUTATIONS##
  }else if((impType1 =="nn" && impType2=="reg") || (impType1 =="reg" && impType2=="nn")){ 
    U<- unique(mono.fix[,1]) 
    a <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        a<-y
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }else{
        a <-NN.func(x,z, 2, 3)
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }
      return(a)
    }
    a<-as.data.frame(rbind(a,mono1.keep))
    a<-as.data.frame(unique(a[1:nrow(a),]))
    a<-a[order(a[,1]),] 
    #REG
    b <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        b <- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }else{
        b<-pred.lm(x[,2],x[,3],miss[,2])
        b<- cbind(miss[,1:2], b,miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }
      return(b)
    }
    b<-as.data.frame(rbind(b,mono1.keep))
    b<-as.data.frame(unique(b[1:nrow(b),]))
    b<-b[order(b[,1]),]
    
    common<- b[,1] %in% a[,1]
    if(impType1=='nn'){
      y.star<-a[common,3]
      y.hat<-b[common,3]
    }else{
      y.star<-b[common,3]
      y.hat<-a[common,3]
    }
    Nn.Reg<-as.matrix(w1*y.star +(1-w1)*y.hat)
    h<- cbind(a[common,1:2], Nn.Reg ,a[common,4:5])
    return(h)
  }else if((impType1 =="nn" && impType2=="locf") || (impType1 =="locf" && impType2=="nn")){ 
   #NN
    U<- unique(mono.fix[,1]) 
    a <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        a<-y
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }else{
        a <-NN.func(x,z, 2, 3)
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }
      return(a)
    }
    a<-as.data.frame(rbind(a,mono1.keep))
    a<-as.data.frame(unique(a[1:nrow(a),]))
    a<-a[order(a[,1]),] 
    #LOCF
    U<-unique(mono.fix[,1])
    c <-foreach(i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        c<-y
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }else{
        c <-LOCF.func(x,z, 2, 3)
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }
      return(c)
    }
    c<-as.data.frame(rbind(c,mono1.keep))
    c<-as.data.frame(unique(c[1:nrow(c),]))
    c<-c[order(c[,1]),]
    
    common<- c[,1] %in% a[,1]
    if(impType1=='nn'){
      y.star<-a[common,3]
      y.hat<-c[common,3]
    }else{
      y.star<-c[common,3]
      y.hat<-a[common,3]
    }
    Nn.LOCF<-w1*y.star +(1-w1)*y.hat
    h<- cbind(c[common,1:2], Nn.LOCF,c[common,4:5])
    return(h)
  }else if((impType1 =="nn" && impType2=="ln") ||(impType1 =="ln" && impType2=="nn")){
    #NN
    U<- unique(mono.fix[,1]) 
    a <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        a<-y
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }else{
        a <-NN.func(x,z, 2, 3)
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }
      return(a)
    }
    a<-as.data.frame(rbind(a,mono1.keep))
    a<-as.data.frame(unique(a[1:nrow(a),]))
    a<-a[order(a[,1]),] 
    #LN
    U<-unique(mono.fix[,1])
    d <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i])
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) || (sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        d<-y
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }else{
        d <-LN.func(x, z, 2, 3)
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }
      return(d)
    }
    d<-as.data.frame(rbind(d,mono1.keep))
    d<-as.data.frame(unique(d[1:nrow(d),]))
    d<-d[order(d[,1]),]
    
    common<- d[,1] %in% a[,1]
    if(impType1=='ln'){
      y.star<-d[common,3]
      y.hat<-a[common,3]
    }else{
      y.star<-a[common,3]
      y.hat<-d[common,3]
    }
    Nn.Ln<-w1*y.star +(1-w1)*y.hat
    h<- cbind(d[common,1:2], Nn.Ln,d[common,4:5])
    return(h)
  }else if((impType1 =="nn" && impType2=="fr") ||(impType1 =="fr" && impType2=="nn")){
    #NN
    U<- unique(mono.fix[,1]) 
    a <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        a<-y
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }else{
        a <-NN.func(x,z, 2, 3)
        names(a)<-names(x)
        a<-as.data.frame(rbind(x,a))
        a<-a[order(a[,2]),]
      }
      return(a)
    }
    a<-as.data.frame(rbind(a,mono1.keep))
    a<-as.data.frame(unique(a[1:nrow(a),]))
    a<-a[order(a[,1]),] 
    #FR
    U<-unique(mono.fix[,1])
    e <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        e<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }else{
        e<-frac.reg(x[,2],x[,3],miss[,2])
        e<- cbind(miss[,1:2], e,miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }
      return(e)
    }
    e<-as.data.frame(rbind(e,mono1.keep))
    e<-as.data.frame(unique(e[1:nrow(e),]))
    e<-e[order(e[,1]),]
    
    common<- e[,1] %in% a[,1]
    if(impType1=='nn'){
      y.star<-a[common,3]
      y.hat<-e[common,3]
    }else{
      y.star<-e[common,3]
      y.hat<-a[common,3]
    }
    Nn.Fr<-w1*y.star +(1-w1)*y.hat
    h<- cbind(e[common,1:2], Nn.Fr,e[common,4:5])
    return(h)
  }else if((impType1 =="reg" && impType2=="locf") ||(impType1 =="locf" && impType2=="reg")){
    #REG
    U<- unique(mono.fix[,1]) 
    b <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        b<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }else{
        b<-pred.lm(x[,2],x[,3],miss[,2])
        b<- cbind(miss[,1:2], b,miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }
      return(b)
    }
    b<-as.data.frame(rbind(b,mono1.keep))
    b<-as.data.frame(unique(b[1:nrow(b),]))
    b<-b[order(b[,1]),]
    #LOCF
    U<-unique(mono.fix[,1])
    c <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        c<-y
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }else{
        c <-LOCF.func(x,z, 2, 3)
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }
      return(c)
    }
    c<-as.data.frame(rbind(c,mono1.keep))
    c<-as.data.frame(unique(c[1:nrow(c),]))
    c<-c[order(c[,1]),]
    
    common<- c[,1] %in% b[,1]
    if(impType1=='reg'){
      y.star<-b[common,3]
      y.hat<-c[common,3]
    }else{
      y.star<-c[common,3]
      y.hat<-b[common,3]
    }
    Reg.LOCF<-w1*y.star +(1-w1)*y.hat
    h<- cbind(c[common,1:2], Reg.LOCF,c[common,4:5])
    return(h)
  }else if((impType1 =="reg" && impType2=="fr") ||(impType1 =="fr" && impType2=="reg")){
    #REG
    U<- unique(mono.fix[,1]) 
    b <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        b<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }else{
        b<-pred.lm(x[,2],x[,3],miss[,2])
        b<- cbind(miss[,1:2], b,miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }
      return(b)
    }
    b<-as.data.frame(rbind(b,mono1.keep))
    b<-as.data.frame(unique(b[1:nrow(b),]))
    b<-b[order(b[,1]),]
    #FR 
    U<-unique(mono.fix[,1])
    e <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        e<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }else{
        e<-frac.reg(x[,2],x[,3],miss[,2])
        e<- cbind(miss[,1:2], e,miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }
      return(e)
    }
    e<-as.data.frame(rbind(e,mono1.keep))
    e<-as.data.frame(unique(e[1:nrow(e),]))
    e<-e[order(e[,1]),]
    
    common<- e[,1] %in% b[,1]
    if(impType1=='fr'){
      y.star<-e[common,3]
      y.hat<-b[common,3]
    }else{
      y.star<-b[common,3]
      y.hat<-e[common,3]
    }
    Reg.Fr<-w1*y.star +(1-w1)*y.hat
    h<- cbind(e[common,1:2], Reg.Fr,e[common,4:5])
    return(h)
  }else if((impType1 =="reg" && impType2=="ln") ||(impType1 =="ln" && impType2=="reg")){
    #REG
    U<- unique(mono.fix[,1]) 
    b <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        b<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }else{
        b<-pred.lm(x[,2],x[,3],miss[,2])
        b<- cbind(miss[,1:2], b,miss[,4:5])
        names(b)<-names(x)
        b<-as.data.frame(rbind(x,b))
        b<-b[order(b[,2]),]
      }
      return(b)
    }
    b<-as.data.frame(rbind(b,mono1.keep))
    b<-as.data.frame(unique(b[1:nrow(b),]))
    b<-b[order(b[,1]),]
    #LN 
    U<-unique(mono.fix[,1])
    d <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i])
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) || (sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        d<-y
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }else{
        d <-LN.func(x, z, 2, 3)
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }
      return(d)
    }
    d<-as.data.frame(rbind(d,mono1.keep))
    d<-as.data.frame(unique(d[1:nrow(d),]))
    d<-d[order(d[,1]),]
  
    common<- d[,1] %in% b[,1]
    if(impType1=='reg'){
      y.star<-b[common,3]
      y.hat<-d[common,3]
    }else{
      y.star<-d[common,3]
      y.hat<-b[common,3]
    }
    Reg.Ln<-w1*y.star +(1-w1)*y.hat
    h<- cbind(d[common,1:2], Reg.Ln,d[common,4:5])
    return(h)
  }else if((impType1 =="ln" && impType2=="locf") ||(impType1 =="locf" && impType2=="ln")){
    #LOCF
    U<-unique(mono.fix[,1])
    c <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        c<-y
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }else{
        c <-LOCF.func(x,z, 2, 3)
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }
      return(c)
    }
    c<-as.data.frame(rbind(c,mono1.keep))
    c<-as.data.frame(unique(c[1:nrow(c),]))
    c<-c[order(c[,1]),]
    #LN
    U<-unique(mono.fix[,1])
    d <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i])
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) || (sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        d<-y
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }else{
        d <-LN.func(x, z, 2, 3)
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }
      return(d)
    }
    d<-as.data.frame(rbind(d,mono1.keep))
    d<-as.data.frame(unique(d[1:nrow(d),]))
    d<-d[order(d[,1]),]
    
    common<- d[,1] %in% c[,1]
    if(impType1=='ln'){
      y.star<-d[common,3]
      y.hat<-c[common,3]
    }else{
      y.star<-c[common,3]
      y.hat<-d[common,3]
    }
    Ln.LOCF<-w1*y.star +(1-w1)*y.hat
    h<- cbind(d[common,1:2], Ln.LOCF,d[common,4:5])
    return(h)
  }else if((impType1 =="fr" && impType2=="locf") ||(impType1 =="locf" && impType2=="fr")){
    #LOCF
    U<-unique(mono.fix[,1])
    c <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i]) 
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) ||(length(x)==3)){
        c<-y
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }else{
        c <-LOCF.func(x,z, 2, 3)
        names(c)<-names(x)
        c<-as.data.frame(rbind(x,c))
        c<-c[order(c[,2]),]
      }
      return(c)
    }
    c<-as.data.frame(rbind(c,mono1.keep))
    c<-as.data.frame(unique(c[1:nrow(c),]))
    c<-c[order(c[,1]),]
    #FR
    U<-unique(mono.fix[,1])
    e <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        e<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }else{
        e<-frac.reg(x[,2],x[,3],miss[,2])
        e<- cbind(miss[,1:2], e,miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }
      return(e)
    }
    e<-as.data.frame(rbind(e,mono1.keep))
    e<-as.data.frame(unique(e[1:nrow(e),]))
    e<-e[order(e[,1]),]
    
    common<- e[,1] %in% c[,1]
    if(impType1=='locf'){
      y.star<-c[common,3]
      y.hat<-e[common,3]
    }else{
      y.star<-e[common,3]
      y.hat<-c[common,3]
    }
    Fr.LOCF<-w1*y.star +(1-w1)*y.hat
    h<- cbind(e[common,1:2], Fr.LOCF,e[common,4:5])
    return(h)
  }else if((impType1 =="ln" && impType2=="fr") ||(impType1 =="fr" && impType2=="ln")){
    #LN
    U<-unique(mono.fix[,1])
    d <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- subset(mono1.keep, mono1.keep[,1] ==U[i]) 
      x<-x[order(x[,2]),] 
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      z<-subset(mono.fix1, mono.fix1[,1]==U[i])
      z<-z[order(z[,2]),]
      if((sum(!is.na(x[,3])) == 0) || (sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        d<-y
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }else{
        d <-LN.func(x, z, 2, 3)
        names(d)<-names(x)
        d<-as.data.frame(rbind(x,d))
        d<-d[order(d[,2]),]
      }
      return(d)
    }
    d<-as.data.frame(rbind(d,mono1.keep))
    d<-as.data.frame(unique(d[1:nrow(d),]))
    d<-d[order(d[,1]),]
    #FR
    U<-unique(mono.fix[,1])
    e <-foreach( i = 1:length(U), .combine='rbind') %do%
    {
      x<- as.data.frame(subset(mono1.keep, mono1.keep[,1] ==U[i])) #subset that doesnt need imputation
      miss<-as.data.frame(subset(mono.fix1, mono.fix1[,1]==U[i])) #subset that needs imputation
      y<-subset(mono.fix, mono.fix[,1]==U[i]) 
      y<-y[order(y[,2]),] 
      if((sum(!is.na(x[,3])) == 0) ||(sum(is.na(x[,3]))==1 && nrow(x)==2 )||(length(x)==3)){
        e<- cbind(miss[,1:2], y[,3],miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }else{
        e<-frac.reg(x[,2],x[,3],miss[,2])
        e<- cbind(miss[,1:2], e,miss[,4:5])
        names(e)<-names(x)
        e<-as.data.frame(rbind(x,e))
        e<-e[order(e[,2]),]
      }
      return(e)
    }
    e<-as.data.frame(rbind(e,mono1.keep))
    e<-as.data.frame(unique(e[1:nrow(e),]))
    e<-e[order(e[,1]),]
    
    common<- d[,1] %in% e[,1]
    if(impType1=='ln'){
      y.star<-d[common,3]
      y.hat<-e[common,3]
    }else{
      y.star<-e[common,3]
      y.hat<-d[common,3]
    }
    Ln.Fr<-w1*y.star +(1-w1)*y.hat
    h<- cbind(d[common,1:2], Ln.Fr,d[common,4:5])
    i <- NULL; rm(i)
    return(h)}
    }
}
