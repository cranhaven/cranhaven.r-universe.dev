LPregression <-
function(j,Tx,Ty,X,X.test,m,method,extraparms){
  reg.dat<-as.data.frame(cbind( Ty[,j],Tx))
  colnames(reg.dat)[1]<-'Tyj'
  
  
  newdat<-eLP.poly.predict(X,Tx,X.test,m[1])
  frmla<-'Tyj~.-1'
  
  if(method=='glmnet'){
    lp.fit<-cv.glmnet(Tx,Ty[,j],family="gaussian",intercept=FALSE)
    lp.pred<-predict(lp.fit,newx=as.matrix(newdat), s="lambda.1se",type='response')
  }else if(method=='lm'){
    if(ncol(Tx)>1){
      if(ncol(Tx)>50){big.flag=TRUE}else{big.flag=FALSE}
      fit1 <- leaps::regsubsets(Tyj~., data = reg.dat,intercept=FALSE,really.big=big.flag)
      
      id<-which.min(summary(fit1)$bic)
      coefi <- coef(fit1, id = id)
      
      frmla<-paste0('Tyj~',names(coefi)[1])
      if(length(coefi)>1){
        for(i in 2:length(coefi)){
          frmla<-paste0(frmla,'+',names(coefi)[i])
        }
      }
      frmla<-paste0(frmla,'-1')
    }
    lp.fit <- lm( as.formula(frmla),data=reg.dat)
    lp.pred<-predict(lp.fit,newdata=newdat,se.fit=TRUE)
  }else if(method=='knn'){
    if(is.null(extraparms$k)){
      extraparms$k=ceiling(sqrt(nrow(X)))
    }
    arglist<-c(list(formula=as.formula(frmla),data=reg.dat),extraparms)
    knnfit<-do.call(knnreg,arglist)
    lp.pred<-predict(knnfit,newdata=newdat)
  }
  
  out<-as.numeric(lp.pred)
  
  return(out)
}
