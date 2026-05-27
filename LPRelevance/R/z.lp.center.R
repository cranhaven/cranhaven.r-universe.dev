z.lp.center <-
function(X,Tx,z,method.mean,X.test,m,extraparms){
  ##centering codes; Using LP, with selected method of smoothing
  if(method.mean=='glmnet'){
    fit.mean<-cv.glmnet(Tx,z,family="gaussian",intercept=TRUE)
    meanvals<-predict(fit.mean,newx=Tx, s="lambda.1se",type='response')
  }else if(method.mean=='lm'){
    centered.flag<-0
    Tx<-eLP.poly(X,m[1])
    reg.dat=as.data.frame(cbind(z,Tx))
    if(ncol(Tx)>50){big.flag=TRUE}else{big.flag=FALSE}
    fit1 <- leaps::regsubsets(z~., data = reg.dat,intercept=TRUE,really.big=big.flag)
    id<-which.min(summary(fit1)$bic)
    coefi <- coef(fit1, id = id)
    if(length(coefi)<2){
      centered.flag<-1
      meanvals<-rep(mean(z),length(z))
      z.mu<-mean(z)
    }else{
      frmla<-paste0('z~',names(coefi)[2])
      if(length(coefi)>2){
        for(i in 2:length(coefi)){
          frmla<-paste0(frmla,'+',names(coefi)[i])
        }
      }
      lm.fit <- lm(as.formula(frmla),data.frame(Tx))
      meanvals=predict(lm.fit,data.frame(Tx))
    }
  }else if(method.mean=='knn'){
    if(is.null(extraparms$k)){
      extraparms$k=ceiling(sqrt(nrow(X)))
    }
    arglist<-c(list(formula=as.formula('z~.'),data=as.data.frame(Tx)),extraparms)
    modelfit<-do.call(knnreg,arglist)
    meanvals<-predict(modelfit,data.frame(Tx))
  }
  y<-z-meanvals
  z.mu.test<-rep(0,nrow(X.test))
  for(i in 1:nrow(X.test)){
    Tx.test<-eLP.poly.predict(X,Tx,X.test[i,],mx=m[1])
    if(method.mean=='glmnet'){
      z.mu<-predict(fit.mean,newx=as.matrix(Tx.test), s="lambda.1se",type='response')
    }else if(method.mean=='lm'){
      if(centered.flag==0){z.mu<-predict(lm.fit,Tx.test)}
    }else if(method.mean %in% c("knn")){
      z.mu<-predict(modelfit,Tx.test)
    }
    z.mu.test[i]<-as.numeric(z.mu)
  }
  out<-list(y=y,zmean=meanvals,z.mu.test=z.mu.test)
  return(out)
}
