
# FULL MATCHING
IPPW = function(Y, Z, X, min.controls = 0.0001,max.controls = 10000, caliper = TRUE, calipersd = 0.2, dim = FALSE, gamma = 0.1, alpha = 0.05){

  # Smahal function
  smahal=
    function(z,X){
      X<-as.matrix(X)
      n<-dim(X)[1]
      rownames(X)<-1:n
      k<-dim(X)[2]
      m<-sum(z)
      for (j in 1:k) X[,j]<-rank(X[,j])
      cv<-stats::cov(X)
      vuntied<-stats::var(1:n)
      rat<-sqrt(vuntied/diag(cv))
      cv<-diag(rat)%*%cv%*%diag(rat)
      out<-matrix(NA,m,n-m)
      Xc<-X[z==0,]
      Xt<-X[z==1,]
      rownames(out)<-rownames(X)[z==1]
      colnames(out)<-rownames(X)[z==0]
      icov<-MASS::ginv(cv)
      for (i in 1:m) out[i,]<-stats::mahalanobis(Xc,Xt[i,],icov,inverted=T)
      out
    }
  
  # Add caliper function
  addcaliper=function(dmat,z,logitp,calipersd=.2,penalty=1000){
    sd.logitp=stats::sd(logitp)
    adif=abs(outer(logitp[z==1],logitp[z==0],"-"))
    adif=(adif-(calipersd*sd.logitp))*(adif>(calipersd*sd.logitp))
    dmat=dmat+adif*penalty
    dmat
  }
  
  # Matching
  treated.index = which(Z == 1)
  propscore.model = stats::glm(Z ~ X, family = 'binomial',x=TRUE,y=TRUE)
  treated = propscore.model$y
  Xmat=propscore.model$x[,-1]
  distmat=smahal(treated,Xmat)
  logit.propscore=stats::predict(propscore.model)
  
  # adding caliper
  if(caliper == TRUE) {
    distmat=addcaliper(distmat,treated,logit.propscore,calipersd)
    subject.index=seq(1,length(treated),1)
    rownames(distmat)=subject.index[treated==1]
    colnames(distmat)=subject.index[treated==0]
    
    matchvec=optmatch::fullmatch(distmat,min.controls,max.controls)
  } else {
    subject.index=seq(1,length(treated),1)
    rownames(distmat)=subject.index[treated==1]
    colnames(distmat)=subject.index[treated==0]
    
    matchvec=optmatch::fullmatch(distmat,min.controls,max.controls)
  }
  
  treated.subject.index=vector("list",length(treated.index))
  matched.control.subject.index=vector("list",length(treated.index))
  matchedset.index=substr(matchvec,start=3,stop=10)
  matchedset.index.numeric=as.numeric(matchedset.index)
  subjects.match.order=as.numeric(names(matchvec))
  matchedset_index = length(unique(matchedset.index.numeric))
  
  # total number in each set
  l <- rep(0,length(treated.subject.index))
  for(i in 1:length(treated.subject.index)){
    matched.set.temp=which(matchedset.index.numeric==i)
    matched.set.temp.indices=subjects.match.order[matched.set.temp]
    l[i] <- length(matched.set.temp.indices)
  }
  
  # the order of matchvec
  for(i in 1:length(treated.index)){
    matched.set.temp=which(matchedset.index.numeric==i)
    matched.set.temp.indices=subjects.match.order[matched.set.temp]
    treated.temp.index=which(matched.set.temp.indices %in% treated.index)
    if(length(treated.temp.index) != 0){
      treated.subject.index[[i]]=matched.set.temp.indices[treated.temp.index]
      matched.control.subject.index[[i]]=matched.set.temp.indices[-treated.temp.index]
    }
  }
  
  # remove null
  if(sum(sapply(treated.subject.index, is.null)) != 0){
    treated.subject.index<- treated.subject.index[-which(sapply(treated.subject.index, is.null))]
    matched.control.subject.index<-matched.control.subject.index[-which(sapply(matched.control.subject.index, is.null))]
  }
  
  # Calculate standardized differences
  treatedmat = X[Z == 1,]
  control.b = X[Z == 0,]
  controlmean.b = apply(control.b,2,mean)
  treatmean = apply(treatedmat,2,mean)
  treatvar = apply(treatedmat,2,stats::var)
  controlvar = apply(control.b, 2, stats::var)
  stand.diff.before=(treatmean-controlmean.b)/sqrt((treatvar+controlvar)/2)
  # number of variables
  ncol_X = ncol(X)
  treatedmat.after=matrix(0,nrow=length(matched.control.subject.index),ncol=ncol_X)
  for (i in 1:length(matched.control.subject.index)) {
    if(length(treated.subject.index[[i]])>1){
      treatedmat.after[i,]=apply(X[treated.subject.index[[i]],],2,mean)
    } else {
      treatedmat.after[i,]=X[treated.subject.index[[i]],]
    }
  }
  controlmat.after=matrix(0,nrow=length(matched.control.subject.index),ncol=ncol_X)
  for (i in 1:length(matched.control.subject.index)) {
    if(length(matched.control.subject.index[[i]])>1){
      controlmat.after[i,]=apply(X[matched.control.subject.index[[i]],],2,mean)
    } else {
      controlmat.after[i,]=X[matched.control.subject.index[[i]],]
    }
  }
  controlmean.after=apply(controlmat.after,2,mean)
  treatedmean.after=apply(treatedmat.after,2,mean)
  stand.diff.after=(treatedmean.after-controlmean.after)/sqrt((treatvar+controlvar)/2)
  balance = cbind(stand.diff.before,stand.diff.after)
  
  # Use XGBoost to estimate propensity score
  length_all = length(Z)
  length_X = ncol(X)
  df = data.frame(Z,X)
  index_model1 = sample(length_all,length_all/2)
  df1 = df[index_model1,]
  df2 = df[-index_model1,]
  prob = rep(0,length_all)
  xgb.model1 = xgboost::xgboost(data = as.matrix(df1[2:length_X]), label = df1$Z, nrounds = 2, objective = "binary:logistic",verbose = 0)
  prob[-index_model1] = stats::predict(xgb.model1, as.matrix(df2[2:length_X]))
  xgb.model2 = xgboost::xgboost(data = as.matrix(df2[2:length_X]), label = df2$Z, nrounds = 2, objective = "binary:logistic",verbose = 0)
  prob[index_model1] = stats::predict(xgb.model2, as.matrix(df1[2:length_X]))
  
  if(dim == TRUE){
    p = rep(0,length(Y))
    for (i in 1:length(treated.subject.index)) {
      index = c(treated.subject.index[[i]],matched.control.subject.index[[i]])
      n = length(index)
      if(length(treated.subject.index[[i]]) == 1) {
        p[index] = 1/n
      } else if(length(matched.control.subject.index[[i]]) == 1 & length(treated.subject.index) > 1) {
        p[index] = 1-(1/n)
      }
    }
  } else if(dim == FALSE){
    p = conditional_p(treated.subject.index,matched.control.subject.index,prob,gamma)
  }
  
  # Create set
  set = NULL
  for (i in 1:length(treated.subject.index)) {
    set[[i]] = c(unlist(treated.subject.index[[i]]),unlist(matched.control.subject.index[[i]]))
  }
  
  # Estimation
  tae_weight = rep(0,length(set))
  for (i in 1:length(set)) {
    index = set[[i]]
    n = length(set[[i]])
    tae = sum(Z[index]*Y[index]/(n*p[index])-((1-Z[index])*Y[index])/(n*(1-p[index])))
    tae_weight[i] = n*tae
  }
  
  N = length(Y)
  tae_all = sum(tae_weight)/N
  
  # CI
  N = length(Y)
  B = length(set)
  n_vector = sapply(set,length)
  w_vector = B*n_vector/N
  W = diag(w_vector,B,B)
  Q = matrix(0,B,1)
  Q[,1] = 1
  H_Q = Q%*%MASS::ginv((t(Q)%*%Q))%*%t(Q)
  I = diag(1,B,B)
  
  y = rep(0,length(set))
  for(i in seq_along(1:B)){
    index = set[[i]]
    ni = length(index)
    tae = sum((Z[index]*Y[index])/(ni*p[index])-((1-Z[index])*Y[index])/(ni*(1-p[index])))
    y[i] = tae/sqrt(1-H_Q[i,i])
  }
  variance = (1/B^2)*t(y)%*%W%*%(I-H_Q)%*%W%*%y
  
  make_interval = function(x,y){
    paste0("[",x,",",y,"]")
  }
  thre = stats::qnorm(1-alpha/2)
  low = tae_all-thre*sqrt(variance[1,1])
  up = tae_all+thre*sqrt(variance[1,1])
  CI = make_interval(format(low,digits=3),format(up,digits=4))
  variance1 = variance[1,1]
  
  list_all = list(estimate=tae_all,var=variance1,CI=CI,balance=balance)
  
  return(list_all)
  
}






