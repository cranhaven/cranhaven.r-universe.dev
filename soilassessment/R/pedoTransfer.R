pedoTransfer=function(method="linear", df, ...){
  dv=as.character(sapply(substitute(list(...))[-1], deparse))
  fml <- as.formula(paste(dv[1],paste((dv[2:length(dv)]),collapse="+"),sep="~"))
  names(fml)<-sub(".*\\$", "",names(fml))
  df=data.frame(df)
  (na.cols=function(x){
    y <- sapply(x, function(xx)any(is.na(xx)))
    names(y[y])
  })
  if(any(is.na(df))){stop(paste("Remove NA in columns: ", paste(na.cols(df), collapse=", ")))}

  if(method=="linear"){
    hk.lm=lm(fml,data=df)
    W=(dv[1])
    df$B=df[,grepl(W, colnames(df))]
    df$dv1=fitted(hk.lm)
    hk=hk.lm
  }

  if(method=="randomforest"){
    hk.rf=randomForest(fml,data=df, importance = TRUE)
    W=(dv[1])
    df$B=df[,grepl(W, colnames(df))]
    df$dv1=hk.rf$predicted
    RF_R2=cor(df$dv1,df$B)^2
    hk=hk.rf
  }
  if(method=="svm"){
    hk.sv=svm(fml,data=df)
    W=(dv[1])
    df$B=df[,grepl(W, colnames(df))]
    df$dv1=fitted(hk.sv)
    SV_R2=cor(df$dv1,df$B)^2
    hk=hk.sv
  }
  if(method=="neuralnetwork"){
    hk.nn= nnet(fml,data=df,size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)
    W=(dv[1])
    df$B=df[,grepl(W, colnames(df))]
    df$dv1=fitted(hk.nn)
    NN_R2=cor(df$dv1,df$B)^2
    hk=hk.nn
  }
  return(hk)

}
